(in-package #:claw-lisp.providers.openrouter)

(defclass openrouter-provider (provider)
  ((credentials
    :initarg :credentials
    :accessor openrouter-provider-credentials
    :documentation "OpenRouter API credentials.")))

(defun make-openrouter-provider (config)
  "Create the OpenRouter provider object from runtime config."
  (declare (type runtime-config config))
  (let ((creds (claw-lisp.config:config-credentials config :openrouter)))
    (make-instance 'openrouter-provider
                   :name "openrouter"
                   :api-key (and creds
                                 (claw-lisp.config:provider-credentials-api-key creds))
                   :credentials creds)))

(defun provider-configured-p (provider)
  "Return T if PROVIDER has valid credentials."
  (let ((creds (openrouter-provider-credentials provider)))
    (claw-lisp.providers.auth:credentials-configured-p creds)))

(defun make-unconfigured-response ()
  "Return a transport-response indicating the provider is not configured."
  (claw-lisp.core.domain:make-transport-response
   :ok-p nil
   :status 0
   :assistant-text "OpenRouter provider registered, but credentials are not configured."
   :raw-response ""
   :provider "openrouter"))

(defmethod send-turn ((provider openrouter-provider) conversation &key model tools system)
  (declare (ignore system))
  (if (not (provider-configured-p provider))
      (make-unconfigured-response)
      (let* ((creds (openrouter-provider-credentials provider))
             (api-key (claw-lisp.config:provider-credentials-api-key creds))
             (base-url (or (claw-lisp.config:provider-credentials-base-url creds)
                           "https://openrouter.ai/api/v1/chat/completions"))
             (body-plist (conversation->chat-json conversation model :tools tools))
             (status nil)
             (body nil))
        (setf (values status body)
              (post-json base-url
                         (list (format nil "Authorization: Bearer ~A" api-key)
                               "HTTP-Referer: https://claw-lisp.local"
                               "X-Title: claw-lisp")
                         body-plist))
        (claw-lisp.core.domain:make-transport-response
         :ok-p (http-post-result-success-p status)
         :status status
         :assistant-text (extract-openrouter-response-text body)
         :raw-response body
         :error-message (unless (http-post-result-success-p status)
                          (extract-openrouter-response-text body))
         :provider "openrouter"
         :tool-calls (extract-openrouter-tool-calls body)))))

(defmethod stream-turn ((provider openrouter-provider) conversation
                        &key model tools on-event system)
  "Fallback streaming implementation for OpenRouter.

   The public runtime currently uses the streaming provider path. OpenRouter
   does not yet expose a native streaming implementation here, so fall back to
   SEND-TURN and emit a minimal callback sequence when assistant text is
   available."
  (declare (ignore system))
  (let ((response (send-turn provider conversation :model model :tools tools)))
    (when (and on-event
               (functionp on-event)
               (claw-lisp.core.domain:transport-response-ok-p response))
      (let ((assistant-text (claw-lisp.core.domain:transport-response-assistant-text response)))
        (handler-case
            (progn
              (funcall on-event "message_start"
                       '(:type "message_start" :message (:id "msg_openrouter_fallback"
                                                      :model "openrouter")))
              (when (and (stringp assistant-text)
                         (> (length assistant-text) 0))
                (funcall on-event "content_block_start"
                         '(:type "content_block_start" :content_block (:type "text")))
                (funcall on-event "content_block_delta"
                         (list :type "content_block_delta"
                               :delta (list :type "text_delta"
                                            :text assistant-text))))
              (funcall on-event "message_stop"
                       '(:type "message_stop")))
          (error (e)
            (format *error-output*
                    "Warning: openrouter on-event callback error: ~A~%"
                    e)))))
    response))

(defmethod normalize-response ((provider openrouter-provider) response)
  (declare (ignore provider))
  response)

(defmethod count-tokens ((provider openrouter-provider) messages &key model)
  (declare (ignore provider model))
  (reduce #'+
          messages
          :key (lambda (message)
                 (max 1
                      (ceiling
                       (length (claw-lisp.core.domain:message-content-text message))
                       4)))
          :initial-value 0))

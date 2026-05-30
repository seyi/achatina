(in-package #:claw-lisp.providers.openrouter)

(defclass openrouter-provider (provider)
  ((credentials
    :initarg :credentials
    :accessor openrouter-provider-credentials
    :documentation "OpenRouter API credentials.")))

(defun make-openrouter-provider (config)
  "Create the OpenRouter provider object from runtime config."
  (declare (type runtime-config config))
  (make-instance 'openrouter-provider
                 :name "openrouter"
                 :credentials (claw-lisp.config:config-credentials config :openrouter)))

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

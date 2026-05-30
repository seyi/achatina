(in-package #:claw-lisp.providers.bedrock)

;; --- AWS Bedrock Provider ---
;;
;; Calls AWS Bedrock using the Bedrock Messages API format.
;; Uses the bedrock-bridge.py Python script as a subprocess.
;;
;; Supported models:
;; - us.anthropic.claude-sonnet-4-6
;; - us.anthropic.claude-opus-4-6-v1
;; - us.anthropic.claude-haiku-4-5

(defclass bedrock-provider (provider)
  ((model-id
    :initarg :model-id
    :initform "us.anthropic.claude-sonnet-4-6"
    :accessor provider-model-id
    :documentation "Bedrock model ID (inference profile)")
   (region
    :initarg :region
    :initform "us-east-1"
    :accessor provider-region
    :documentation "AWS region")))

(defun make-bedrock-provider (&key (model-id "us.anthropic.claude-sonnet-4-6")
                                   (region "us-east-1"))
  "Create a Bedrock provider instance."
  (make-instance 'bedrock-provider :name "bedrock" :model-id model-id :region region))

(defun call-bedrock-bridge (body-plist model-id region)
  "Call the bedrock-bridge.py Python script and return (values status response-body).
   
   BODY-PLIST is the request body as a plist.
   MODEL-ID is the Bedrock model ID.
   REGION is the AWS region."
  (let* ((body-json (claw-lisp.providers.http-utils:json-encode-string body-plist))
         (script-path (merge-pathnames "bedrock-bridge.py"
                                       (asdf:system-source-directory :claw-lisp)))
         (args (list "python3" (namestring script-path)
                     body-json model-id region))
         (output nil)
         (status 0))
    (handler-case
        (let ((result (uiop:run-program args
                                        :output '(:string :stripped t)
                                        :error-output *error-output*
                                        :ignore-error-status t)))
          (setf output result
                status 0))
      (error (c)
        (format *error-output* "bedrock-bridge error: ~A~%" c)
        (setf status 1
              output (format nil "{\"error\": \"~A\"}" c))))
    (values status output)))

(defun bedrock-body->messages-plist (messages)
  "Convert Claw Lisp messages to Bedrock Messages API format.
   
   Returns a list of plists suitable for JSON encoding."
  (loop for msg in messages
        collect
        (let ((role (claw-lisp.core.domain:message-role msg))
              (content (claw-lisp.core.domain:message-content msg)))
          `(:role ,(string-downcase (symbol-name role))
            :content ,(if (listp content)
                          (claw-lisp.providers.http-utils:json-encode-string content)
                          content)))))

(defun extract-bedrock-response (response-json)
  "Extract text and tool calls from Bedrock response JSON.
   
   Returns a transport-response struct."
  (let* ((response (claw-lisp.providers.http-utils:json-decode response-json))
         (content (getf response :content ""))
         (tool-calls (getf response :tool_calls nil))
         (usage (getf response :usage nil))
         (stop-reason (getf response :stop_reason nil)))
    (claw-lisp.core.domain:make-transport-response
     :ok-p t
     :status 200
     :assistant-text content
     :raw-response response-json
     :provider "bedrock"
     :tool-calls tool-calls
     :metadata (list :usage usage :stop-reason stop-reason))))

(defmethod send-turn ((provider bedrock-provider) conversation &key model tools system)
  "Send a turn to Bedrock and return the response."
  (let* ((model-id (or model (provider-model-id provider)))
         (region (provider-region provider))
         (messages (bedrock-body->messages-plist
                    (claw-lisp.core.domain:conversation-messages conversation)))
         (body `(:anthropic_version "bedrock-2023-05-31"
                :max_tokens 4096
                :temperature 0.0
                :messages ,messages
                ,@(when system `(:system ,system))
                ,@(when tools `(:tools ,tools)))))
    (multiple-value-bind (status response-json)
        (call-bedrock-bridge body model-id region)
      (if (= status 0)
          (extract-bedrock-response response-json)
          (claw-lisp.core.domain:make-transport-response
           :ok-p nil
           :status status
           :assistant-text (format nil "Bedrock error: ~A" response-json)
           :raw-response response-json
           :provider "bedrock")))))

(defmethod stream-turn ((provider bedrock-provider) conversation &key model tools on-event system)
  "Stream a turn from Bedrock.
   
   Note: Streaming is not yet implemented for Bedrock provider.
   Falls back to send-turn."
  (declare (ignore on-event))
  ;; Streaming not yet implemented — use sync call
  (send-turn provider conversation :model model :tools tools :system system))

(defmethod normalize-response ((provider bedrock-provider) response)
  "Normalize Bedrock response."
  (declare (ignore provider))
  response)

(defmethod count-tokens ((provider bedrock-provider) messages &key model)
  "Estimate token count for messages."
  (declare (ignore provider model))
  (reduce #'+
          messages
          :key (lambda (msg)
                 (max 1
                      (ceiling (length (claw-lisp.core.domain:message-content-text msg))
                               4)))
          :initial-value 0))

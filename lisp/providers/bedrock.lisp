(in-package #:claw-lisp.providers.bedrock)

;; --- AWS Bedrock Provider ---
;;
;; Bedrock is intentionally de-scoped from the current public/runtime default
;; path until the bridge and model-ID contract are validated end to end.

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

(defun %bedrock-unavailable ()
  "Signal that Bedrock is intentionally unavailable in this build."
  (error "Bedrock support is not included in this build."))

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
  (declare (ignore provider conversation model tools system))
  (%bedrock-unavailable))

(defmethod stream-turn ((provider bedrock-provider) conversation &key model tools on-event system)
  "Stream a turn from Bedrock.
   
   Note: Streaming is not yet implemented for Bedrock provider.
   Falls back to send-turn."
  (declare (ignore provider conversation model tools on-event system))
  (%bedrock-unavailable))

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

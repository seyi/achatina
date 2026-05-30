(in-package #:claw-lisp.providers.mock)

(defclass mock-provider (provider) ())

(defparameter +mock-tool-loop-prefix+ "tool:echo:"
  "Prefix that triggers the baseline mock provider tool-call loop.")

(defun last-message-with-role (conversation role)
  "Return the most recent message in CONVERSATION with ROLE, or NIL."
  (find role
        (reverse (claw-lisp.core.domain:conversation-messages conversation))
        :key #'claw-lisp.core.domain:message-role
        :test #'eq))

(defun make-mock-provider ()
  "Create a local provider useful for early runtime testing."
  (make-instance 'mock-provider
                 :name "mock"))

(defmethod send-turn ((provider mock-provider) conversation &key model tools)
  (declare (ignore provider model tools))
  (let* ((latest-user (last-message-with-role conversation :user))
         (tool-results (claw-lisp.core.domain:conversation-tool-results conversation))
         (user-text (if latest-user
                        (claw-lisp.core.domain:message-content-text latest-user)
                        "")))
    (cond
      ((and (stringp user-text)
            (>= (length user-text) (length +mock-tool-loop-prefix+))
            (string= +mock-tool-loop-prefix+
                     user-text
                     :end2 (length +mock-tool-loop-prefix+))
            (null tool-results))
       (let ((tool-text (subseq user-text (length +mock-tool-loop-prefix+))))
         (claw-lisp.core.domain:make-transport-response
          :ok-p t
          :status 200
          :assistant-text ""
          :raw-response "{}"
          :provider "mock"
          :tool-calls
          (list (list :id "toolu_mock_01"
                      :name "echo"
                      :input (list :text tool-text))))))
      ((and tool-results
            latest-user)
       (let ((latest-tool-result (car (last tool-results))))
         (claw-lisp.core.domain:make-transport-response
          :ok-p t
          :status 200
          :assistant-text
          (format nil
                  "Mock provider used echo tool result: ~A"
                  (claw-lisp.core.domain:tool-result-content latest-tool-result))
          :raw-response "{}"
          :provider "mock"
          :tool-calls nil)))
      (t
       (claw-lisp.core.domain:make-transport-response
        :ok-p t
        :status 200
        :assistant-text "Mock provider reply. No external model call executed."
        :raw-response "{}"
        :provider "mock"
        :tool-calls nil)))))

(defmethod normalize-response ((provider mock-provider) response)
  (declare (ignore provider))
  response)

(defmethod count-tokens ((provider mock-provider) messages &key model)
  (declare (ignore provider model))
  (reduce #'+
          messages
          :key (lambda (message)
                 (max 1
                      (ceiling
                       (length (claw-lisp.core.domain:message-content-text message))
                       4)))
          :initial-value 0))

(defmethod stream-turn ((provider mock-provider) conversation &key model tools on-event system)
  "Mock streaming turn that fires on-event callbacks for testing.

   Simulates the same event sequence a real Anthropic stream would produce
   so that callback-handling code (including error isolation) can be exercised
   without hitting the network. Errors from the callback are caught and logged
   but do not abort the turn."
  (declare (ignore model system))
  (when (and on-event (functionp on-event))
    (handler-case
        (progn
          (funcall on-event "message_start"
                   '(:type "message_start" :message (:id "msg_mock_001" :model "mock")))
          (funcall on-event "content_block_start"
                   '(:type "content_block_start" :content_block (:type "text")))
          (funcall on-event "content_block_delta"
                   '(:type "content_block_delta" :delta (:type "text_delta" :text "Mock provider reply. ")))
          (funcall on-event "content_block_delta"
                   '(:type "content_block_delta" :delta (:type "text_delta" :text "No external model call executed.")))
          (funcall on-event "message_stop"
                   '(:type "message_stop")))
      (error (e)
        (format *error-output*
                "Warning: mock on-event callback error: ~A~%"
                e))))
  (send-turn provider conversation :model model :tools tools))

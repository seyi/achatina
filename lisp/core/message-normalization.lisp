(in-package #:claw-lisp.core.message-normalization)

;; --- Message Normalization ---
;; Normalizes conversation messages before sending to the API:
;;   1. Merge streaming assistant messages by message ID
;;   2. Strip thinking blocks for non-thinking models
;;   3. Ensure user/assistant alternation
;;   4. Repair orphaned tool-use/tool-result pairs

(defvar *validation-mode* nil
  "When T, enable extra validation logging and round-trip checks during normalization.")

(defvar *payload-capture-path* nil
  "Directory path where real Anthropic payloads are captured for regression tests.")

(defun capture-payload (payload type &key conversation-id turn-id (format :json))
  "Capture a request/response payload to disk for regression testing.
TURN-ID, when provided, is included in the filename for traceability."
  (when (and *payload-capture-path* (stringp *payload-capture-path*))
    (let* ((ts (get-universal-time))
           (filename (format nil "~A/capture-~A-~A~@[-~A~]-~A.~A"
                             *payload-capture-path*
                             (or conversation-id "conv")
                             type
                             turn-id
                             ts
                             (if (eq format :lisp) "lisp" "json")))
           (content (if (eq format :lisp)
                        (format nil "~S" payload)
                        (if (stringp payload)
                            payload
                            (with-output-to-string (sw)
                              (yason:encode payload sw))))))
      (ensure-directories-exist (directory-namestring filename))
      (with-open-file (out filename
                           :direction :output
                           :if-exists :supersede
                           :external-format :utf-8)
        (write-string content out))
      (when *validation-mode*
        (format *trace-output* "~&[NORMALIZATION] Captured ~A: ~A~%" type filename))
      filename)))

(defun copy-message (msg)
  "Create a deep copy of MSG."
  (let ((content (claw-lisp.core.domain:message-content msg))
        (metadata (claw-lisp.core.domain:message-metadata msg)))
    (claw-lisp.core.domain:make-message
     :role (claw-lisp.core.domain:message-role msg)
     :content (if (listp content)
                  (copy-list content)
                  content)
     :metadata (if metadata
                   (copy-list metadata)
                   nil))))

(defun merge-messages-by-id (messages)
  "Merge consecutive assistant messages with the same message ID."
  (let ((result nil)
        (current nil))
    (dolist (msg messages)
      (let ((role (claw-lisp.core.domain:message-role msg))
            (msg-id (getf (claw-lisp.core.domain:message-metadata msg) :message-id)))
        (if (and (eq role :assistant)
                 current
                 (eq (claw-lisp.core.domain:message-role current) :assistant)
                 msg-id
                 (string= msg-id
                          (getf (claw-lisp.core.domain:message-metadata current)
                                :message-id)))
            (let ((current-content (claw-lisp.core.domain:message-content current))
                  (new-content (claw-lisp.core.domain:message-content msg)))
              (setf current
                    (claw-lisp.core.domain:make-message
                     :role :assistant
                     :content (append (if (listp current-content)
                                          (copy-list current-content)
                                          (list current-content))
                                      (if (listp new-content)
                                          (copy-list new-content)
                                          (list new-content)))
                     :metadata (copy-list
                                (claw-lisp.core.domain:message-metadata current)))))
            (progn
              (when current
                (push current result))
              (setf current (copy-message msg))))))
    (when current
      (push current result))
    (nreverse result)))

(defun strip-thinking-blocks (messages)
  "Remove thinking blocks from assistant messages for non-thinking models."
  (loop for msg in messages
        collect
        (if (eq (claw-lisp.core.domain:message-role msg) :assistant)
            (let ((content (claw-lisp.core.domain:message-content msg)))
              (if (listp content)
                  (let ((filtered (remove-if #'claw-lisp.core.domain:thinking-block-p
                                             content)))
                    (if (= (length filtered) (length content))
                        msg
                        (claw-lisp.core.domain:make-message
                         :role :assistant
                         :content (copy-list filtered)
                         :metadata (copy-list
                                    (claw-lisp.core.domain:message-metadata msg)))))
                  msg))
            msg)))

(defun ensure-role-alternation (messages)
  "Ensure strict user/assistant alternation by merging consecutive same-role messages."
  (let ((result nil)
        (pending nil))
    (dolist (msg messages)
      (let ((role (claw-lisp.core.domain:message-role msg)))
        (if (null pending)
            (setf pending (list msg))
            (let ((pending-role (claw-lisp.core.domain:message-role (first pending))))
              (if (eq role pending-role)
                  (let ((merged-content
                          (append
                           (if (listp (claw-lisp.core.domain:message-content (first pending)))
                               (claw-lisp.core.domain:message-content (first pending))
                               (list (claw-lisp.core.domain:message-content (first pending))))
                           (if (listp (claw-lisp.core.domain:message-content msg))
                               (claw-lisp.core.domain:message-content msg)
                               (list (claw-lisp.core.domain:message-content msg))))))
                    (setf pending
                          (list (claw-lisp.core.domain:make-message
                                 :role role
                                 :content (copy-list merged-content)
                                 :metadata (copy-list
                                            (claw-lisp.core.domain:message-metadata msg))))))
                  (progn
                    (push (first pending) result)
                    (setf pending (list msg))))))))
    (when pending
      (push (first pending) result))
    (nreverse result)))

(defun repair-orphaned-tool-results (messages tool-results)
  "Create error tool-results for tool-use blocks with no matching result."
  (let ((tool-use-ids nil)
        (result-ids nil))
    ;; Collect all tool-use IDs from assistant messages
    (dolist (msg messages)
      (when (eq (claw-lisp.core.domain:message-role msg) :assistant)
        (dolist (block (claw-lisp.core.domain:message-content msg))
          (when (claw-lisp.core.domain:tool-use-block-p block)
            (push (claw-lisp.core.domain:tool-use-block-id block) tool-use-ids)))))
    ;; Collect all tool-result IDs
    (dolist (tr tool-results)
      (push (claw-lisp.core.domain:tool-result-call-id tr) result-ids))
    ;; Find orphaned tool-use IDs
    (let ((orphaned (set-difference tool-use-ids result-ids :test #'string=)))
      (if (null orphaned)
          messages
          ;; Append error message for orphaned tools
          (append
           messages
           (list (claw-lisp.core.domain:make-message
                  :role :user
                  :content (list
                            (claw-lisp.core.domain:make-tool-result-block
                             :tool-use-id (car orphaned)
                             :content "Tool execution failed: no result returned"
                             :is-error t)))))))))

(defun normalize-messages-for-api (messages model-capabilities tool-results)
  "Normalize messages for API submission.
Applies: merge, strip-thinking, ensure-alternation, repair."
  (let* ((thinking-p (claw-lisp.core.domain:model-capabilities-thinking-p
                      model-capabilities))
         (merged (merge-messages-by-id messages))
         (stripped (if thinking-p
                       merged
                       (strip-thinking-blocks merged)))
         (alternated (ensure-role-alternation stripped))
         (repaired (repair-orphaned-tool-results alternated tool-results)))
    (when *validation-mode*
      (format *trace-output*
              "~&[NORMALIZATION] ~A -> ~A messages~%"
              (length messages)
              (length repaired)))
    repaired))

(defun validate-normalization-roundtrip (messages model-capabilities tool-results)
  "Validate that normalization is idempotent.
Returns (values normalized passed-p report-string)."
  (let* ((normalized1 (normalize-messages-for-api messages model-capabilities tool-results))
         (normalized2 (normalize-messages-for-api normalized1 model-capabilities tool-results))
         (same-length (= (length normalized1) (length normalized2)))
         (same-content
           (and same-length
                (loop for m1 in normalized1
                      for m2 in normalized2
                      always (string=
                              (claw-lisp.core.domain:message-content-text m1)
                              (claw-lisp.core.domain:message-content-text m2)))))
         (passed (and same-length same-content)))
    (values normalized2
            passed
            (if passed
                "Normalization is idempotent"
                "WARNING: NOT idempotent"))))

(defun normalize-conversation-for-anthropic (conversation model-capabilities
                                             &optional extra-tool-results)
  "Normalize a conversation for Anthropic API submission.
Returns a fresh conversation structure.
EXTRA-TOOL-RESULTS, when provided, are appended to the conversation's own tool-results
for normalization (e.g. orphan repair), but the conversation's tool-results are preserved
in the output."
  (let* ((messages (claw-lisp.core.domain:conversation-messages conversation))
         (conv-tool-results (claw-lisp.core.domain:conversation-tool-results conversation))
         (tool-results (if extra-tool-results
                           (append conv-tool-results extra-tool-results)
                           conv-tool-results))
         (normalized (normalize-messages-for-api messages model-capabilities tool-results)))
    (claw-lisp.core.domain:make-conversation
     :id (claw-lisp.core.domain:conversation-id conversation)
     :messages normalized
     :tool-results conv-tool-results
     :metadata (copy-list (claw-lisp.core.domain:conversation-metadata conversation)))))

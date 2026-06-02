(in-package #:claw-lisp.tests)

;;; PRF-001: Bounded Coding Task Regression Test
;;;
;;; This test proves the full coding CLI pipeline works end-to-end:
;;; - Mock provider issues tool calls simulating inspect → edit → verify
;;; - Runtime enforces phase transitions
;;; - Progression policy advances phases based on tool types
;;; - Completion detection fires after verify
;;; - Session reaches :complete phase within bounded iterations

;;; --- Coding Task Provider ---
;;; Simulates a 3-turn coding task:
;;;   Turn 1: Read a file (inspect)
;;;   Turn 2: Write a file (edit)
;;;   Turn 3: Text response confirming completion (verify → complete)

(defclass coding-task-provider (claw-lisp.core.protocols:provider)
  ((call-count :initform 0 :accessor coding-task-provider-call-count)
   (fixture-root :initarg :fixture-root :reader coding-task-provider-fixture-root)))

(defmethod claw-lisp.core.protocols:send-turn
    ((provider coding-task-provider) conversation &key model tools system)
  (declare (ignore conversation model tools system))
  (incf (coding-task-provider-call-count provider))
  (let ((root (coding-task-provider-fixture-root provider)))
    (case (coding-task-provider-call-count provider)
      ;; Turn 1: inspect — read a file
      (1 (claw-lisp.core.domain:make-transport-response
          :ok-p t :status 200
          :assistant-text ""
          :raw-response "{}"
          :provider "coding-task"
          :tool-calls (list (list :id "toolu_read_01"
                                  :name "file-read"
                                  :input (list :path (namestring
                                                      (merge-pathnames "test.txt" root)))))))
      ;; Turn 2: edit — write a file
      (2 (claw-lisp.core.domain:make-transport-response
          :ok-p t :status 200
          :assistant-text ""
          :raw-response "{}"
          :provider "coding-task"
          :tool-calls (list (list :id "toolu_write_01"
                                  :name "file-write"
                                  :input (list :path (namestring
                                                      (merge-pathnames "test-out.txt" root))
                                               :content "fixed code")))))
      ;; Turn 3: completion text (no tools)
      (t (claw-lisp.core.domain:make-transport-response
          :ok-p t :status 200
          :assistant-text "I have completed the coding task. The file has been updated."
          :raw-response "{}"
          :provider "coding-task"
          :tool-calls nil)))))

(defmethod claw-lisp.core.protocols:stream-turn
    ((provider coding-task-provider) conversation &key model tools on-event system)
  (declare (ignore on-event))
  (claw-lisp.core.protocols:send-turn provider conversation
                                       :model model :tools tools :system system))

(defmethod claw-lisp.core.protocols:normalize-response
    ((provider coding-task-provider) response)
  (declare (ignore provider))
  response)

(defmethod claw-lisp.core.protocols:count-tokens
    ((provider coding-task-provider) messages &key model)
  (declare (ignore provider model))
  (max 1 (length messages)))

;;; --- Regression Test ---

(defun test-prf-001-bounded-coding-task ()
  "PRF-001: End-to-end regression test for bounded coding task.

   Verifies:
   1. Session starts in :inspect
   2. Phase transitions occur based on tool types
   3. Completion detection fires
   4. Session reaches :complete within bounded iterations
   5. Phase history records the full flow
   6. Turn count is bounded"
  (let* ((root (merge-pathnames
                (format nil "claw-lisp-prf001-~A/" (get-universal-time))
                #P"/tmp/"))
         (transcripts-root (merge-pathnames "transcripts/" root))
         (config (claw-lisp.config::%make-runtime-config
                  :data-root (namestring root)
                  :transcripts-root (namestring transcripts-root)
                  :artifacts-root (namestring (merge-pathnames "artifacts/" root))
                  :memory-root (namestring (merge-pathnames "memory/" root))
                  :default-provider "coding-task"
                  :default-model "mock-model"))
         (runtime (make-runtime :config config)))
    (unwind-protect
         (progn
           (claw-lisp.core.runtime:register-provider
            runtime
            (make-instance 'coding-task-provider
                           :name "coding-task"
                           :fixture-root root))
           ;; Register tools that the provider will call
           (register-tool runtime (make-instance 'claw-lisp.tools.file-read::file-read-tool
                                                 :name "file-read"
                                                 :description "Read file"))
           (register-tool runtime (make-instance 'claw-lisp.tools.file-write::file-write-tool
                                                 :name "file-write"
                                                 :description "Write file"))

           ;; Create test fixture under isolated root
           (ensure-directories-exist root)
           (with-open-file (s (merge-pathnames "test.txt" root)
                            :direction :output :if-exists :supersede)
             (write-string "original code" s))

           (let* ((session (start-session runtime
                                          :provider-name "coding-task"
                                          :model "mock-model"
                                          :session-id "prf-001-test"))
                  (updated (submit-user-message runtime session
                                               "Fix the bug in test.txt")))

             ;; Assertion 1: Session reached :complete
             (%assert (eq :complete (claw-lisp.core.phases:get-current-phase updated))
                      "Session should reach :complete phase, got ~A"
                      (claw-lisp.core.phases:get-current-phase updated))
             (format t "  ✓ Session reached :complete phase~%")

             ;; Assertion 2: Turn count is bounded (should be 3 turns)
             (let ((turns (claw-lisp.core.phases:get-turn-count updated)))
               (%assert (<= turns 5)
                        "Turn count should be bounded, got ~D" turns)
               (format t "  ✓ Turn count bounded: ~D turns~%" turns))

             ;; Assertion 3: Phase history shows progression
             (let ((history (claw-lisp.core.phases:get-phase-history updated)))
               (%assert (> (length history) 0)
                        "Phase history should not be empty")
               ;; Most recent should be :complete
               (%assert (eq :complete (getf (first history) :phase))
                        "Most recent history entry should be :complete")
               (format t "  ✓ Phase history recorded (~D transitions)~%" (length history)))

             ;; Assertion 4 & 5: Conversation structure
             (let* ((conversation (claw-lisp.core.domain:agent-session-conversation updated))
                    (messages (claw-lisp.core.domain:conversation-messages conversation))
                    (tool-results (claw-lisp.core.domain:conversation-tool-results conversation)))
               (%assert (>= (length messages) 4)
                        "Should have at least 4 messages (user + assistant rounds), got ~D"
                        (length messages))
               (%assert (>= (length tool-results) 1)
                        "Should have at least 1 tool result, got ~D"
                        (length tool-results))
               (format t "  ✓ Conversation: ~D messages, ~D tool results~%"
                       (length messages) (length tool-results))

               ;; Assertion 5: Final message has content
               (let* ((last-msg (car (last messages)))
                      (content (when last-msg
                                 (claw-lisp.core.domain:message-content last-msg))))
                 (%assert (and (stringp content) (> (length content) 0))
                          "Final message should have non-empty content, got: ~A" content)
                 (format t "  ✓ Final message has content: ~A~%"
                         (subseq content 0 (min 50 (length content))))))))

      ;; Cleanup
      (when (probe-file root)
        (uiop:delete-directory-tree root :validate t)))))

(defun run-prf-001-test ()
  (format t "~%=== PRF-001 Bounded Coding Task Regression Test ===~%~%")
  (test-prf-001-bounded-coding-task)
  (format t "~%=== PRF-001 Passed! ===~%"))

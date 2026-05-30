;;;; lisp/tests/durable-memory-scoring-test.lisp
;;;;
;;;; Smoke tests for Phase 6 Task 3: Save/Ignore Criteria & Scoring Engine.
;;;; Quick validation that scoring logic works correctly.

(in-package #:claw-lisp.tests)

;;; ============================================================
;;; Local Test Helper
;;; ============================================================

(defun %assert (condition format-control &rest format-args)
  "Assert that CONDITION is true. Signals an error with FORMAT-ARGS on failure."
  (unless condition
    (error (apply #'format nil format-control format-args))))

;;; ============================================================
;;; Scoring Smoke Tests
;;; ============================================================

(defun test-explicit-user-request-saves ()
  "Smoke test: Explicit user request should save regardless of threshold."
  (let* ((candidate (claw-lisp.storage.durable-memory:make-durable-memory-candidate
                     :kind :user
                     :subject-id "user-123"
                     :content "Remember, I prefer dark mode in the IDE."
                     :explicit-user-request-p t)))
    (multiple-value-bind (save-p reason score importance anti-score)
        (claw-lisp.storage.durable-memory:should-save-durable-memory-p candidate)
      (%assert save-p "Explicit request should save")
      (%assert (eq reason :explicit-request)
               "Reason should be :explicit-request, got ~A" reason)
      (%assert (>= importance 0.5)
               "Importance should be >= 0.5 for explicit request, got ~A" importance)
      (format t "✓ Explicit user request: SAVE (score=~A, importance=~A, anti=~A)~%"
              score importance anti-score))))

(defun test-stack-trace-rejected ()
  "Smoke test: Stack trace should be rejected (high anti-score)."
  (let* ((candidate (claw-lisp.storage.durable-memory:make-durable-memory-candidate
                     :kind :project
                     :subject-id "proj-456"
                     :content "Traceback (most recent call last):
  File \"app.py\", line 42, in main
    result = process(data)
  at process_data (module.py:15)
Exception: ValueError")))
    (multiple-value-bind (save-p reason score importance anti-score)
        (claw-lisp.storage.durable-memory:should-save-durable-memory-p candidate)
      (%assert (not save-p) "Stack trace should not save")
      (%assert (eq reason :anti-criteria)
               "Reason should be :anti-criteria, got ~A" reason)
      (%assert (>= anti-score 0.7)
               "Anti-score should be >= 0.7 for stack trace, got ~A" anti-score)
      (format t "✓ Stack trace: REJECT (score=~A, importance=~A, anti=~A)~%"
              score importance anti-score))))

(defun test-ephemeral-instruction-rejected ()
  "Smoke test: Ephemeral instruction ('now run') should be rejected."
  (let* ((candidate (claw-lisp.storage.durable-memory:make-durable-memory-candidate
                     :kind :project
                     :subject-id "proj-456"
                     :content "Now run this command to start the server: npm start")))
    (multiple-value-bind (save-p reason score importance anti-score)
        (claw-lisp.storage.durable-memory:should-save-durable-memory-p candidate)
      (%assert (not save-p) "Ephemeral instruction should not save")
      (%assert (eq reason :score-below-threshold)
               "Reason should be :score-below-threshold, got ~A" reason)
      (%assert (>= anti-score 0.2)
               "Anti-score should be >= 0.2 for ephemeral, got ~A" anti-score)
      (format t "✓ Ephemeral instruction: REJECT (score=~A, importance=~A, anti=~A)~%"
              score importance anti-score))))

(defun test-empty-content-rejected ()
  "Smoke test: Empty content should be rejected."
  (let* ((candidate (claw-lisp.storage.durable-memory:make-durable-memory-candidate
                     :kind :user
                     :subject-id "user-123"
                     :content "")))
    (multiple-value-bind (save-p reason score importance anti-score)
        (claw-lisp.storage.durable-memory:should-save-durable-memory-p candidate)
      (%assert (not save-p) "Empty content should not save")
      (%assert (eq reason :empty-content)
               "Reason should be :empty-content, got ~A" reason)
      (%assert (= score 0.0) "Score should be 0.0 for empty, got ~A" score)
      (format t "✓ Empty content: REJECT (score=~A, importance=~A, anti=~A)~%"
              score importance anti-score))))

(defun test-preference-statement-saves ()
  "Smoke test: User preference statement should save."
  (let* ((candidate (claw-lisp.storage.durable-memory:make-durable-memory-candidate
                     :kind :user
                     :subject-id "user-123"
                     :content "I always prefer to use tabs over spaces for indentation in Python code.")))
    (multiple-value-bind (save-p reason score importance anti-score)
        (claw-lisp.storage.durable-memory:should-save-durable-memory-p candidate)
      (%assert save-p "User preference should save")
      (%assert (eq reason :score-exceeded-threshold)
               "Reason should be :score-exceeded-threshold, got ~A" reason)
      (%assert (>= importance 0.25)
               "Importance should be >= 0.25 for preference, got ~A" importance)
      (%assert (< anti-score 0.3)
               "Anti-score should be < 0.3 for preference, got ~A" anti-score)
      (format t "✓ User preference: SAVE (score=~A, importance=~A, anti=~A)~%"
              score importance anti-score))))

(defun test-code-block-rejected ()
  "Smoke test: Raw code block should be rejected (high anti-score)."
  (let* ((candidate (claw-lisp.storage.durable-memory:make-durable-memory-candidate
                     :kind :reference
                     :subject-id "proj-456"
                     :content "```python
def hello():
    print('Hello, World!')
    return True
```")))
    (multiple-value-bind (save-p reason score importance anti-score)
        (claw-lisp.storage.durable-memory:should-save-durable-memory-p candidate)
      (%assert (not save-p) "Raw code block should not save")
      (%assert (>= anti-score 0.4)
               "Anti-score should be >= 0.4 for code block, got ~A" anti-score)
      (format t "✓ Code block: REJECT (score=~A, importance=~A, anti=~A)~%"
              score importance anti-score))))

(defun test-importance-scoring-length ()
  "Smoke test: Importance scoring rewards moderate length."
  (let* ((short (claw-lisp.storage.durable-memory:make-durable-memory-candidate
                 :kind :user :subject-id "user-123" :content "Hi"))
         (medium (claw-lisp.storage.durable-memory:make-durable-memory-candidate
                  :kind :user :subject-id "user-123"
                  :content "I prefer to use dark mode in the IDE because it reduces eye strain."))
         (long (claw-lisp.storage.durable-memory:make-durable-memory-candidate
                :kind :user :subject-id "user-123"
                :content "In our project architecture, we have decided to use a layered approach with clear separation of concerns between the presentation layer, business logic layer, and data access layer. This pattern has proven effective in maintaining code quality and facilitating future enhancements.")))
    (let ((short-score (claw-lisp.storage.durable-memory:compute-durable-memory-importance-score short))
          (medium-score (claw-lisp.storage.durable-memory:compute-durable-memory-importance-score medium))
          (long-score (claw-lisp.storage.durable-memory:compute-durable-memory-importance-score long)))
      (%assert (<= short-score medium-score)
               "Medium content should score >= short content")
      (%assert (<= medium-score long-score)
               "Long content should score >= medium content (up to 200 words)")
      (format t "✓ Length scoring: short=~A, medium=~A, long=~A~%"
              short-score medium-score long-score))))

;;; ============================================================
;;; Test Runner
;;; ============================================================

(defun run-durable-memory-scoring-tests ()
  "Run all durable memory scoring smoke tests. Returns T if all pass."
  (format t "~&=== Durable Memory Scoring Smoke Tests ===~%")
  (let ((results (list
                  (test-explicit-user-request-saves)
                  (test-stack-trace-rejected)
                  (test-ephemeral-instruction-rejected)
                  (test-empty-content-rejected)
                  (test-preference-statement-saves)
                  (test-code-block-rejected)
                  (test-importance-scoring-length))))
    (if (every #'identity results)
        (progn
          (format t "~&ALL SMOKE TESTS PASSED~%")
          t)
        (progn
          (format t "~&SOME SMOKE TESTS FAILED~%")
          nil))))

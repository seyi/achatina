;;;; lisp/tests/durable-memory-test.lisp
;;;;
;;;; Comprehensive tests for Phase 6 Durable Memory System.
;;;; Run with: (claw-lisp.tests:run-durable-memory-tests)

(in-package #:claw-lisp.tests)

;;; ============================================================
;;; Local Test Helper
;;; ============================================================

(defun %assert (condition format-control &rest format-args)
  "Assert that CONDITION is true. Signals an error with FORMAT-ARGS on failure."
  (unless condition
    (error (apply #'format nil format-control format-args))))

;;; ============================================================
;;; Serialization Round-Trip Tests
;;; ============================================================

(defun test-domain-model-serialization-roundtrip ()
  "Test durable-memory-record serialization/deserialization round-trip."
  (let* ((record (claw-lisp.storage.durable-memory:make-durable-memory-record
                  :id "uuid-1234"
                  :kind :user
                  :subject-id "user-123"
                  :title "Preferred Editor Theme"
                  :content "I prefer dark mode in all editors."
                  :source :conversation
                  :created-universal-time 389234234
                  :updated-universal-time 389234235
                  :importance-score 0.8
                  :staleness-score 0.1
                  :last-accessed-universal-time 389234236
                  :tags '(:preference :editor)
                  :version 1
                  :supersedes-id nil))
         (plist (claw-lisp.storage.durable-memory:durable-memory-record-to-plist record))
         (record2 (claw-lisp.storage.durable-memory:plist-to-durable-memory-record plist)))
    (%assert (equal (claw-lisp.storage.durable-memory:durable-memory-record-id record)
                    (claw-lisp.storage.durable-memory:durable-memory-record-id record2))
             "ID round-trip failed")
    (%assert (equal (claw-lisp.storage.durable-memory:durable-memory-record-content record)
                    (claw-lisp.storage.durable-memory:durable-memory-record-content record2))
             "Content round-trip failed")
    (%assert (equal (claw-lisp.storage.durable-memory:durable-memory-record-tags record)
                    (claw-lisp.storage.durable-memory:durable-memory-record-tags record2))
             "Tags round-trip failed")
    (%assert (equal (claw-lisp.storage.durable-memory:durable-memory-record-kind record)
                    (claw-lisp.storage.durable-memory:durable-memory-record-kind record2))
             "Kind round-trip failed")
    (format t "~%✓ Durable memory record serialization round-trip passed~%")
    t))

;;; ============================================================
;;; Scoring Edge Case Tests
;;; ============================================================

(defun test-scoring-explicit-request-and-anti-criteria ()
  "Test scoring with explicit request and anti-criteria."
  (let* ((candidate (claw-lisp.storage.durable-memory:make-durable-memory-candidate
                     :kind :user
                     :subject-id "user-123"
                     :content "Remember, I prefer dark mode."
                     :explicit-user-request-p t)))
    (multiple-value-bind (save-p reason score importance anti-score)
        (claw-lisp.storage.durable-memory:should-save-durable-memory-p candidate)
      (%assert save-p "Explicit request should save")
      (%assert (eq reason :explicit-request)
               "Reason should be :explicit-request, got ~A" reason)
      (%assert (>= importance 0.5)
               "Importance should be >= 0.5 for explicit request, got ~A" importance)
      (%assert (< anti-score 0.3)
               "Anti-score should be < 0.3 for explicit request, got ~A" anti-score)
      (format t "~%✓ Explicit request scoring passed~%")
      t)))

(defun test-scoring-raw-code-block ()
  "Test scoring for raw code block (should be rejected)."
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
      (%assert (eq reason :anti-criteria)
               "Reason should be :anti-criteria, got ~A" reason)
      (%assert (>= anti-score 0.4)
               "Anti-score should be >= 0.4 for code block, got ~A" anti-score)
      (format t "~%✓ Raw code block scoring passed~%")
      t)))

(defun test-scoring-stack-trace ()
  "Test scoring for stack trace (should be rejected)."
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
      (format t "~%✓ Stack trace scoring passed~%")
      t)))

;;; ============================================================
;;; Test Runner
;;; ============================================================

(defun run-durable-memory-tests ()
  "Run all durable memory tests. Returns T if all pass."
  (format t "~%&=== Durable Memory Comprehensive Tests ===~%")
  (let ((results (list
                  (test-domain-model-serialization-roundtrip)
                  (test-scoring-explicit-request-and-anti-criteria)
                  (test-scoring-raw-code-block)
                  (test-scoring-stack-trace))))
    (if (every #'identity results)
        (progn
          (format t "~%&ALL TESTS PASSED~%")
          t)
        (progn
          (format t "~%&SOME TESTS FAILED~%")
          nil))))

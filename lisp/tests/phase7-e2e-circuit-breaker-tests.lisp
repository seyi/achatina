;;;; lisp/tests/phase7-e2e-circuit-breaker-tests.lisp
;;;;
;;;; Phase 7 Task 7 Step 3 — Circuit Breaker Tests
;;;;
;;;; Tests for embedding failure handling and recovery.

(in-package #:claw-lisp.tests)

;;; ============================================================
;;; CB-01: Normal Operation (No Failures)
;;; ============================================================

(defun test-cb-01-normal-operation ()
  "Test that embedding calls succeed under normal conditions."
  (format t "~%&CB-01: Normal operation (no failures)~%")
  (handler-case
      (%with-clean-state
        (%with-circuit-breaker-config (:cooldown-seconds 1 :failure-threshold 3)
          (let* ((session (%create-test-session))
                 (user-msg "What are my preferences?"))
            ;; Simulate a turn - should succeed
            (multiple-value-bind (messages metadata)
                (%simulate-turn session user-msg)
              ;; Verify injection occurred
              (%assert-injected-memory-count metadata 1)
              ;; Verify circuit breaker is still closed
              (%assert (null claw-lisp.storage.durable-memory-search:*dmq-circuit-open-until*)
                       "Circuit breaker should remain closed")
              (format t "~%✓ CB-01 passed~%")
              t))))
    (error (e)
      (format t "~%✗ CB-01 FAILED: ~A~%" e)
      nil)))

;;; ============================================================
;;; CB-02: Circuit Opens After Threshold
;;; ============================================================

(defun test-cb-02-circuit-opens-after-threshold ()
  "Test that circuit opens after 3 consecutive failures."
  (format t "~%&CB-02: Circuit opens after threshold~%")
  (handler-case
      (%with-clean-state
        (%with-circuit-breaker-config (:cooldown-seconds 60 :failure-threshold 3)
          ;; Manually trigger failures
          (let ((failures 0))
            ;; Simulate 3 failures
            (dotimes (i 3)
              (handler-case
                  (claw-lisp.storage.durable-memory-search:record-embedding-failure)
                (warning () (incf failures))))
            ;; Verify circuit is now open
            (%assert (not (null claw-lisp.storage.durable-memory-search:*dmq-circuit-open-until*))
                     "Circuit breaker should be open after 3 failures")
            (%assert (= claw-lisp.storage.durable-memory-search:*dmq-embedding-failures* 3)
                     "Failure count should be 3")
            (format t "~%✓ CB-02 passed~%")
            t)))
    (error (e)
      (format t "~%✗ CB-02 FAILED: ~A~%" e)
      nil)))

;;; ============================================================
;;; CB-03: Cooldown Expiry
;;; ============================================================

(defun test-cb-03-cooldown-expiry ()
  "Test that circuit closes after cooldown period expires."
  (format t "~%&CB-03: Cooldown expiry~%")
  (handler-case
      (%with-clean-state
        (%with-circuit-breaker-config (:cooldown-seconds 1 :failure-threshold 3)
          ;; Trigger failures to open circuit
          (dotimes (i 3)
            (claw-lisp.storage.durable-memory-search:record-embedding-failure))
          ;; Verify circuit is open
          (%assert (not (null claw-lisp.storage.durable-memory-search:*dmq-circuit-open-until*))
                   "Circuit should be open")
          ;; Wait for cooldown (1 second)
          (sleep 1.1)
          ;; Query should succeed now (circuit should be closed)
          (let ((available (claw-lisp.storage.durable-memory-search:embedding-available-p)))
            (%assert available "Circuit should be closed after cooldown")
            (format t "~%✓ CB-03 passed~%")
            t)))
    (error (e)
      (format t "~%✗ CB-03 FAILED: ~A~%" e)
      nil)))

;;; ============================================================
;;; CB-04: Manual Reset
;;; ============================================================

(defun test-cb-04-manual-reset ()
  "Test manual circuit breaker reset."
  (format t "~%&CB-04: Manual reset~%")
  (handler-case
      (%with-clean-state
        ;; Trigger failures to open circuit
        (dotimes (i 5)
          (claw-lisp.storage.durable-memory-search:record-embedding-failure))
        ;; Verify circuit is open
        (%assert (not (null claw-lisp.storage.durable-memory-search:*dmq-circuit-open-until*))
                 "Circuit should be open")
        (%assert (= claw-lisp.storage.durable-memory-search:*dmq-embedding-failures* 5)
                 "Failure count should be 5")
        ;; Manual reset
        (claw-lisp.storage.durable-memory-search:reset-embedding-circuit-breaker)
        ;; Verify circuit is closed
        (%assert (null claw-lisp.storage.durable-memory-search:*dmq-circuit-open-until*)
                 "Circuit should be closed after reset")
        (%assert (= claw-lisp.storage.durable-memory-search:*dmq-embedding-failures* 0)
                 "Failure count should be 0 after reset")
        (format t "~%✓ CB-04 passed~%")
        t)
    (error (e)
      (format t "~%✗ CB-04 FAILED: ~A~%" e)
      nil)))

;;; ============================================================
;;; Test Runner
;;; ============================================================

(defun run-phase7-circuit-breaker-tests ()
  "Run all circuit breaker tests. Returns T if all pass."
  (format t "~%&=== Phase 7 Task 7 — Circuit Breaker Tests ===~%")
  (let ((results (list
                  (test-cb-01-normal-operation)
                  (test-cb-02-circuit-opens-after-threshold)
                  (test-cb-03-cooldown-expiry)
                  (test-cb-04-manual-reset))))
    (if (every #'identity results)
        (progn
          (format t "~%&ALL CIRCUIT BREAKER TESTS PASSED~%")
          t)
        (progn
          (format t "~%&SOME CIRCUIT BREAKER TESTS FAILED~%")
          nil))))

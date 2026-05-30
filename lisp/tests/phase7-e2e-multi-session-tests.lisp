;;;; lisp/tests/phase7-e2e-multi-session-tests.lisp
;;;;
;;;; Phase 7 Task 7 Step 5 — Multi-Session Tests
;;;;
;;;; Tests for multi-session isolation and memory sharing.

(in-package #:claw-lisp.tests)

;;; ============================================================
;;; MS-01: Parallel Sessions (Independent Dedup)
;;; ============================================================

(defun test-ms-01-parallel-sessions ()
  "Test that parallel sessions have independent dedup logs."
  (format t "~%&MS-01: Parallel sessions (independent dedup)~%")
  (handler-case
      (%with-clean-state
        (%with-temp-memory-store (:count 2 :kinds '(:user))
          (let* ((session-a (%create-test-session :turn-id 0))
                 (session-b (%create-test-session :turn-id 0))
                 (user-msg "What are my preferences?"))
            ;; Session A - Turn 1: should inject
            (multiple-value-bind (m1 meta1)
                (%simulate-turn session-a user-msg)
              (%assert-injected-memory-count meta1 1))
            ;; Session B - Turn 1: should ALSO inject (independent dedup)
            (multiple-value-bind (m2 meta2)
                (%simulate-turn session-b user-msg)
              (%assert-injected-memory-count meta2 1))
            (format t "~%✓ MS-01 passed~%")
            t)))
    (error (e)
      (format t "~%✗ MS-01 FAILED: ~A~%" e)
      nil)))

;;; ============================================================
;;; MS-02: Session Restart (Ephemeral Dedup)
;;; ============================================================

(defun test-ms-02-session-restart ()
  "Test that dedup is ephemeral (per-session, not persisted)."
  (format t "~%&MS-02: Session restart (ephemeral dedup)~%")
  (handler-case
      (%with-clean-state
        (%with-temp-memory-store (:count 2 :kinds '(:user))
          (let* ((session-1 (%create-test-session :turn-id 0))
                 (session-2 (%create-test-session :turn-id 0))
                 (user-msg "What are my preferences?"))
            ;; Session 1 - Turn 1: should inject
            (multiple-value-bind (m1 meta1)
                (%simulate-turn session-1 user-msg)
              (%assert-injected-memory-count meta1 1))
            ;; Session 1 - Turn 2: should NOT inject (dedup)
            (multiple-value-bind (m2 meta2)
                (%simulate-turn session-1 user-msg)
              (%assert-no-injection meta2))
            ;; Session 2 (new session) - Turn 1: should inject (fresh dedup)
            (multiple-value-bind (m3 meta3)
                (%simulate-turn session-2 user-msg)
              (%assert-injected-memory-count meta3 1))
            (format t "~%✓ MS-02 passed~%")
            t)))
    (error (e)
      (format t "~%✗ MS-02 FAILED: ~A~%" e)
      nil)))

;;; ============================================================
;;; MS-03: Cross-Session Sharing (Same Subject-ID)
;;; ============================================================

(defun test-ms-03-cross-session-sharing ()
  "Test that same memories are available across sessions with same subject-id."
  (format t "~%&MS-03: Cross-session sharing~%")
  (handler-case
      (%with-clean-state
        (%with-temp-memory-store (:count 3 :kinds '(:user))
          (let* ((session-a (%create-test-session :turn-id 0))
                 (session-b (%create-test-session :turn-id 0))
                 (user-msg "What are my preferences?"))
            ;; Session A - should retrieve memories
            (multiple-value-bind (m1 meta1)
                (%simulate-turn session-a user-msg)
              (%assert (>= (getf meta1 :count) 1)
                       "Session A should retrieve memories"))
            ;; Session B - should also retrieve same memories
            (multiple-value-bind (m2 meta2)
                (%simulate-turn session-b user-msg)
              (%assert (>= (getf meta2 :count) 1)
                       "Session B should retrieve memories"))
            (format t "~%✓ MS-03 passed~%")
            t)))
    (error (e)
      (format t "~%✗ MS-03 FAILED: ~A~%" e)
      nil)))

;;; ============================================================
;;; Test Runner
;;; ============================================================

(defun run-phase7-multi-session-tests ()
  "Run all multi-session tests. Returns T if all pass."
  (format t "~%&=== Phase 7 Task 7 — Multi-Session Tests ===~%")
  (let ((results (list
                  (test-ms-01-parallel-sessions)
                  (test-ms-02-session-restart)
                  (test-ms-03-cross-session-sharing))))
    (if (every #'identity results)
        (progn
          (format t "~%&ALL MULTI-SESSION TESTS PASSED~%")
          t)
        (progn
          (format t "~%&SOME MULTI-SESSION TESTS FAILED~%")
          nil))))

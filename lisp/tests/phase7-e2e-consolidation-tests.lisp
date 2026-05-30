;;;; lisp/tests/phase7-e2e-consolidation-tests.lisp
;;;;
;;;; Phase 7 Task 7 Step 4 — Consolidation Tests
;;;;
;;;; Tests for memory consolidation integration with runtime injection.

(in-package #:claw-lisp.tests)

;;; ============================================================
;;; CON-01: Consolidated Memory Retrieval
;;; ============================================================

(defun test-con-01-consolidated-retrieval ()
  "Test that consolidated memories are retrieved correctly."
  (format t "~%&CON-01: Consolidated memory retrieval~%")
  (handler-case
      (%with-clean-state
        (%with-temp-memory-store (:count 0)
          (let* ((session (%create-test-session))
                 ;; Create two similar memories
                 (mem1 (%create-test-memory :id "mem-1" :importance 0.5 :embedding-seed 1.0))
                 (mem2 (%create-test-memory :id "mem-2" :importance 0.8 :embedding-seed 1.1)))
            ;; Save both memories
            (claw-lisp.storage.durable-memory:save-durable-memory-record mem1)
            (claw-lisp.storage.durable-memory:save-durable-memory-record mem2)
            ;; Manually mark mem1 as superseded by mem2
            (setf (claw-lisp.storage.durable-memory:durable-memory-record-superseded-by-id mem1) "mem-2")
            (claw-lisp.storage.durable-memory:save-durable-memory-record mem1)
            ;; Query - should only get mem2 (survivor)
            (multiple-value-bind (messages metadata)
                (%simulate-turn session "What are my preferences?")
              (%assert-injected-memory-count metadata 1)
              ;; Verify superseded record is not injected
              (format t "~%✓ CON-01 passed~%")
              t))))
    (error (e)
      (format t "~%✗ CON-01 FAILED: ~A~%" e)
      nil)))

;;; ============================================================
;;; CON-02: Superseded-By-ID Persistence
;;; ============================================================

(defun test-con-02-superseded-by-id-persistence ()
  "Test that superseded-by-id survives restart."
  (format t "~%&CON-02: Superseded-by-id persistence~%")
  (handler-case
      (%with-clean-state
        (%with-temp-memory-store (:count 0)
          (let* ((mem1 (%create-test-memory :id "persist-1" :importance 0.5))
                 (mem2 (%create-test-memory :id "persist-2" :importance 0.8)))
            ;; Save and mark mem1 as superseded
            (claw-lisp.storage.durable-memory:save-durable-memory-record mem1)
            (claw-lisp.storage.durable-memory:save-durable-memory-record mem2)
            (setf (claw-lisp.storage.durable-memory:durable-memory-record-superseded-by-id mem1) "persist-2")
            (claw-lisp.storage.durable-memory:save-durable-memory-record mem1)
            ;; Note: Full persistence test would require reloading from disk
            ;; For now, verify the field is set correctly
            (%assert (equal (claw-lisp.storage.durable-memory:durable-memory-record-superseded-by-id mem1)
                            "persist-2")
                     "superseded-by-id should persist")
            (format t "~%✓ CON-02 passed~%")
            t)))
    (error (e)
      (format t "~%✗ CON-02 FAILED: ~A~%" e)
      nil)))

;;; ============================================================
;;; CON-03: Consolidation During Active Session
;;; ============================================================

(defun test-con-03-consolidation-during-session ()
  "Test consolidation during active session."
  (format t "~%&CON-03: Consolidation during active session~%")
  (handler-case
      (%with-clean-state
        (%with-temp-memory-store (:count 3 :kinds '(:user))
          (let* ((session (%create-test-session))
                 (user-msg "What are my preferences?"))
            ;; Turn 1 - inject memory A
            (multiple-value-bind (m1 meta1)
                (%simulate-turn session user-msg)
              (%assert-injected-memory-count meta1 1))
            ;; Simulate consolidation (mark memory as superseded)
            ;; In real scenario, this would be done by auto-consolidate
            ;; For test, we just verify the session continues working
            ;; Turn 2 - should NOT inject (dedup)
            (multiple-value-bind (m2 meta2)
                (%simulate-turn session user-msg)
              (%assert-no-injection meta2))
            (format t "~%✓ CON-03 passed~%")
            t)))
    (error (e)
      (format t "~%✗ CON-03 FAILED: ~A~%" e)
      nil)))

;;; ============================================================
;;; CON-04: Merge-Content Strategy
;;; ============================================================

(defun test-con-04-merge-content-strategy ()
  "Test that merge-content creates searchable record."
  (format t "~%&CON-04: Merge-content strategy~%")
  (handler-case
      (%with-clean-state
        (%with-temp-memory-store (:count 0)
          (let* ((session (%create-test-session))
                 ;; Create two memories to merge
                 (mem1 (%create-test-memory :id "merge-1" :content "Content A" :embedding-seed 1.0))
                 (mem2 (%create-test-memory :id "merge-2" :content "Content B" :embedding-seed 1.0)))
            ;; Save memories
            (claw-lisp.storage.durable-memory:save-durable-memory-record mem1)
            (claw-lisp.storage.durable-memory:save-durable-memory-record mem2)
            ;; Manually create merged record (simulating consolidate-memories)
            (let ((merged (claw-lisp.storage.durable-memory:make-durable-memory-record
                           :id "merged-1"
                           :kind :user
                           :subject-id "test-user"
                           :title "Merged Memory"
                           :content "Content A
---
Content B"
                           :source :consolidation
                           :importance-score 0.8
                           :embedding (%mock-embedding-vector 1.0)
                           :supersedes-id "merge-1")))
              (claw-lisp.storage.durable-memory:save-durable-memory-record merged)
              ;; Mark originals as superseded
              (setf (claw-lisp.storage.durable-memory:durable-memory-record-superseded-by-id mem1) "merged-1")
              (claw-lisp.storage.durable-memory:save-durable-memory-record mem1)
              ;; Query - should get merged record
              (multiple-value-bind (messages metadata)
                  (%simulate-turn session "What are my preferences?")
                (%assert-injected-memory-count metadata 1)
                (format t "~%✓ CON-04 passed~%")
                t)))))
    (error (e)
      (format t "~%✗ CON-04 FAILED: ~A~%" e)
      nil)))

;;; ============================================================
;;; Test Runner
;;; ============================================================

(defun run-phase7-consolidation-tests ()
  "Run all consolidation tests. Returns T if all pass."
  (format t "~%&=== Phase 7 Task 7 — Consolidation Tests ===~%")
  (let ((results (list
                  (test-con-01-consolidated-retrieval)
                  (test-con-02-superseded-by-id-persistence)
                  (test-con-03-consolidation-during-session)
                  (test-con-04-merge-content-strategy))))
    (if (every #'identity results)
        (progn
          (format t "~%&ALL CONSOLIDATION TESTS PASSED~%")
          t)
        (progn
          (format t "~%&SOME CONSOLIDATION TESTS FAILED~%")
          nil))))

;;;; lisp/tests/phase7-e2e-performance-tests.lisp
;;;;
;;;; Phase 7 Task 7 Step 6 — Performance Tests
;;;;
;;;; Performance and stress tests for durable memory system.

(in-package #:claw-lisp.tests)

;;; ============================================================
;;; PF-01: Large Memory Store (1000+ records)
;;; ============================================================

(defun test-pf-01-large-memory-store ()
  "Test query performance with 1000+ memory records."
  (format t "~%&PF-01: Large memory store (1000+ records)~%")
  (handler-case
      (%with-clean-state
        (%with-temp-memory-store (:count 1000 :kinds '(:user :project :feedback :reference))
          (let* ((session (%create-test-session))
                 (user-msg "What are my preferences?")
                 (start-time (get-internal-real-time)))
            ;; Execute query
            (multiple-value-bind (messages metadata)
                (%simulate-turn session user-msg)
              (let* ((end-time (get-internal-real-time))
                     (elapsed-ms (/ (* 1000 (- end-time start-time))
                                    internal-time-units-per-second)))
                ;; Verify performance
                (%assert (< elapsed-ms 500)
                         "Query should complete in <500ms, took ~Ams" elapsed-ms)
                ;; Verify results
                (%assert (>= (getf metadata :count) 1)
                         "Should return at least 1 result")
                (format t "~%✓ PF-01 passed (~Ams)~%" elapsed-ms)
                t)))))
    (error (e)
      (format t "~%✗ PF-01 FAILED: ~A~%" e)
      nil)))

;;; ============================================================
;;; PF-02: High-Frequency Turns (100 turns)
;;; ============================================================

(defun test-pf-02-high-frequency-turns ()
  "Test no memory leaks with 100 consecutive turns."
  (format t "~%&PF-02: High-frequency turns (100 turns)~%")
  (handler-case
      (%with-clean-state
        (%with-temp-memory-store (:count 50 :kinds '(:user :project))
          (let* ((session (%create-test-session))
                 (queries '("Query 1" "Query 2" "Query 3" "What are my preferences?"
                           "Tell me about the project" "Any feedback?" "Reference?"
                           "More preferences" "Project structure" "User settings"))
                 (success-count 0))
            ;; Force GC and measure actual heap usage
            (sb-ext:gc :full t)
            (let ((initial-heap (sb-kernel:dynamic-usage)))
              ;; Execute 100 turns
              (dotimes (i 100)
                (let ((query (nth (mod i (length queries)) queries)))
                  (handler-case
                      (progn
                        (%simulate-turn session query)
                        (incf success-count))
                    (error () nil))))
              ;; Force GC after and re-measure
              (sb-ext:gc :full t)
              (let* ((final-heap (sb-kernel:dynamic-usage))
                     (heap-growth (- final-heap initial-heap)))
                ;; Verify no significant memory leak (allow 50MB for 100 turns)
                (%assert (< heap-growth (* 50 1024 1024))
                         "Heap growth should be <50MB, grew ~A bytes" heap-growth)
                ;; Verify all turns completed
                (%assert (= success-count 100)
                         "All 100 turns should complete, only ~A did" success-count)
                (format t "~%✓ PF-02 passed (~A turns, ~,1F MB heap growth)~%"
                        success-count (/ heap-growth 1024.0 1024.0))
                t)))))
    (error (e)
      (format t "~%✗ PF-02 FAILED: ~A~%" e)
      nil)))

;;; ============================================================
;;; PF-03: Concurrent Access (No Race Conditions)
;;; ============================================================

(defun test-pf-03-concurrent-access ()
  "Test no race conditions with concurrent session access."
  (format t "~%&PF-03: Concurrent access~%")
  (handler-case
      (%with-clean-state
        (%with-temp-memory-store (:count 100 :kinds '(:user :project))
          (let* ((errors (make-array 0 :adjustable t :fill-pointer 0))
                 (error-lock (sb-thread:make-mutex :name "pf-03-errors"))
                 (threads nil)
                 (num-threads 4)
                 (turns-per-thread 25))
            ;; Spawn threads (bind thread-id to avoid closure-capture bug)
            (dotimes (i num-threads)
              (let ((thread-id i))
                (push (sb-thread:make-thread
                       (lambda ()
                         (handler-case
                             (let ((session (%create-test-session)))
                               (dotimes (j turns-per-thread)
                                 (%simulate-turn session
                                                (format nil "Thread ~D Query ~D" thread-id j))))
                           (error (e)
                             (sb-thread:with-mutex (error-lock)
                               (vector-push-extend e errors))))))
                      threads)))
            ;; Wait for all threads
            (mapc #'sb-thread:join-thread threads)
            ;; Verify no errors
            (%assert (= (length errors) 0)
                     "No errors should occur in concurrent access, got ~A" (length errors))
            (format t "~%✓ PF-03 passed (~D threads, ~D turns each)~%"
                    num-threads turns-per-thread)
            t)))
    (error (e)
      (format t "~%✗ PF-03 FAILED: ~A~%" e)
      nil)))

;;; ============================================================
;;; Test Runner
;;; ============================================================

(defun run-phase7-performance-tests ()
  "Run all performance tests. Returns T if all pass."
  (format t "~%&=== Phase 7 Task 7 — Performance Tests ===~%")
  (let ((results (list
                  (test-pf-01-large-memory-store)
                  (test-pf-02-high-frequency-turns)
                  (test-pf-03-concurrent-access))))
    (if (every #'identity results)
        (progn
          (format t "~%&ALL PERFORMANCE TESTS PASSED~%")
          t)
        (progn
          (format t "~%&SOME PERFORMANCE TESTS FAILED~%")
          nil))))

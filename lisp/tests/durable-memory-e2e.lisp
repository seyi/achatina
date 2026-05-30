;;;; lisp/tests/durable-memory-e2e.lisp
;;;;
;;;; End-to-end test for Phase 6 Durable Memory System.
;;;; Verifies that durable memory pipeline functions work correctly.
;;;;
;;;; Run with: (claw-lisp.tests:run-durable-memory-e2e-test)

(in-package #:claw-lisp.tests)

(defun %assert (condition format-control &rest format-args)
  "Assert that CONDITION is true. Signals an error with FORMAT-ARGS on failure."
  (unless condition
    (error (apply #'format nil format-control format-args))))

(defun run-durable-memory-e2e-test ()
  "End-to-end test: verify durable memory pipeline functions work.
   
   This test:
   1. Creates a durable memory record via the scoring pipeline
   2. Saves it via the storage backend
   3. Loads it back
   4. Verifies the file was written
   
   Returns T if all checks pass, NIL otherwise."
  (format t "~%=== Phase 6 Durable Memory E2E Test ===~%")
  (let* ((test-id (format nil "e2e-~A" (get-universal-time)))
         (root (merge-pathnames
                (format nil "claw-lisp-e2e-test-~A/" test-id)
                #P"/tmp/"))
         (memory-root (merge-pathnames "memory/durable/" root))
         (user-dir (merge-pathnames "user/" memory-root))
         (passed t)
         saved-record loaded-records)
    (unwind-protect
         (progn
           ;; Temporarily override storage root for test
           (let ((claw-lisp.storage.durable-memory::*durable-memory-storage-root* memory-root))
             ;; Ensure directories exist
             (format t "  Creating test directory at ~A...~%" user-dir)
             (ensure-directories-exist user-dir)
             
             ;; Test 1: Create and score a candidate
             (format t "  Creating durable memory candidate...~%")
             (let* ((candidate (claw-lisp.storage.durable-memory:make-durable-memory-candidate
                                :kind :user
                                :subject-id test-id
                                :content "I prefer dark mode in the IDE"
                                :source :conversation
                                :explicit-user-request-p t))
                    (save-result (multiple-value-list
                                  (claw-lisp.storage.durable-memory:should-save-durable-memory-p candidate))))
               (format t "    Scoring result: ~A~%" save-result)
               (if (first save-result)
                   (format t "    ✓ Candidate should be saved~%")
                   (progn
                     (format t "    ✗ Candidate should be saved but wasn't~%")
                     (setf passed nil))))
             
             ;; Test 2: Save a record directly
             (format t "  Saving durable memory record...~%")
             (let ((record (claw-lisp.storage.durable-memory:make-user-memory
                            :subject-id test-id
                            :title "E2E Test Preference"
                            :content "I prefer dark mode in the IDE"
                            :source :conversation
                            :importance-score 0.8
                            :tags '(:e2e-test :preference))))
               (setf saved-record (claw-lisp.storage.durable-memory:save-durable-memory-record record))
               (if saved-record
                   (format t "    ✓ Record saved with ID: ~A~%" (claw-lisp.storage.durable-memory:durable-memory-record-id saved-record))
                   (progn
                     (format t "    ✗ Record save returned NIL~%")
                     (setf passed nil))))
             
             ;; Test 3: Load records back
             (format t "  Loading durable memory records...~%")
             (setf loaded-records (claw-lisp.storage.durable-memory:load-durable-memories :user test-id))
             (if loaded-records
                 (format t "    ✓ Loaded ~A record(s)~%" (length loaded-records))
                 (progn
                   (format t "    ✗ No records loaded~%")
                   (setf passed nil)))
             
             ;; Test 4: Verify file exists
             (format t "  Checking memory file exists...~%")
             (let ((expected-file (merge-pathnames (format nil "~A.md" test-id) user-dir)))
               (if (probe-file expected-file)
                   (format t "    ✓ Memory file exists at ~A~%" expected-file)
                   (progn
                     (format t "    ✗ Memory file NOT found at ~A~%" expected-file)
                     (setf passed nil))))
             
             ;; Test 5: Verify file content
             (when (probe-file (merge-pathnames (format nil "~A.md" test-id) user-dir))
               (format t "  Checking memory file content...~%")
               (let ((content (uiop:read-file-string (merge-pathnames (format nil "~A.md" test-id) user-dir))))
                 (if (search "I prefer dark mode" content)
                     (format t "    ✓ Content found in file~%")
                     (progn
                       (format t "    ✗ Content NOT found in file~%")
                       (format t "    File content: ~A~%" content)
                       (setf passed nil)))))
             
             ;; Summary
             (format t "~%=== E2E Test Result ===~%")
             (if passed
                 (format t "✓ ALL CHECKS PASSED - Phase 6 durable memory pipeline is functional!~%")
                 (format t "✗ SOME CHECKS FAILED - See above for details~%"))
             passed))
         
      ;; Cleanup
      (when (probe-file root)
        (format t "  Cleaning up test directory...~%")
        (uiop:delete-directory-tree root :validate t)))))

;; Run the test if this file is loaded directly
#|(run-durable-memory-e2e-test)|#

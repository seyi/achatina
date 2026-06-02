(in-package #:claw-lisp.tests)

;;; FND-003 Tool Envelope Tests

(defun test-wrap-tool-success ()
  (let ((env (claw-lisp.core.tool-envelope:wrap-tool-success
              "file-read" "file contents"
              :duration-ms 42 :phase :inspect)))
    (%assert (claw-lisp.core.tool-envelope:envelope-succeeded-p env)
             "Success envelope should report succeeded")
    (%assert (not (claw-lisp.core.tool-envelope:envelope-failed-p env))
             "Success envelope should not report failed")
    (%assert (string= "file-read" (claw-lisp.core.tool-envelope:tool-envelope-tool-name env))
             "Tool name should be file-read")
    (%assert (string= "file contents" (claw-lisp.core.tool-envelope:tool-envelope-content env))
             "Content should be preserved")
    (%assert (= 42 (claw-lisp.core.tool-envelope:tool-envelope-duration-ms env))
             "Duration should be 42")
    (%assert (eq :inspect (claw-lisp.core.tool-envelope:tool-envelope-phase-at-execution env))
             "Phase should be :inspect")
    (%assert (> (claw-lisp.core.tool-envelope:tool-envelope-timestamp env) 0)
             "Timestamp should be set")
    (format t "  ✓ wrap-tool-success~%")))

(defun test-wrap-tool-failure ()
  (let ((env (claw-lisp.core.tool-envelope:wrap-tool-failure
              "file-write"
              (make-condition 'claw-lisp.core.conditions:permission-error
                             :tool-name "file-write" :message "denied")
              :duration-ms 5 :phase :edit)))
    (%assert (claw-lisp.core.tool-envelope:envelope-failed-p env)
             "Failure envelope should report failed")
    (%assert (not (claw-lisp.core.tool-envelope:envelope-succeeded-p env))
             "Failure envelope should not report succeeded")
    (%assert (eq :permission (claw-lisp.core.tool-envelope:tool-envelope-error-type env))
             "Error type should be :permission")
    (%assert (stringp (claw-lisp.core.tool-envelope:tool-envelope-error-message env))
             "Error message should be a string")
    (%assert (eq :edit (claw-lisp.core.tool-envelope:tool-envelope-phase-at-execution env))
             "Phase should be :edit")
    (format t "  ✓ wrap-tool-failure~%")))

(defun test-classify-tool-error ()
  (%assert (eq :permission
               (claw-lisp.core.tool-envelope:classify-tool-error
                (make-condition 'claw-lisp.core.conditions:permission-error
                               :tool-name "x" :message "no")))
           "permission-error should classify as :permission")
  (%assert (eq :execution
               (claw-lisp.core.tool-envelope:classify-tool-error
                (make-condition 'claw-lisp.core.conditions:tool-error
                               :tool-name "x" :message "fail")))
           "tool-error should classify as :execution")
  (%assert (eq :validation
               (claw-lisp.core.tool-envelope:classify-tool-error
                (make-condition 'claw-lisp.core.conditions:config-error
                               :message "bad")))
           "config-error should classify as :validation")
  (%assert (eq :execution
               (claw-lisp.core.tool-envelope:classify-tool-error
                (make-condition 'simple-error :format-control "unknown")))
           "Unknown errors should classify as :execution")
  (format t "  ✓ classify-tool-error~%"))

(defun test-envelope-from-tool-result-success ()
  (let* ((tr (claw-lisp.core.domain:make-tool-result
              :call-id "c1" :tool-name "grep" :content "match found"))
         (env (claw-lisp.core.tool-envelope:envelope-from-tool-result tr :phase :inspect)))
    (%assert (claw-lisp.core.tool-envelope:envelope-succeeded-p env)
             "Normal content should be success")
    (%assert (string= "grep" (claw-lisp.core.tool-envelope:tool-envelope-tool-name env))
             "Tool name should be grep")
    (%assert (string= "match found" (claw-lisp.core.tool-envelope:tool-envelope-content env))
             "Content should be preserved")
    (%assert (eq :inspect (claw-lisp.core.tool-envelope:tool-envelope-phase-at-execution env))
             "Phase should be :inspect")
    (format t "  ✓ envelope-from-tool-result (success)~%")))

(defun test-envelope-from-tool-result-error-content ()
  (let* ((tr (claw-lisp.core.domain:make-tool-result
              :call-id "c2" :tool-name "file-read" :content "Error: file not found"))
         (env (claw-lisp.core.tool-envelope:envelope-from-tool-result tr)))
    (%assert (claw-lisp.core.tool-envelope:envelope-failed-p env)
             "Error: prefix should be detected as failure")
    (%assert (eq :execution (claw-lisp.core.tool-envelope:tool-envelope-error-type env))
             "Error type should be :execution")
    (format t "  ✓ envelope-from-tool-result (error prefix)~%")))

(defun test-envelope-from-tool-result-case-insensitive ()
  (let* ((tr (claw-lisp.core.domain:make-tool-result
              :call-id "c3" :tool-name "grep" :content "ERROR: something went wrong"))
         (env (claw-lisp.core.tool-envelope:envelope-from-tool-result tr)))
    (%assert (claw-lisp.core.tool-envelope:envelope-failed-p env)
             "ERROR: (uppercase) should be detected as failure")
    (format t "  ✓ envelope-from-tool-result (case-insensitive)~%")))

(defun test-envelope-from-tool-result-empty-content ()
  (let* ((tr (claw-lisp.core.domain:make-tool-result
              :call-id "c4" :tool-name "file-read" :content ""))
         (env (claw-lisp.core.tool-envelope:envelope-from-tool-result tr)))
    (%assert (claw-lisp.core.tool-envelope:envelope-succeeded-p env)
             "Empty content should be success (valid empty file)")
    (format t "  ✓ envelope-from-tool-result (empty content = success)~%")))

(defun test-envelope-from-tool-result-short-content ()
  (let* ((tr (claw-lisp.core.domain:make-tool-result
              :call-id "c5" :tool-name "echo" :content "Err"))
         (env (claw-lisp.core.tool-envelope:envelope-from-tool-result tr)))
    (%assert (claw-lisp.core.tool-envelope:envelope-succeeded-p env)
             "Content shorter than 6 chars should be success even if partial match")
    (format t "  ✓ envelope-from-tool-result (short content not false-positive)~%")))

(defun test-envelope-tool-classification ()
  ;; Read-only tools
  (%assert (claw-lisp.core.tool-envelope:envelope-is-read-only-p
            (claw-lisp.core.tool-envelope:wrap-tool-success "file-read" "x"))
           "file-read should be read-only")
  (%assert (claw-lisp.core.tool-envelope:envelope-is-read-only-p
            (claw-lisp.core.tool-envelope:wrap-tool-success "glob" "x"))
           "glob should be read-only")
  (%assert (claw-lisp.core.tool-envelope:envelope-is-read-only-p
            (claw-lisp.core.tool-envelope:wrap-tool-success "grep" "x"))
           "grep should be read-only")

  ;; Mutation tools
  (%assert (claw-lisp.core.tool-envelope:envelope-is-mutation-p
            (claw-lisp.core.tool-envelope:wrap-tool-success "file-write" "x"))
           "file-write should be mutation")
  (%assert (claw-lisp.core.tool-envelope:envelope-is-mutation-p
            (claw-lisp.core.tool-envelope:wrap-tool-success "file-replace" "x"))
           "file-replace should be mutation")

  ;; Mutation tools should NOT be read-only
  (%assert (not (claw-lisp.core.tool-envelope:envelope-is-read-only-p
                 (claw-lisp.core.tool-envelope:wrap-tool-success "file-write" "x")))
           "file-write should not be read-only")

  ;; Unknown tools (echo, shell-command) return NIL from both
  (%assert (not (claw-lisp.core.tool-envelope:envelope-is-read-only-p
                 (claw-lisp.core.tool-envelope:wrap-tool-success "echo" "x")))
           "echo should not be read-only")
  (%assert (not (claw-lisp.core.tool-envelope:envelope-is-mutation-p
                 (claw-lisp.core.tool-envelope:wrap-tool-success "echo" "x")))
           "echo should not be mutation")
  (%assert (not (claw-lisp.core.tool-envelope:envelope-is-read-only-p
                 (claw-lisp.core.tool-envelope:wrap-tool-success "shell-command" "x")))
           "shell-command should not be read-only (can mutate)")
  (%assert (not (claw-lisp.core.tool-envelope:envelope-is-mutation-p
                 (claw-lisp.core.tool-envelope:wrap-tool-success "shell-command" "x")))
           "shell-command should not be mutation (can also read)")

  (format t "  ✓ tool classification (read-only, mutation, unknown)~%"))

(defun test-envelope-failed-p-is-not-success ()
  "Verify envelope-failed-p is simply (not success), no separate field."
  (let ((success-env (claw-lisp.core.tool-envelope:wrap-tool-success "x" "y"))
        (fail-env (claw-lisp.core.tool-envelope:wrap-tool-failure
                   "x" (make-condition 'simple-error :format-control "z"))))
    (%assert (eq (not (claw-lisp.core.tool-envelope:envelope-succeeded-p success-env))
                 (claw-lisp.core.tool-envelope:envelope-failed-p success-env))
             "failed-p should be (not succeeded-p) for success envelope")
    (%assert (eq (not (claw-lisp.core.tool-envelope:envelope-succeeded-p fail-env))
                 (claw-lisp.core.tool-envelope:envelope-failed-p fail-env))
             "failed-p should be (not succeeded-p) for failure envelope")
    (format t "  ✓ envelope-failed-p is strictly (not success)~%")))

(defun run-tool-envelope-tests ()
  (format t "~%=== FND-003 Tool Envelope Tests ===~%~%")
  (test-wrap-tool-success)
  (test-wrap-tool-failure)
  (test-classify-tool-error)
  (test-envelope-from-tool-result-success)
  (test-envelope-from-tool-result-error-content)
  (test-envelope-from-tool-result-case-insensitive)
  (test-envelope-from-tool-result-empty-content)
  (test-envelope-from-tool-result-short-content)
  (test-envelope-tool-classification)
  (test-envelope-failed-p-is-not-success)
  (format t "~%=== All FND-003 Tests Passed! ===~%"))

(in-package #:claw-lisp.tests)

;;; FND-004 Tool Phase Compatibility Tests

(defun test-file-read-valid-phases ()
  (let ((tool (make-instance 'claw-lisp.tools.file-read::file-read-tool
                             :name "file-read" :description "read")))
    (%assert (claw-lisp.core.tool-phases:tool-valid-for-phase-p tool :inspect)
             "file-read should be valid in :inspect")
    (%assert (claw-lisp.core.tool-phases:tool-valid-for-phase-p tool :edit)
             "file-read should be valid in :edit")
    (%assert (claw-lisp.core.tool-phases:tool-valid-for-phase-p tool :verify)
             "file-read should be valid in :verify")
    (%assert (not (claw-lisp.core.tool-phases:tool-valid-for-phase-p tool :complete))
             "file-read should not be valid in :complete")
    (format t "  ✓ file-read valid phases~%")))

(defun test-file-write-valid-phases ()
  (let ((tool (make-instance 'claw-lisp.tools.file-write::file-write-tool
                             :name "file-write" :description "write")))
    (%assert (claw-lisp.core.tool-phases:tool-valid-for-phase-p tool :edit)
             "file-write should be valid in :edit")
    (%assert (not (claw-lisp.core.tool-phases:tool-valid-for-phase-p tool :inspect))
             "file-write should not be valid in :inspect")
    (%assert (not (claw-lisp.core.tool-phases:tool-valid-for-phase-p tool :verify))
             "file-write should not be valid in :verify")
    (%assert (not (claw-lisp.core.tool-phases:tool-valid-for-phase-p tool :complete))
             "file-write should not be valid in :complete")
    (format t "  ✓ file-write valid only in :edit~%")))

(defun test-shell-command-valid-phases ()
  (let ((tool (make-instance 'claw-lisp.tools.shell-command::shell-command-tool
                             :name "shell-command" :description "shell")))
    (%assert (claw-lisp.core.tool-phases:tool-valid-for-phase-p tool :edit)
             "shell-command should be valid in :edit")
    (%assert (claw-lisp.core.tool-phases:tool-valid-for-phase-p tool :verify)
             "shell-command should be valid in :verify")
    (%assert (not (claw-lisp.core.tool-phases:tool-valid-for-phase-p tool :inspect))
             "shell-command should not be valid in :inspect")
    (format t "  ✓ shell-command valid in :edit and :verify~%")))

(defun test-echo-tool-valid-all-phases ()
  (let ((tool (make-instance 'claw-lisp.tools.echo::echo-tool
                             :name "echo" :description "echo")))
    (%assert (claw-lisp.core.tool-phases:tool-valid-for-phase-p tool :inspect)
             "echo should be valid in :inspect")
    (%assert (claw-lisp.core.tool-phases:tool-valid-for-phase-p tool :edit)
             "echo should be valid in :edit")
    (%assert (claw-lisp.core.tool-phases:tool-valid-for-phase-p tool :verify)
             "echo should be valid in :verify")
    (%assert (claw-lisp.core.tool-phases:tool-valid-for-phase-p tool :complete)
             "echo should be valid in :complete")
    (format t "  ✓ echo tool valid in all phases (default)~%")))

(defun test-phase-violation-signaled ()
  (let ((tool (make-instance 'claw-lisp.tools.file-write::file-write-tool
                             :name "file-write" :description "write"))
        (session (claw-lisp.core.domain:make-agent-session
                  :id "phase-test" :provider :mock :model "m"
                  :conversation nil :state nil)))
    (claw-lisp.core.phases:initialize-phase-state session)
    (claw-lisp.core.phases:transition-phase session :inspect "start")

    ;; file-write in :inspect should signal phase-violation-error
    (handler-case
        (progn
          (claw-lisp.core.tool-phases:check-tool-phase-compatibility tool session)
          (%assert nil "Should have signaled phase-violation-error"))
      (claw-lisp.core.conditions:phase-violation-error (c)
        (%assert (string= "file-write" (claw-lisp.core.conditions:phase-violation-tool c))
                 "Violation should name the tool")
        (%assert (eq :inspect (claw-lisp.core.conditions:phase-violation-current-phase c))
                 "Violation should report current phase")))
    (format t "  ✓ phase violation signaled for wrong phase~%")))

(defun test-no-violation-when-no-phase ()
  (let ((tool (make-instance 'claw-lisp.tools.file-write::file-write-tool
                             :name "file-write" :description "write"))
        (session (claw-lisp.core.domain:make-agent-session
                  :id "no-phase-test" :provider :mock :model "m"
                  :conversation nil :state nil)))
    ;; No phase initialized — should be permissive
    (handler-case
        (progn
          (claw-lisp.core.tool-phases:check-tool-phase-compatibility tool session)
          (format t "  ✓ no violation when phase not set (permissive)~%"))
      (error (e)
        (%assert nil "Should not signal when no phase set: ~A" e)))))

(defun run-tool-phases-tests ()
  (format t "~%=== FND-004 Tool Phase Compatibility Tests ===~%~%")
  (test-file-read-valid-phases)
  (test-file-write-valid-phases)
  (test-shell-command-valid-phases)
  (test-echo-tool-valid-all-phases)
  (test-phase-violation-signaled)
  (test-no-violation-when-no-phase)
  (format t "~%=== All FND-004 Tests Passed! ===~%"))

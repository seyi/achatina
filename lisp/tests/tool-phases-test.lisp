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

(defun test-file-replace-valid-phases ()
  (let ((tool (make-instance 'claw-lisp.tools.file-replace::file-replace-tool
                             :name "file-replace" :description "replace")))
    (%assert (claw-lisp.core.tool-phases:tool-valid-for-phase-p tool :edit)
             "file-replace should be valid in :edit")
    (%assert (not (claw-lisp.core.tool-phases:tool-valid-for-phase-p tool :inspect))
             "file-replace should not be valid in :inspect")
    (%assert (not (claw-lisp.core.tool-phases:tool-valid-for-phase-p tool :verify))
             "file-replace should not be valid in :verify")
    (%assert (not (claw-lisp.core.tool-phases:tool-valid-for-phase-p tool :complete))
             "file-replace should not be valid in :complete")
    (format t "  ✓ file-replace valid only in :edit~%")))

(defun test-grep-valid-phases ()
  (let ((tool (make-instance 'claw-lisp.tools.grep::grep-tool
                             :name "grep" :description "grep")))
    (%assert (claw-lisp.core.tool-phases:tool-valid-for-phase-p tool :inspect)
             "grep should be valid in :inspect")
    (%assert (claw-lisp.core.tool-phases:tool-valid-for-phase-p tool :edit)
             "grep should be valid in :edit")
    (%assert (claw-lisp.core.tool-phases:tool-valid-for-phase-p tool :verify)
             "grep should be valid in :verify")
    (%assert (not (claw-lisp.core.tool-phases:tool-valid-for-phase-p tool :complete))
             "grep should not be valid in :complete")
    (format t "  ✓ grep valid in :inspect, :edit, :verify~%")))

(defun test-glob-valid-phases ()
  (let ((tool (make-instance 'claw-lisp.tools.glob::glob-tool
                             :name "glob" :description "glob")))
    (%assert (claw-lisp.core.tool-phases:tool-valid-for-phase-p tool :inspect)
             "glob should be valid in :inspect")
    (%assert (claw-lisp.core.tool-phases:tool-valid-for-phase-p tool :edit)
             "glob should be valid in :edit")
    (%assert (claw-lisp.core.tool-phases:tool-valid-for-phase-p tool :verify)
             "glob should be valid in :verify")
    (%assert (not (claw-lisp.core.tool-phases:tool-valid-for-phase-p tool :complete))
             "glob should not be valid in :complete")
    (format t "  ✓ glob valid in :inspect, :edit, :verify~%")))

(defun test-positive-path-compatibility-check ()
  (let ((tool (make-instance 'claw-lisp.tools.file-write::file-write-tool
                             :name "file-write" :description "write"))
        (session (claw-lisp.core.domain:make-agent-session
                  :id "positive-test" :provider :mock :model "m"
                  :conversation nil :state nil)))
    (claw-lisp.core.phases:initialize-phase-state session)
    (claw-lisp.core.phases:transition-phase session :inspect "start")
    (claw-lisp.core.phases:transition-phase session :edit "editing")

    ;; file-write in :edit should succeed without signaling
    (claw-lisp.core.tool-phases:check-tool-phase-compatibility tool session)
    (format t "  ✓ positive path: file-write valid in :edit (no signal)~%")))

(defun test-no-violation-when-no-phase ()
  (let ((tool (make-instance 'claw-lisp.tools.file-write::file-write-tool
                             :name "file-write" :description "write"))
        (session (claw-lisp.core.domain:make-agent-session
                  :id "no-phase-test" :provider :mock :model "m"
                  :conversation nil :state nil))
        (violation-signaled nil))
    ;; No phase initialized — should be permissive
    (handler-case
        (claw-lisp.core.tool-phases:check-tool-phase-compatibility tool session)
      (claw-lisp.core.conditions:phase-violation-error ()
        (setf violation-signaled t)))
    (%assert (not violation-signaled)
             "Should not signal phase-violation-error when no phase set")
    (format t "  ✓ no violation when phase not set (permissive)~%")))

(defun run-tool-phases-tests ()
  (format t "~%=== FND-004 Tool Phase Compatibility Tests ===~%~%")
  (test-file-read-valid-phases)
  (test-file-write-valid-phases)
  (test-file-replace-valid-phases)
  (test-grep-valid-phases)
  (test-glob-valid-phases)
  (test-shell-command-valid-phases)
  (test-echo-tool-valid-all-phases)
  (test-phase-violation-signaled)
  (test-positive-path-compatibility-check)
  (test-no-violation-when-no-phase)
  (format t "~%=== All FND-004 Tests Passed! ===~%"))

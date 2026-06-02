(in-package #:claw-lisp.tests)

;;; PHZ-003/004 Phase Progression Policy Tests

(defun %make-test-runtime-for-progression ()
  "Create a minimal runtime with tools registered for progression tests."
  (let ((runtime (make-runtime)))
    (register-tool runtime (make-instance 'claw-lisp.tools.file-read::file-read-tool
                                          :name "file-read" :description "read"))
    (register-tool runtime (make-instance 'claw-lisp.tools.file-write::file-write-tool
                                          :name "file-write" :description "write"))
    (register-tool runtime (make-instance 'claw-lisp.tools.grep::grep-tool
                                          :name "grep" :description "grep"))
    (register-tool runtime (make-instance 'claw-lisp.tools.shell-command::shell-command-tool
                                          :name "shell-command" :description "shell"))
    runtime))

(defun %make-test-session ()
  (let ((session (claw-lisp.core.domain:make-agent-session
                  :id "progression-test" :provider :mock :model "m"
                  :conversation nil :state nil)))
    (claw-lisp.core.phases:initialize-phase-state session)
    session))

(defun test-classify-read-only-tools ()
  (let ((runtime (%make-test-runtime-for-progression))
        (tool-calls '((:name "file-read" :id "t1" :input nil)
                      (:name "grep" :id "t2" :input nil))))
    (%assert (eq :read-only
                 (claw-lisp.core.phase-progression:classify-tool-calls-for-phase
                  tool-calls runtime))
             "file-read + grep should classify as :read-only")
    (format t "  ✓ classify read-only tools~%")))

(defun test-classify-mutation-tools ()
  (let ((runtime (%make-test-runtime-for-progression))
        (tool-calls '((:name "file-write" :id "t1" :input nil))))
    (%assert (eq :mutation
                 (claw-lisp.core.phase-progression:classify-tool-calls-for-phase
                  tool-calls runtime))
             "file-write should classify as :mutation")
    (format t "  ✓ classify mutation tools~%")))

(defun test-classify-mixed-tools ()
  (let ((runtime (%make-test-runtime-for-progression))
        (tool-calls '((:name "file-read" :id "t1" :input nil)
                      (:name "file-write" :id "t2" :input nil))))
    (%assert (eq :mixed
                 (claw-lisp.core.phase-progression:classify-tool-calls-for-phase
                  tool-calls runtime))
             "file-read + file-write should classify as :mixed")
    (format t "  ✓ classify mixed tools~%")))

(defun test-recommend-edit-on-mutation-in-inspect ()
  (let ((runtime (%make-test-runtime-for-progression))
        (session (%make-test-session))
        (tool-calls '((:name "file-write" :id "t1" :input nil))))
    (claw-lisp.core.phases:transition-phase session :inspect "start")
    (multiple-value-bind (phase reason)
        (claw-lisp.core.phase-progression:recommend-phase-transition
         session tool-calls runtime)
      (%assert (eq :edit phase) "Should recommend :edit")
      (%assert (string= "mutation-tool-in-inspect" reason) "Reason should be mutation")
      (format t "  ✓ recommend :edit on mutation in :inspect~%"))))

(defun test-recommend-edit-on-stagnation ()
  (let ((runtime (%make-test-runtime-for-progression))
        (session (%make-test-session))
        (tool-calls '((:name "file-read" :id "t1" :input nil))))
    (claw-lisp.core.phases:transition-phase session :inspect "start")
    ;; Simulate stagnation
    (dotimes (i claw-lisp.core.phase-progression:+read-only-stagnation-threshold+)
      (claw-lisp.core.phases:increment-phase-counter session :inspect))
    (multiple-value-bind (phase reason)
        (claw-lisp.core.phase-progression:recommend-phase-transition
         session tool-calls runtime)
      (%assert (eq :edit phase) "Should recommend :edit after stagnation")
      (%assert (string= "read-only-stagnation" reason) "Reason should be stagnation")
      (format t "  ✓ recommend :edit on read-only stagnation~%"))))

(defun test-no-recommendation-below-threshold ()
  (let ((runtime (%make-test-runtime-for-progression))
        (session (%make-test-session))
        (tool-calls '((:name "file-read" :id "t1" :input nil))))
    (claw-lisp.core.phases:transition-phase session :inspect "start")
    ;; Below threshold
    (claw-lisp.core.phases:increment-phase-counter session :inspect)
    (multiple-value-bind (phase reason)
        (claw-lisp.core.phase-progression:recommend-phase-transition
         session tool-calls runtime)
      (%assert (null phase) "Should not recommend below threshold")
      (%assert (null reason) "Reason should be nil")
      (format t "  ✓ no recommendation below stagnation threshold~%"))))

(defun test-recommend-verify-after-edit-stagnation ()
  (let ((runtime (%make-test-runtime-for-progression))
        (session (%make-test-session))
        (tool-calls '((:name "file-read" :id "t1" :input nil))))
    (claw-lisp.core.phases:transition-phase session :inspect "start")
    (claw-lisp.core.phases:transition-phase session :edit "editing")
    ;; Simulate edit phase with only read-only tools
    (dotimes (i claw-lisp.core.phase-progression:+edit-without-verify-threshold+)
      (claw-lisp.core.phases:increment-phase-counter session :edit))
    (multiple-value-bind (phase reason)
        (claw-lisp.core.phase-progression:recommend-phase-transition
         session tool-calls runtime)
      (%assert (eq :verify phase) "Should recommend :verify")
      (%assert (string= "edit-phase-read-only-tools" reason)
               "Reason should be edit-phase-read-only-tools")
      (format t "  ✓ recommend :verify after edit stagnation with read-only tools~%"))))

(defun test-apply-progression-policy-transitions ()
  (let ((runtime (%make-test-runtime-for-progression))
        (session (%make-test-session))
        (tool-calls '((:name "file-write" :id "t1" :input nil))))
    (claw-lisp.core.phases:transition-phase session :inspect "start")
    (multiple-value-bind (transitioned new-phase reason)
        (claw-lisp.core.phase-progression:apply-progression-policy
         session tool-calls runtime nil)
      (%assert transitioned "Should have transitioned")
      (%assert (eq :edit new-phase) "New phase should be :edit")
      (%assert (stringp reason) "Reason should be a string")
      (%assert (eq :edit (claw-lisp.core.phases:get-current-phase session))
               "Session should now be in :edit")
      (format t "  ✓ apply-progression-policy performs transition~%"))))

(defun test-apply-progression-policy-no-op ()
  (let ((runtime (%make-test-runtime-for-progression))
        (session (%make-test-session))
        (tool-calls '((:name "file-read" :id "t1" :input nil))))
    (claw-lisp.core.phases:transition-phase session :inspect "start")
    (multiple-value-bind (transitioned new-phase reason)
        (claw-lisp.core.phase-progression:apply-progression-policy
         session tool-calls runtime nil)
      (%assert (not transitioned) "Should not transition")
      (%assert (null new-phase) "No new phase")
      (%assert (null reason) "No reason")
      (%assert (eq :inspect (claw-lisp.core.phases:get-current-phase session))
               "Session should remain in :inspect")
      (format t "  ✓ apply-progression-policy no-op when below threshold~%"))))

(defun test-no-recommendation-with-nil-tool-calls ()
  (let ((runtime (%make-test-runtime-for-progression))
        (session (%make-test-session)))
    (claw-lisp.core.phases:transition-phase session :inspect "start")
    ;; Exceed stagnation threshold
    (dotimes (i (+ claw-lisp.core.phase-progression:+read-only-stagnation-threshold+ 2))
      (claw-lisp.core.phases:increment-phase-counter session :inspect))
    ;; With nil tool-calls, should NOT recommend despite stagnation
    (multiple-value-bind (phase reason)
        (claw-lisp.core.phase-progression:recommend-phase-transition
         session nil runtime)
      (%assert (null phase) "Should not recommend with nil tool-calls")
      (%assert (null reason) "Reason should be nil")
      (format t "  ✓ no recommendation with nil tool-calls (permissive)~%"))))

(defun test-classify-nil-tool-calls ()
  (let ((runtime (%make-test-runtime-for-progression)))
    (%assert (null (claw-lisp.core.phase-progression:classify-tool-calls-for-phase
                    nil runtime))
             "nil tool-calls should classify as nil")
    (%assert (null (claw-lisp.core.phase-progression:classify-tool-calls-for-phase
                    '() runtime))
             "Empty tool-calls should classify as nil")
    (format t "  ✓ classify nil/empty tool-calls returns nil~%")))

(defun test-classify-dual-phase-tool ()
  "file-read is valid in inspect+edit+verify but is classified by its envelope
   predicate as read-only (not by its valid-phases list)."
  (let ((runtime (%make-test-runtime-for-progression))
        (tool-calls '((:name "file-read" :id "t1" :input nil))))
    (let ((result (claw-lisp.core.phase-progression:classify-tool-calls-for-phase
                   tool-calls runtime)))
      (%assert (eq :read-only result)
               "file-read should classify as :read-only via envelope predicate, got ~A" result)
      (format t "  ✓ dual-phase tool classified by envelope predicate (read-only)~%"))))

(defun test-apply-progression-returns-three-nils-when-no-recommendation ()
  (let ((runtime (%make-test-runtime-for-progression))
        (session (%make-test-session))
        (tool-calls '((:name "file-read" :id "t1" :input nil))))
    ;; Put session in :complete (no valid transitions from :complete)
    (claw-lisp.core.phases:transition-phase session :inspect "s")
    (claw-lisp.core.phases:transition-phase session :edit "e")
    (claw-lisp.core.phases:transition-phase session :verify "v")
    (claw-lisp.core.phases:transition-phase session :complete "c")
    ;; Artificially set counters high to trigger recommendation
    (dotimes (i 10)
      (claw-lisp.core.phases:increment-phase-counter session :inspect))
    (multiple-value-bind (transitioned new-phase reason)
        (claw-lisp.core.phase-progression:apply-progression-policy
         session tool-calls runtime nil)
      (%assert (null transitioned) "Should not transition from :complete")
      (%assert (null new-phase) "New phase should be nil")
      (%assert (null reason) "Reason should be nil")
      (format t "  ✓ apply-progression-policy returns three nils (no recommendation path)~%"))))

(defun run-phase-progression-tests ()
  (format t "~%=== PHZ-003/004 Phase Progression Tests ===~%~%")
  (test-classify-read-only-tools)
  (test-classify-mutation-tools)
  (test-classify-mixed-tools)
  (test-classify-nil-tool-calls)
  (test-classify-dual-phase-tool)
  (test-recommend-edit-on-mutation-in-inspect)
  (test-recommend-edit-on-stagnation)
  (test-no-recommendation-below-threshold)
  (test-no-recommendation-with-nil-tool-calls)
  (test-recommend-verify-after-edit-stagnation)
  (test-apply-progression-policy-transitions)
  (test-apply-progression-policy-no-op)
  (test-apply-progression-returns-three-nils-when-no-recommendation)
  (format t "~%=== All PHZ-003/004 Tests Passed! ===~%"))

(in-package #:claw-lisp.tests)

;;; FND-001 Phase State Tracking Tests

(defun test-phase-state-initialization ()
  (let ((session (claw-lisp.core.domain:make-agent-session
                  :id "test-session-001" :provider :mock :model "test-model"
                  :conversation nil :state nil)))
    (claw-lisp.core.phases:initialize-phase-state session)
    (let ((counters (getf (claw-lisp.core.domain:agent-session-state session) :phase-counters)))
      (%assert (not (null counters)) "Phase counters should be initialized")
      (%assert (= 0 (getf counters :inspect)) "Inspect counter should be 0")
      (%assert (= 0 (getf counters :edit)) "Edit counter should be 0")
      (%assert (= 0 (getf counters :verify)) "Verify counter should be 0")
      (%assert (= 0 (getf counters :complete)) "Complete counter should be 0"))
    (let ((history (getf (claw-lisp.core.domain:agent-session-state session) :phase-history)))
      (%assert (listp history) "Phase history should be a list")
      (%assert (null history) "Phase history should be empty initially"))
    (%assert (= 0 (claw-lisp.core.phases:get-turn-count session)) "Turn count should be 0")
    (format t "  ✓ test-phase-state-initialization~%")))

(defun test-phase-transition-basic ()
  (let ((session (claw-lisp.core.domain:make-agent-session
                  :id "test-session-002" :provider :mock :model "test-model"
                  :conversation nil :state nil)))
    (claw-lisp.core.phases:initialize-phase-state session)
    (claw-lisp.core.phases:transition-phase session :inspect "starting session")
    (%assert (eq :inspect (claw-lisp.core.phases:get-current-phase session))
             "Current phase should be :inspect")
    (claw-lisp.core.phases:transition-phase session :edit "inspection complete")
    (%assert (eq :edit (claw-lisp.core.phases:get-current-phase session))
             "Current phase should be :edit")
    (claw-lisp.core.phases:transition-phase session :verify "edits done")
    (%assert (eq :verify (claw-lisp.core.phases:get-current-phase session))
             "Current phase should be :verify")
    (claw-lisp.core.phases:transition-phase session :complete "verify passed")
    (%assert (eq :complete (claw-lisp.core.phases:get-current-phase session))
             "Current phase should be :complete")
    (%assert (= 4 (claw-lisp.core.phases:phase-transition-count session))
             "Should have 4 transitions in history")
    (format t "  ✓ test-phase-transition-basic~%")))

(defun test-invalid-phase-transitions ()
  (let ((session (claw-lisp.core.domain:make-agent-session
                  :id "test-session-003" :provider :mock :model "test-model"
                  :conversation nil :state nil)))
    (claw-lisp.core.phases:initialize-phase-state session)
    (claw-lisp.core.phases:transition-phase session :inspect "start")
    ;; Invalid: inspect → verify
    (handler-case
        (progn
          (claw-lisp.core.phases:transition-phase session :verify "skip edit")
          (%assert nil "Should have signaled invalid-phase-transition"))
      (claw-lisp.core.conditions:invalid-phase-transition () nil))
    (%assert (eq :inspect (claw-lisp.core.phases:get-current-phase session))
             "Should still be in :inspect after failed transition")
    ;; Valid transitions
    (claw-lisp.core.phases:transition-phase session :edit "now edit")
    (claw-lisp.core.phases:transition-phase session :verify "now verify")
    (claw-lisp.core.phases:transition-phase session :edit "retry edit")
    (%assert (eq :edit (claw-lisp.core.phases:get-current-phase session))
             "Should allow retry from verify to edit")
    (claw-lisp.core.phases:transition-phase session :complete "done")
    ;; Invalid: complete → anything
    (handler-case
        (progn
          (claw-lisp.core.phases:transition-phase session :inspect "restart")
          (%assert nil "Should have signaled invalid-phase-transition"))
      (claw-lisp.core.conditions:invalid-phase-transition () nil))
    (format t "  ✓ test-invalid-phase-transitions~%")))

(defun test-phase-counters ()
  (let ((session (claw-lisp.core.domain:make-agent-session
                  :id "test-session-004" :provider :mock :model "test-model"
                  :conversation nil :state nil)))
    (claw-lisp.core.phases:initialize-phase-state session)
    (claw-lisp.core.phases:transition-phase session :inspect "start")
    (claw-lisp.core.phases:increment-phase-counter session :inspect)
    (claw-lisp.core.phases:increment-phase-counter session :inspect)
    (claw-lisp.core.phases:increment-phase-counter session :inspect)
    (%assert (= 3 (claw-lisp.core.phases:get-phase-counter session :inspect))
             "Inspect counter should be 3")
    (claw-lisp.core.phases:transition-phase session :edit "done inspecting")
    (claw-lisp.core.phases:increment-phase-counter session :edit)
    (%assert (= 1 (claw-lisp.core.phases:get-phase-counter session :edit))
             "Edit counter should be 1")
    (%assert (= 3 (claw-lisp.core.phases:get-phase-counter session :inspect))
             "Inspect counter should still be 3")
    (format t "  ✓ test-phase-counters~%")))

(defun test-phase-queries ()
  (let ((session (claw-lisp.core.domain:make-agent-session
                  :id "test-session-005" :provider :mock :model "test-model"
                  :conversation nil :state nil)))
    (claw-lisp.core.phases:initialize-phase-state session)
    (claw-lisp.core.phases:transition-phase session :inspect "start")
    (%assert (claw-lisp.core.phases:in-phase-p session :inspect) "Should be in :inspect")
    (%assert (not (claw-lisp.core.phases:in-phase-p session :edit)) "Should not be in :edit")
    (%assert (claw-lisp.core.phases:has-entered-phase-p session :inspect) "Should have entered :inspect")
    (%assert (not (claw-lisp.core.phases:has-entered-phase-p session :edit)) "Should not have entered :edit")
    (claw-lisp.core.phases:transition-phase session :edit "move to edit")
    (%assert (claw-lisp.core.phases:has-entered-phase-p session :inspect) "Should still have :inspect in history")
    (%assert (claw-lisp.core.phases:has-entered-phase-p session :edit) "Should now have entered :edit")
    (%assert (numberp (claw-lisp.core.phases:phase-duration-seconds session)) "Phase duration should be a number")
    (format t "  ✓ test-phase-queries~%")))

(defun test-phase-summary ()
  (let ((session (claw-lisp.core.domain:make-agent-session
                  :id "test-session-006" :provider :mock :model "test-model"
                  :conversation nil :state nil)))
    (claw-lisp.core.phases:initialize-phase-state session)
    (claw-lisp.core.phases:transition-phase session :inspect "start")
    (claw-lisp.core.phases:increment-phase-counter session :inspect)
    (claw-lisp.core.phases:increment-turn-count session)
    (let ((summary (claw-lisp.core.phases:phase-summary session)))
      (%assert (eq :inspect (getf summary :current-phase)) "Current phase should be :inspect")
      (%assert (= 1 (getf summary :turn-count)) "Turn count should be 1")
      (%assert (= 1 (getf summary :inspect-tool-count)) "Inspect tool count should be 1"))
    (format t "  ✓ test-phase-summary~%")))

(defun test-bounded-phase-history ()
  (let ((session (claw-lisp.core.domain:make-agent-session
                  :id "test-session-007" :provider :mock :model "test-model"
                  :conversation nil :state nil))
        (max-entries claw-lisp.core.phases:+max-phase-history-entries+))
    (claw-lisp.core.phases:initialize-phase-state session)
    (claw-lisp.core.phases:transition-phase session :inspect "start")
    (dotimes (i (+ max-entries 10))
      (let ((current (claw-lisp.core.phases:get-current-phase session)))
        (case current
          (:inspect (claw-lisp.core.phases:transition-phase session :edit (format nil "t-~A" i)))
          (:edit (claw-lisp.core.phases:transition-phase session :verify (format nil "t-~A" i)))
          (:verify (if (< i (+ max-entries 8))
                       (claw-lisp.core.phases:transition-phase session :edit (format nil "t-~A" i))
                       (claw-lisp.core.phases:transition-phase session :complete (format nil "t-~A" i))))
          (:complete (return)))))
    (let ((history (claw-lisp.core.phases:get-phase-history session)))
      (%assert (<= (length history) max-entries) "Phase history should not exceed max entries")
      (when (> (length history) 0)
        (%assert (eq (getf (first history) :phase) :complete)
                 "Most recent transition should be at head of history")
        (let ((has-start (find "start" history
                               :key (lambda (entry) (getf entry :trigger))
                               :test #'string=)))
          (%assert (null has-start) "Oldest transition should have been evicted"))))
    (format t "  ✓ test-bounded-phase-history~%")))

(defun test-transition-preserves-unrelated-state ()
  (let ((session (claw-lisp.core.domain:make-agent-session
                  :id "test-session-008" :provider :mock :model "test-model"
                  :conversation nil :state nil)))
    (claw-lisp.core.phases:initialize-phase-state session)
    (let ((state (claw-lisp.core.domain:agent-session-state session)))
      (setf (getf state :custom-sentinel-1) "preserved-value-1")
      (setf (getf state :custom-sentinel-2) 42)
      (setf (getf state :custom-sentinel-3) '(:nested :data))
      (setf (claw-lisp.core.domain:agent-session-state session) state))
    (claw-lisp.core.phases:transition-phase session :inspect "start")
    (claw-lisp.core.phases:transition-phase session :edit "found issue")
    (claw-lisp.core.phases:transition-phase session :verify "done editing")
    (let ((final-state (claw-lisp.core.domain:agent-session-state session)))
      (%assert (string= "preserved-value-1" (getf final-state :custom-sentinel-1))
               "String sentinel should be preserved")
      (%assert (= 42 (getf final-state :custom-sentinel-2))
               "Integer sentinel should be preserved")
      (%assert (equal '(:nested :data) (getf final-state :custom-sentinel-3))
               "List sentinel should be preserved"))
    (format t "  ✓ test-transition-preserves-unrelated-state~%")))

(defun run-phases-tests ()
  (format t "~%=== FND-001 Phase State Tests ===~%~%")
  (test-phase-state-initialization)
  (test-phase-transition-basic)
  (test-invalid-phase-transitions)
  (test-phase-counters)
  (test-phase-queries)
  (test-phase-summary)
  (test-bounded-phase-history)
  (test-transition-preserves-unrelated-state)
  (format t "~%=== All FND-001 Phase Tests Passed! ===~%"))

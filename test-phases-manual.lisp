(require :asdf)
(load "/root/quicklisp/setup.lisp")
(push #P"/workspace/" asdf:*central-registry*)
(asdf:load-system :claw-lisp)

(in-package #:claw-lisp.core.phases)

(format t "~%~%=== FND-001 Phase State Tracking Manual Test ===~%~%")

;; Test 1: Initialize phase state
(format t "Test 1: Initialize phase state...~%")
(let ((session (claw-lisp.core.domain:make-agent-session
                :id "test-001"
                :provider :mock
                :model "test-model"
                :conversation nil
                :state nil)))
  (initialize-phase-state session)
  (let ((state (claw-lisp.core.domain:agent-session-state session)))
    (assert (getf state :phase-counters) nil "Phase counters should be initialized")
    (assert (= 0 (getf (getf state :phase-counters) :inspect)) nil "Inspect counter should be 0")
    (assert (listp (getf state :phase-history)) nil "Phase history should be a list")
    (assert (null (getf state :phase-history)) nil "Phase history should be empty")
    (format t "  ✓ Phase state initialized correctly~%")))

;; Test 2: Basic phase transitions
(format t "~%Test 2: Basic phase transitions...~%")
(let ((session (claw-lisp.core.domain:make-agent-session
                :id "test-002"
                :provider :mock
                :model "test-model"
                :conversation nil
                :state nil)))
  (initialize-phase-state session)
  (transition-phase session :inspect "start")
  (assert (eq :inspect (get-current-phase session)) nil "Should be in inspect phase")
  (format t "  ✓ Transitioned to :inspect~%")

  (transition-phase session :edit "found issues")
  (assert (eq :edit (get-current-phase session)) nil "Should be in edit phase")
  (format t "  ✓ Transitioned to :edit~%")

  (transition-phase session :verify "done editing")
  (assert (eq :verify (get-current-phase session)) nil "Should be in verify phase")
  (format t "  ✓ Transitioned to :verify~%")

  (transition-phase session :complete "all done")
  (assert (eq :complete (get-current-phase session)) nil "Should be in complete phase")
  (format t "  ✓ Transitioned to :complete~%")

  (assert (= 4 (phase-transition-count session)) nil "Should have 4 transitions")
  (format t "  ✓ Transition history tracked correctly~%"))

;; Test 3: Invalid transitions
(format t "~%Test 3: Invalid phase transitions...~%")
(let ((session (claw-lisp.core.domain:make-agent-session
                :id "test-003"
                :provider :mock
                :model "test-model"
                :conversation nil
                :state nil)))
  (initialize-phase-state session)
  (transition-phase session :inspect "start")

  ;; Try invalid: inspect -> verify (should skip edit)
  (handler-case
      (progn
        (transition-phase session :verify "skip edit")
        (error "Should have signaled invalid-phase-transition"))
    (claw-lisp.core.conditions:invalid-phase-transition (c)
      (format t "  ✓ Correctly rejected :inspect -> :verify transition~%")))

  ;; Should still be in inspect
  (assert (eq :inspect (get-current-phase session)) nil "Should still be in inspect")
  (format t "  ✓ Phase unchanged after invalid transition~%")

  ;; Valid: inspect -> edit
  (transition-phase session :edit "now edit")
  ;; Valid: edit -> verify
  (transition-phase session :verify "now verify")
  ;; Valid: verify -> edit (retry)
  (transition-phase session :edit "retry")
  (assert (eq :edit (get-current-phase session)) nil "Should allow retry")
  (format t "  ✓ :verify -> :edit retry allowed~%")

  ;; Complete
  (transition-phase session :complete "done")

  ;; Try transition from complete
  (handler-case
      (progn
        (transition-phase session :inspect "restart")
        (error "Should have signaled invalid-phase-transition"))
    (claw-lisp.core.conditions:invalid-phase-transition (c)
      (format t "  ✓ Correctly rejected transition from :complete~%"))))

;; Test 4: Phase counters
(format t "~%Test 4: Phase counters...~%")
(let ((session (claw-lisp.core.domain:make-agent-session
                :id "test-004"
                :provider :mock
                :model "test-model"
                :conversation nil
                :state nil)))
  (initialize-phase-state session)
  (transition-phase session :inspect "start")

  (increment-phase-counter session :inspect)
  (increment-phase-counter session :inspect)
  (increment-phase-counter session :inspect)
  (assert (= 3 (get-phase-counter session :inspect)) nil "Inspect counter should be 3")
  (format t "  ✓ Phase counter incremented correctly~%"))

;; Test 5: Phase queries
(format t "~%Test 5: Phase queries...~%")
(let ((session (claw-lisp.core.domain:make-agent-session
                :id "test-005"
                :provider :mock
                :model "test-model"
                :conversation nil
                :state nil)))
  (initialize-phase-state session)
  (transition-phase session :inspect "start")

  (assert (in-phase-p session :inspect) nil "Should be in :inspect")
  (assert (not (in-phase-p session :edit)) nil "Should not be in :edit")
  (format t "  ✓ in-phase-p works correctly~%")

  (assert (has-entered-phase-p session :inspect) nil "Should have entered :inspect")
  (assert (not (has-entered-phase-p session :edit)) nil "Should not have entered :edit yet")
  (format t "  ✓ has-entered-phase-p works correctly~%")

  (transition-phase session :edit "move on")
  (assert (has-entered-phase-p session :inspect) nil "Should still have :inspect in history")
  (assert (has-entered-phase-p session :edit) nil "Should now have entered :edit")
  (format t "  ✓ Phase history tracking works~%")

  (let ((duration (phase-duration-seconds session)))
    (assert (numberp duration) nil "Duration should be a number")
    (format t "  ✓ Phase duration: ~A seconds~%" duration)))

;; Test 6: Phase summary
(format t "~%Test 6: Phase summary...~%")
(let ((session (claw-lisp.core.domain:make-agent-session
                :id "test-006"
                :provider :mock
                :model "test-model"
                :conversation nil
                :state nil)))
  (initialize-phase-state session)
  (transition-phase session :inspect "start")
  (increment-phase-counter session :inspect)
  (increment-turn-count session)

  (let ((summary (phase-summary session)))
    (assert (eq :inspect (getf summary :current-phase)) nil "Current phase should be :inspect")
    (assert (= 1 (getf summary :turn-count)) nil "Turn count should be 1")
    (assert (= 1 (getf summary :inspect-tool-count)) nil "Inspect tool count should be 1")
    (format t "  ✓ Phase summary complete and accurate~%")))

;; Test 7: Bounded history
(format t "~%Test 7: Bounded phase history...~%")
(let ((session (claw-lisp.core.domain:make-agent-session
                :id "test-007"
                :provider :mock
                :model "test-model"
                :conversation nil
                :state nil))
      (max-entries claw-lisp.core.phases:+max-phase-history-entries+))
  (initialize-phase-state session)

  ;; Create more transitions than max, cycling through valid transitions
  ;; inspect -> edit -> verify -> edit -> verify -> ... -> complete
  (transition-phase session :inspect "start")
  (dotimes (i (+ max-entries 10))
    (let ((current (get-current-phase session)))
      (case current
        (:inspect (transition-phase session :edit (format nil "transition-~A" i)))
        (:edit (transition-phase session :verify (format nil "transition-~A" i)))
        (:verify (if (< i (+ max-entries 8))
                     (transition-phase session :edit (format nil "transition-~A" i))
                     (transition-phase session :complete (format nil "transition-~A" i))))
        (:complete (return)))))

  (let ((history (get-phase-history session)))
    (assert (<= (length history) max-entries) nil "History should be bounded")
    (format t "  ✓ Phase history bounded to max ~A entries (actual: ~A)~%"
            max-entries (length history))))

(format t "~%~%=== All FND-001 Tests Passed! ===~%~%")
(uiop:quit 0)

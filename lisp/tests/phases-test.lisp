(in-package #:claw-lisp.tests)

;;; FND-001 Phase State Tracking Tests

(def-test test-phase-state-initialization ()
  "Test that phase state can be initialized in a session."
  (let ((session (claw-lisp.core.domain:make-agent-session
                  :id "test-session-001"
                  :provider :mock
                  :model "test-model"
                  :conversation nil
                  :state nil)))

    ;; Initialize phase state
    (claw-lisp.core.phases:initialize-phase-state session)

    ;; Check phase counters exist
    (let ((counters (getf (claw-lisp.core.domain:agent-session-state session) :phase-counters)))
      (is (not (null counters)) "Phase counters should be initialized")
      (is (= 0 (getf counters :inspect)) "Inspect counter should be 0")
      (is (= 0 (getf counters :edit)) "Edit counter should be 0")
      (is (= 0 (getf counters :verify)) "Verify counter should be 0")
      (is (= 0 (getf counters :complete)) "Complete counter should be 0"))

    ;; Check phase history exists
    (let ((history (getf (claw-lisp.core.domain:agent-session-state session) :phase-history)))
      (is (listp history) "Phase history should be a list")
      (is (null history) "Phase history should be empty initially"))

    ;; Check turn count
    (is (= 0 (claw-lisp.core.phases:get-turn-count session)) "Turn count should be 0")))

(def-test test-phase-transition-basic ()
  "Test basic phase transitions work correctly."
  (let ((session (claw-lisp.core.domain:make-agent-session
                  :id "test-session-002"
                  :provider :mock
                  :model "test-model"
                  :conversation nil
                  :state nil)))

    (claw-lisp.core.phases:initialize-phase-state session)

    ;; Transition to inspect
    (claw-lisp.core.phases:transition-phase session :inspect "starting session")
    (is (eq :inspect (claw-lisp.core.phases:get-current-phase session))
        "Current phase should be :inspect")

    ;; Transition to edit
    (claw-lisp.core.phases:transition-phase session :edit "inspection complete")
    (is (eq :edit (claw-lisp.core.phases:get-current-phase session))
        "Current phase should be :edit")

    ;; Transition to verify
    (claw-lisp.core.phases:transition-phase session :verify "edits done")
    (is (eq :verify (claw-lisp.core.phases:get-current-phase session))
        "Current phase should be :verify")

    ;; Transition to complete
    (claw-lisp.core.phases:transition-phase session :complete "verify passed")
    (is (eq :complete (claw-lisp.core.phases:get-current-phase session))
        "Current phase should be :complete")

    ;; Check history length
    (is (= 4 (claw-lisp.core.phases:phase-transition-count session))
        "Should have 4 transitions in history")))

(def-test test-invalid-phase-transitions ()
  "Test that invalid phase transitions are rejected."
  (let ((session (claw-lisp.core.domain:make-agent-session
                  :id "test-session-003"
                  :provider :mock
                  :model "test-model"
                  :conversation nil
                  :state nil)))

    (claw-lisp.core.phases:initialize-phase-state session)

    ;; Valid: NIL → inspect
    (claw-lisp.core.phases:transition-phase session :inspect "start")

    ;; Invalid: inspect → verify (skipping edit)
    (signals claw-lisp.core.conditions:invalid-phase-transition
      (claw-lisp.core.phases:transition-phase session :verify "skip edit"))

    ;; Still in inspect after failed transition
    (is (eq :inspect (claw-lisp.core.phases:get-current-phase session))
        "Should still be in :inspect after failed transition")

    ;; Valid: inspect → edit
    (claw-lisp.core.phases:transition-phase session :edit "now edit")

    ;; Valid: edit → verify
    (claw-lisp.core.phases:transition-phase session :verify "now verify")

    ;; Valid: verify → edit (retry)
    (claw-lisp.core.phases:transition-phase session :edit "retry edit")
    (is (eq :edit (claw-lisp.core.phases:get-current-phase session))
        "Should allow retry from verify to edit")

    ;; Valid: edit → complete
    (claw-lisp.core.phases:transition-phase session :complete "done")

    ;; Invalid: complete → anything
    (signals claw-lisp.core.conditions:invalid-phase-transition
      (claw-lisp.core.phases:transition-phase session :inspect "restart"))))

(def-test test-phase-counters ()
  "Test that phase counters increment correctly."
  (let ((session (claw-lisp.core.domain:make-agent-session
                  :id "test-session-004"
                  :provider :mock
                  :model "test-model"
                  :conversation nil
                  :state nil)))

    (claw-lisp.core.phases:initialize-phase-state session)
    (claw-lisp.core.phases:transition-phase session :inspect "start")

    ;; Increment inspect counter
    (claw-lisp.core.phases:increment-phase-counter session :inspect)
    (claw-lisp.core.phases:increment-phase-counter session :inspect)
    (claw-lisp.core.phases:increment-phase-counter session :inspect)

    (is (= 3 (claw-lisp.core.phases:get-phase-counter session :inspect))
        "Inspect counter should be 3")

    ;; Transition to edit
    (claw-lisp.core.phases:transition-phase session :edit "done inspecting")

    ;; Increment edit counter
    (claw-lisp.core.phases:increment-phase-counter session :edit)

    (is (= 1 (claw-lisp.core.phases:get-phase-counter session :edit))
        "Edit counter should be 1")
    (is (= 3 (claw-lisp.core.phases:get-phase-counter session :inspect))
        "Inspect counter should still be 3")))

(def-test test-phase-queries ()
  "Test phase query functions."
  (let ((session (claw-lisp.core.domain:make-agent-session
                  :id "test-session-005"
                  :provider :mock
                  :model "test-model"
                  :conversation nil
                  :state nil)))

    (claw-lisp.core.phases:initialize-phase-state session)
    (claw-lisp.core.phases:transition-phase session :inspect "start")

    ;; Test in-phase-p
    (is (claw-lisp.core.phases:in-phase-p session :inspect)
        "Should be in :inspect phase")
    (is (not (claw-lisp.core.phases:in-phase-p session :edit))
        "Should not be in :edit phase")

    ;; Test has-entered-phase-p
    (is (claw-lisp.core.phases:has-entered-phase-p session :inspect)
        "Should have entered :inspect phase")
    (is (not (claw-lisp.core.phases:has-entered-phase-p session :edit))
        "Should not have entered :edit phase yet")

    ;; Transition to edit
    (claw-lisp.core.phases:transition-phase session :edit "move to edit")

    ;; Now both phases should show as entered
    (is (claw-lisp.core.phases:has-entered-phase-p session :inspect)
        "Should still have :inspect in history")
    (is (claw-lisp.core.phases:has-entered-phase-p session :edit)
        "Should now have entered :edit phase")

    ;; Test phase-duration-seconds (just check it returns a number)
    (is (numberp (claw-lisp.core.phases:phase-duration-seconds session))
        "Phase duration should be a number")))

(def-test test-phase-summary ()
  "Test that phase-summary returns complete state."
  (let ((session (claw-lisp.core.domain:make-agent-session
                  :id "test-session-006"
                  :provider :mock
                  :model "test-model"
                  :conversation nil
                  :state nil)))

    (claw-lisp.core.phases:initialize-phase-state session)
    (claw-lisp.core.phases:transition-phase session :inspect "start")
    (claw-lisp.core.phases:increment-phase-counter session :inspect)
    (claw-lisp.core.phases:increment-turn-count session)

    (let ((summary (claw-lisp.core.phases:phase-summary session)))
      (is (getf summary :current-phase) "Summary should include current-phase")
      (is (eq :inspect (getf summary :current-phase)) "Current phase should be :inspect")
      (is (= 1 (getf summary :turn-count)) "Turn count should be 1")
      (is (= 1 (getf summary :inspect-tool-count)) "Inspect tool count should be 1"))))

(def-test test-bounded-phase-history ()
  "Test that phase history is bounded to prevent unbounded growth."
  (let ((session (claw-lisp.core.domain:make-agent-session
                  :id "test-session-007"
                  :provider :mock
                  :model "test-model"
                  :conversation nil
                  :state nil))
        (max-entries claw-lisp.core.phases:+max-phase-history-entries+))

    (claw-lisp.core.phases:initialize-phase-state session)

    ;; Create more transitions than the max using valid transitions
    ;; inspect -> edit -> verify -> edit -> verify -> ... -> complete
    (claw-lisp.core.phases:transition-phase session :inspect "start")
    (dotimes (i (+ max-entries 10))
      (let ((current (claw-lisp.core.phases:get-current-phase session)))
        (case current
          (:inspect (claw-lisp.core.phases:transition-phase session :edit (format nil "transition-~A" i)))
          (:edit (claw-lisp.core.phases:transition-phase session :verify (format nil "transition-~A" i)))
          (:verify (if (< i (+ max-entries 8))
                       (claw-lisp.core.phases:transition-phase session :edit (format nil "transition-~A" i))
                       (claw-lisp.core.phases:transition-phase session :complete (format nil "transition-~A" i))))
          (:complete (return)))))

    ;; History should be capped at max
    (let ((history (claw-lisp.core.phases:get-phase-history session)))
      (is (<= (length history) max-entries)
          "Phase history should not exceed max entries"))))

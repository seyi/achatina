(in-package #:claw-lisp.tests)

;;; FND-002 Completion Detection Tests

(def-test test-coding-task-complete-p-verify-passed ()
  "Test that coding-task-complete-p returns T when verify passed."
  (let ((session (claw-lisp.core.domain:make-agent-session
                  :id "test-completion-001"
                  :provider :mock
                  :model "test-model"
                  :conversation nil
                  :state nil)))

    (claw-lisp.core.phases:initialize-phase-state session)
    (claw-lisp.core.phases:transition-phase session :inspect "start")
    (claw-lisp.core.phases:transition-phase session :edit "found issue")
    (claw-lisp.core.phases:transition-phase session :verify "done editing")
    (claw-lisp.core.phases:set-last-verify-result session t)
    (claw-lisp.core.phases:transition-phase session :complete "verify passed")

    (is (claw-lisp.core.completion:coding-task-complete-p session)
        "Should be complete when verify passed and in :complete phase")))

(def-test test-coding-task-complete-p-no-tool-calls ()
  "Test that coding-task-complete-p returns T when no tool calls in last turn."
  (let ((session (claw-lisp.core.domain:make-agent-session
                  :id "test-completion-002"
                  :provider :mock
                  :model "test-model"
                  :conversation nil
                  :state nil)))

    (claw-lisp.core.phases:initialize-phase-state session)
    (claw-lisp.core.phases:transition-phase session :inspect "start")
    (claw-lisp.core.phases:transition-phase session :edit "found issue")
    (claw-lisp.core.phases:transition-phase session :verify "done editing")
    (claw-lisp.core.phases:set-last-turn-tool-count session 0)
    (claw-lisp.core.phases:transition-phase session :complete "model finished")

    (is (claw-lisp.core.completion:coding-task-complete-p session)
        "Should be complete when no tool calls and in :complete phase")))

(def-test test-coding-task-complete-p-not-complete-phase ()
  "Test that coding-task-complete-p returns NIL when not in :complete phase."
  (let ((session (claw-lisp.core.domain:make-agent-session
                  :id "test-completion-003"
                  :provider :mock
                  :model "test-model"
                  :conversation nil
                  :state nil)))

    (claw-lisp.core.phases:initialize-phase-state session)
    (claw-lisp.core.phases:transition-phase session :inspect "start")
    (claw-lisp.core.phases:set-last-verify-result session t)

    (is (not (claw-lisp.core.completion:coding-task-complete-p session))
        "Should not be complete when in :inspect phase")))

(def-test test-has-text-content-p ()
  "Test detection of text content in responses."
  ;; Response with text block
  (let ((response-with-text '(:content ((:type :text :text "Hello world")))))
    (is (claw-lisp.core.completion:has-text-content-p response-with-text)
        "Should detect text content"))

  ;; Response without text block
  (let ((response-no-text '(:content ((:type :tool_use :id "1" :name "read")))))
    (is (not (claw-lisp.core.completion:has-text-content-p response-no-text))
        "Should not detect text when only tool uses"))

  ;; NIL response
  (is (not (claw-lisp.core.completion:has-text-content-p nil))
      "Should handle nil response"))

(def-test test-has-tool-calls-p ()
  "Test detection of tool calls in responses."
  ;; Response with tool use
  (let ((response-with-tool '(:content ((:type :tool_use :id "1" :name "read")))))
    (is (claw-lisp.core.completion:has-tool-calls-p response-with-tool)
        "Should detect tool calls"))

  ;; Response without tool use
  (let ((response-no-tool '(:content ((:type :text :text "Hello")))))
    (is (not (claw-lisp.core.completion:has-tool-calls-p response-no-tool))
        "Should not detect tools when only text"))

  ;; NIL response
  (is (not (claw-lisp.core.completion:has-tool-calls-p nil))
      "Should handle nil response"))

(def-test test-transition-to-complete ()
  "Test transition-to-complete function."
  (let ((session (claw-lisp.core.domain:make-agent-session
                  :id "test-completion-004"
                  :provider :mock
                  :model "test-model"
                  :conversation nil
                  :state nil)))

    (claw-lisp.core.phases:initialize-phase-state session)
    (claw-lisp.core.phases:transition-phase session :inspect "start")
    (claw-lisp.core.phases:transition-phase session :edit "found issue")
    (claw-lisp.core.phases:transition-phase session :verify "done editing")

    ;; Transition to complete
    (claw-lisp.core.completion:transition-to-complete session "test-reason")

    (is (eq :complete (claw-lisp.core.phases:get-current-phase session))
        "Should be in :complete phase")

    ;; Check history
    (let ((history (claw-lisp.core.phases:get-phase-history session)))
      (is (> (length history) 0) "Should have history")
      (let ((recent (first history)))
        (is (eq :complete (getf recent :phase))
            "Most recent transition should be to :complete")
        (is (string= "test-reason" (getf recent :trigger))
            "Reason should be recorded in history")))))

(def-test test-check-completion-triggers-verify-passed ()
  "Test completion trigger: verify passed."
  (let ((session (claw-lisp.core.domain:make-agent-session
                  :id "test-completion-005"
                  :provider :mock
                  :model "test-model"
                  :conversation nil
                  :state nil)))

    (claw-lisp.core.phases:initialize-phase-state session)
    (claw-lisp.core.phases:transition-phase session :verify "checking")
    (claw-lisp.core.phases:set-last-verify-result session t)

    (multiple-value-bind (triggered reason)
        (claw-lisp.core.completion:check-completion-triggers session nil)
      (is triggered "Trigger should fire when verify passed")
      (is (string= "verify-passed" reason)
          "Reason should be verify-passed"))))

(def-test test-check-completion-triggers-model-confirmed ()
  "Test completion trigger: model confirmed completion with text."
  (let ((session (claw-lisp.core.domain:make-agent-session
                  :id "test-completion-006"
                  :provider :mock
                  :model "test-model"
                  :conversation nil
                  :state nil))
        (response '(:content ((:type :text :text "Task complete")))))

    (claw-lisp.core.phases:initialize-phase-state session)
    (claw-lisp.core.phases:transition-phase session :verify "checking")

    (multiple-value-bind (triggered reason)
        (claw-lisp.core.completion:check-completion-triggers session response)
      (is triggered "Trigger should fire when model confirms with text")
      (is (string= "model-confirmed-completion" reason)
          "Reason should be model-confirmed-completion"))))

(def-test test-check-completion-triggers-max-iterations ()
  "Test completion trigger: max iterations exceeded."
  (let ((session (claw-lisp.core.domain:make-agent-session
                  :id "test-completion-007"
                  :provider :mock
                  :model "test-model"
                  :conversation nil
                  :state nil))
        (max-iters claw-lisp.core.completion:+max-coding-task-iterations+))

    (claw-lisp.core.phases:initialize-phase-state session)
    (claw-lisp.core.phases:transition-phase session :inspect "start")

    ;; Simulate many turns
    (dotimes (i (+ max-iters 1))
      (claw-lisp.core.phases:increment-turn-count session))

    (multiple-value-bind (triggered reason)
        (claw-lisp.core.completion:check-completion-triggers session nil)
      (is triggered "Trigger should fire when max iterations exceeded")
      (is (string= "max-iterations" reason)
          "Reason should be max-iterations"))))

(def-test test-check-completion-triggers-no-trigger ()
  "Test that no trigger fires when conditions not met."
  (let ((session (claw-lisp.core.domain:make-agent-session
                  :id "test-completion-008"
                  :provider :mock
                  :model "test-model"
                  :conversation nil
                  :state nil))
        (response '(:content ((:type :tool_use :id "1" :name "read")))))

    (claw-lisp.core.phases:initialize-phase-state session)
    (claw-lisp.core.phases:transition-phase session :inspect "start")

    (multiple-value-bind (triggered reason)
        (claw-lisp.core.completion:check-completion-triggers session response)
      (is (not triggered) "No trigger should fire")
      (is (null reason) "Reason should be nil"))))

(def-test test-maybe-auto-complete ()
  "Test maybe-auto-complete transitions when trigger fires."
  (let ((session (claw-lisp.core.domain:make-agent-session
                  :id "test-completion-009"
                  :provider :mock
                  :model "test-model"
                  :conversation nil
                  :state nil)))

    (claw-lisp.core.phases:initialize-phase-state session)
    (claw-lisp.core.phases:transition-phase session :verify "checking")
    (claw-lisp.core.phases:set-last-verify-result session t)

    (multiple-value-bind (completed reason)
        (claw-lisp.core.completion:maybe-auto-complete session nil)
      (is completed "Should complete when trigger fires")
      (is (string= "verify-passed" reason) "Should return reason")
      (is (eq :complete (claw-lisp.core.phases:get-current-phase session))
          "Session should be in :complete phase"))))

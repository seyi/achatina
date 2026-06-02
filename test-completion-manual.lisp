(require :asdf)
(load "/root/quicklisp/setup.lisp")
(push #P"/workspace/" asdf:*central-registry*)
(asdf:load-system :claw-lisp)

(in-package #:claw-lisp.core.completion)

(format t "~%~%=== FND-002 Completion Detection Manual Tests ===~%~%")

;; Test 1: coding-task-complete-p with verify passed
(format t "Test 1: Completion predicate with verify passed...~%")
(let ((session (claw-lisp.core.domain:make-agent-session
                :id "test-001"
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

  (assert (coding-task-complete-p session) nil "Should be complete")
  (format t "  ✓ Task complete when verify passed~%"))

;; Test 2: coding-task-complete-p with no tool calls
(format t "~%Test 2: Completion predicate with no tool calls...~%")
(let ((session (claw-lisp.core.domain:make-agent-session
                :id "test-002"
                :provider :mock
                :model "test-model"
                :conversation nil
                :state nil)))
  (claw-lisp.core.phases:initialize-phase-state session)
  (claw-lisp.core.phases:transition-phase session :inspect "start")
  (claw-lisp.core.phases:transition-phase session :edit "found issue")
  (claw-lisp.core.phases:set-last-turn-tool-count session 0)
  (claw-lisp.core.phases:transition-phase session :complete "model finished")

  (assert (coding-task-complete-p session) nil "Should be complete")
  (format t "  ✓ Task complete when no tool calls~%"))

;; Test 3: Not complete when not in complete phase
(format t "~%Test 3: Not complete when in other phases...~%")
(let ((session (claw-lisp.core.domain:make-agent-session
                :id "test-003"
                :provider :mock
                :model "test-model"
                :conversation nil
                :state nil)))
  (claw-lisp.core.phases:initialize-phase-state session)
  (claw-lisp.core.phases:transition-phase session :inspect "start")
  (claw-lisp.core.phases:set-last-verify-result session t)

  (assert (not (coding-task-complete-p session)) nil "Should not be complete")
  (format t "  ✓ Not complete in :inspect phase~%"))

;; Test 4: has-text-content-p
(format t "~%Test 4: Text content detection...~%")
(let ((response-with-text '(:content ((:type :text :text "Hello world"))))
      (response-no-text '(:content ((:type :tool_use :id "1" :name "read")))))
  (assert (has-text-content-p response-with-text) nil "Should detect text")
  (assert (not (has-text-content-p response-no-text)) nil "Should not detect text")
  (assert (not (has-text-content-p nil)) nil "Should handle nil")
  (format t "  ✓ Text content detection works~%"))

;; Test 5: has-tool-calls-p
(format t "~%Test 5: Tool call detection...~%")
(let ((response-with-tool '(:content ((:type :tool_use :id "1" :name "read"))))
      (response-no-tool '(:content ((:type :text :text "Hello")))))
  (assert (has-tool-calls-p response-with-tool) nil "Should detect tool")
  (assert (not (has-tool-calls-p response-no-tool)) nil "Should not detect tool")
  (assert (not (has-tool-calls-p nil)) nil "Should handle nil")
  (format t "  ✓ Tool call detection works~%"))

;; Test 6: transition-to-complete
(format t "~%Test 6: Transition to complete...~%")
(let ((session (claw-lisp.core.domain:make-agent-session
                :id "test-004"
                :provider :mock
                :model "test-model"
                :conversation nil
                :state nil)))
  (claw-lisp.core.phases:initialize-phase-state session)
  (claw-lisp.core.phases:transition-phase session :verify "checking")

  (transition-to-complete session "test-reason")

  (assert (eq :complete (claw-lisp.core.phases:get-current-phase session))
          nil "Should be complete")

  (let ((history (claw-lisp.core.phases:get-phase-history session)))
    (assert (> (length history) 0) nil "Should have history")
    (let ((recent (first history)))
      (assert (eq :complete (getf recent :phase)) nil "Should be :complete")
      (assert (string= "test-reason" (getf recent :trigger)) nil "Should have reason")
      (format t "  ✓ Transition to complete works~%"))))

;; Test 7: Completion trigger - verify passed
(format t "~%Test 7: Completion trigger - verify passed...~%")
(let ((session (claw-lisp.core.domain:make-agent-session
                :id "test-005"
                :provider :mock
                :model "test-model"
                :conversation nil
                :state nil)))
  (claw-lisp.core.phases:initialize-phase-state session)
  (claw-lisp.core.phases:transition-phase session :verify "checking")
  (claw-lisp.core.phases:set-last-verify-result session t)

  (multiple-value-bind (triggered reason)
      (check-completion-triggers session nil)
    (assert triggered nil "Should trigger")
    (assert (string= "verify-passed" reason) nil "Reason should be verify-passed")
    (format t "  ✓ Verify passed trigger works~%")))

;; Test 8: Completion trigger - model confirmed
(format t "~%Test 8: Completion trigger - model confirmed...~%")
(let ((session (claw-lisp.core.domain:make-agent-session
                :id "test-006"
                :provider :mock
                :model "test-model"
                :conversation nil
                :state nil))
      (response '(:content ((:type :text :text "Task complete")))))
  (claw-lisp.core.phases:initialize-phase-state session)
  (claw-lisp.core.phases:transition-phase session :verify "checking")

  (multiple-value-bind (triggered reason)
      (check-completion-triggers session response)
    (assert triggered nil "Should trigger")
    (assert (string= "model-confirmed-completion" reason) nil "Reason should be model-confirmed")
    (format t "  ✓ Model confirmed trigger works~%")))

;; Test 9: Completion trigger - max iterations
(format t "~%Test 9: Completion trigger - max iterations...~%")
(let ((session (claw-lisp.core.domain:make-agent-session
                :id "test-007"
                :provider :mock
                :model "test-model"
                :conversation nil
                :state nil))
      (max-iters +max-coding-task-iterations+))
  (claw-lisp.core.phases:initialize-phase-state session)
  (claw-lisp.core.phases:transition-phase session :inspect "start")

  (dotimes (i (+ max-iters 1))
    (claw-lisp.core.phases:increment-turn-count session))

  (multiple-value-bind (triggered reason)
      (check-completion-triggers session nil)
    (assert triggered nil "Should trigger")
    (assert (string= "max-iterations" reason) nil "Reason should be max-iterations")
    (format t "  ✓ Max iterations trigger works (limit: ~A)~%" max-iters)))

;; Test 10: No trigger fires
(format t "~%Test 10: No trigger when conditions not met...~%")
(let ((session (claw-lisp.core.domain:make-agent-session
                :id "test-008"
                :provider :mock
                :model "test-model"
                :conversation nil
                :state nil))
      (response '(:content ((:type :tool_use :id "1" :name "read")))))
  (claw-lisp.core.phases:initialize-phase-state session)
  (claw-lisp.core.phases:transition-phase session :inspect "start")

  (multiple-value-bind (triggered reason)
      (check-completion-triggers session response)
    (assert (not triggered) nil "Should not trigger")
    (assert (null reason) nil "Reason should be nil")
    (format t "  ✓ No trigger fires correctly~%")))

;; Test 11: maybe-auto-complete
(format t "~%Test 11: Auto-completion...~%")
(let ((session (claw-lisp.core.domain:make-agent-session
                :id "test-009"
                :provider :mock
                :model "test-model"
                :conversation nil
                :state nil)))
  (claw-lisp.core.phases:initialize-phase-state session)
  (claw-lisp.core.phases:transition-phase session :verify "checking")
  (claw-lisp.core.phases:set-last-verify-result session t)

  (multiple-value-bind (completed reason)
      (maybe-auto-complete session nil)
    (assert completed nil "Should complete")
    (assert (string= "verify-passed" reason) nil "Reason should be verify-passed")
    (assert (eq :complete (claw-lisp.core.phases:get-current-phase session))
            nil "Should be in :complete phase")
    (format t "  ✓ Auto-completion works~%")))

(format t "~%~%=== All FND-002 Tests Passed! ===~%~%")
(uiop:quit 0)

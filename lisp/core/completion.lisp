(in-package #:claw-lisp.core.completion)

;;; ============================================================
;;; Completion Detection for Coding CLI
;;; ============================================================
;;;
;;; This module implements completion detection for coding tasks:
;;;   - Determine when a coding task is complete
;;;   - Transition to :complete phase with reason
;;;   - Check completion triggers after each turn
;;;
;;; Completion criteria:
;;;   1. Session is in :complete phase
;;;   2. Last verify result passed OR no tool calls in last turn
;;;   3. Not exceeded max iterations

(defconstant +max-coding-task-iterations+ 20
  "Maximum number of turns before forcing completion.
   Prevents infinite loops in coding tasks.")

;;; --- Completion Predicates ---

(defun coding-task-complete-p (session)
  "Return T if coding task is complete, NIL otherwise.

   A task is complete when:
   - Session is in :complete phase AND
   - (Verify passed OR model finished naturally with no tool calls)"
  (let ((phase (claw-lisp.core.phases:get-current-phase session)))
    (when (eq phase :complete)
      (let ((verify-passed (claw-lisp.core.phases:get-last-verify-result session))
            (last-turn-tools (claw-lisp.core.phases:get-last-turn-tool-count session)))
        (or verify-passed
            (zerop last-turn-tools))))))

(defun has-text-content-p (response)
  "Return T if RESPONSE contains text content blocks."
  (when response
    (let ((content (getf response :content)))
      (and content
           (listp content)
           (some (lambda (block)
                   (and (consp block)
                        (eq :text (getf block :type))))
                 content)))))

(defun has-tool-calls-p (response)
  "Return T if RESPONSE contains tool use blocks."
  (when response
    (let ((content (getf response :content)))
      (and content
           (listp content)
           (some (lambda (block)
                   (and (consp block)
                        (eq :tool_use (getf block :type))))
                 content)))))

;;; --- Completion Transition ---

(defun transition-to-complete (session reason)
  "Transition SESSION to :complete phase with REASON.

   Records completion in phase history and returns session."
  (claw-lisp.core.phases:transition-phase session :complete reason)
  session)

;;; --- Completion Triggers ---

(defun check-completion-triggers (session last-response)
  "Check if any completion trigger is met after LAST-RESPONSE.

   Triggers:
   1. Verify phase + verify passed
   2. Verify phase + text response with no tool calls (model confirmed)
   3. Max iterations exceeded

   Returns (values triggered-p reason) where triggered-p is T if a trigger
   fired and reason is the completion reason string."
  (let ((phase (claw-lisp.core.phases:get-current-phase session))
        (verify-passed (claw-lisp.core.phases:get-last-verify-result session))
        (turn-count (claw-lisp.core.phases:get-turn-count session)))

    (cond
      ;; Trigger 1: Verify passed
      ((and (eq phase :verify) verify-passed)
       (values t "verify-passed"))

      ;; Trigger 2: Model confirmed completion (text with no tools after verify)
      ((and (eq phase :verify)
            (has-text-content-p last-response)
            (not (has-tool-calls-p last-response)))
       (values t "model-confirmed-completion"))

      ;; Trigger 3: Max iterations reached
      ((>= turn-count +max-coding-task-iterations+)
       (values t "max-iterations"))

      ;; No trigger
      (t (values nil nil)))))

(defun maybe-auto-complete (session last-response)
  "Check completion triggers and auto-transition to :complete if met.

   Returns (values completed-p reason) where completed-p is T if session
   was transitioned to :complete, and reason is the completion reason."
  (multiple-value-bind (triggered reason)
      (check-completion-triggers session last-response)
    (when triggered
      (transition-to-complete session reason)
      (values t reason))))

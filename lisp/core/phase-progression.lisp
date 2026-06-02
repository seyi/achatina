(in-package #:claw-lisp.core.phase-progression)

;;; ============================================================
;;; Phase Progression Policy (PHZ-003)
;;; ============================================================
;;;
;;; Implements the simple progression policy for the coding CLI:
;;; - Nudge toward :edit if stuck in repeated read-only inspection
;;; - Nudge toward :verify once edits are present
;;; - Emit transcript events for phase transitions
;;;
;;; The policy observes tool execution patterns and recommends
;;; phase transitions. It does NOT own the state machine — it
;;; advises the runtime which calls transition-phase.

(defconstant +read-only-stagnation-threshold+ 3
  "Number of consecutive read-only tool rounds before nudging toward :edit.")

(defconstant +edit-without-verify-threshold+ 5
  "Number of edit-phase tool rounds before nudging toward :verify.")

;;; --- Progression Analysis ---

(defun classify-tool-calls-for-phase (tool-calls runtime)
  "Classify TOOL-CALLS as :read-only, :mutation, or :mixed.
   Uses the tool-phases protocol to determine tool categories."
  (let ((has-read nil)
        (has-write nil))
    (dolist (tool-call tool-calls)
      (let* ((tool-name (getf tool-call :name))
             (tool (claw-lisp.core.runtime:resolve-tool runtime tool-name)))
        (when tool
          (let ((phases (claw-lisp.core.tool-phases:tool-valid-phases tool)))
            (cond
              ((and (member :inspect phases)
                    (not (member :edit phases :test
                                 (lambda (a b) (and (eq a :edit) (eq b :edit))))))
               (setf has-read t))
              ((and (member :edit phases)
                    (not (member :inspect phases)))
               (setf has-write t))
              (t
               (setf has-read t)))))))
    (cond
      ((and has-read has-write) :mixed)
      (has-write :mutation)
      (t :read-only))))

(defun recommend-phase-transition (session tool-calls runtime)
  "Analyze SESSION state and TOOL-CALLS to recommend a phase transition.

   Returns (values recommended-phase reason) or (values nil nil) if no
   transition is recommended.

   Policy rules:
   1. If in :inspect and tools include mutations → recommend :edit
   2. If in :inspect and read-only count exceeds threshold → recommend :edit
   3. If in :edit and no mutations in this round → recommend :verify
   4. Otherwise → no recommendation"
  (let* ((current-phase (claw-lisp.core.phases:get-current-phase session))
         (inspect-count (claw-lisp.core.phases:get-phase-counter session :inspect))
         (edit-count (claw-lisp.core.phases:get-phase-counter session :edit))
         (tool-class (when tool-calls
                       (classify-tool-calls-for-phase tool-calls runtime))))
    (cond
      ;; In :inspect with mutation tools → advance to :edit
      ((and (eq current-phase :inspect)
            (eq tool-class :mutation))
       (values :edit "mutation-tool-in-inspect"))

      ;; In :inspect with mixed tools → advance to :edit
      ((and (eq current-phase :inspect)
            (eq tool-class :mixed))
       (values :edit "mixed-tools-in-inspect"))

      ;; In :inspect too long → nudge to :edit
      ((and (eq current-phase :inspect)
            (>= inspect-count +read-only-stagnation-threshold+))
       (values :edit "read-only-stagnation"))

      ;; In :edit with only read-only tools → advance to :verify
      ((and (eq current-phase :edit)
            (eq tool-class :read-only)
            (>= edit-count +edit-without-verify-threshold+))
       (values :verify "edit-phase-read-only-tools"))

      ;; No recommendation
      (t (values nil nil)))))

;;; --- Transcript Logging (PHZ-004) ---

(defun emit-phase-transition-event (session transcript-path from-phase to-phase reason)
  "Emit a transcript event recording a phase transition."
  (claw-lisp.core.runtime:maybe-append-transcript-event
   session
   transcript-path
   (list :event "phase_transition"
         :session_id (claw-lisp.core.domain:agent-session-id session)
         :from_phase (when from-phase (string-downcase (symbol-name from-phase)))
         :to_phase (string-downcase (symbol-name to-phase))
         :reason reason
         :turn_count (claw-lisp.core.phases:get-turn-count session)
         :timestamp (get-universal-time))))

;;; --- Integration Entry Point ---

(defun apply-progression-policy (session tool-calls runtime transcript-path)
  "Apply the progression policy after tool execution.

   Checks if a phase transition is recommended based on tool patterns,
   performs the transition if valid, and emits transcript event.

   Returns (values transitioned-p new-phase reason) or (values nil nil nil)."
  (multiple-value-bind (recommended-phase reason)
      (recommend-phase-transition session tool-calls runtime)
    (when recommended-phase
      (let ((current-phase (claw-lisp.core.phases:get-current-phase session)))
        (when (claw-lisp.core.phases:valid-transition-p current-phase recommended-phase)
          (claw-lisp.core.phases:transition-phase session recommended-phase reason)
          (emit-phase-transition-event session transcript-path
                                       current-phase recommended-phase reason)
          (values t recommended-phase reason))))))

(in-package #:claw-lisp.core.phases)

;;; ============================================================
;;; Phase State Management for Coding CLI
;;; ============================================================
;;;
;;; This module implements explicit phase tracking for the coding agent:
;;;   :inspect  - reading files, analyzing code, understanding context
;;;   :edit     - writing/modifying files, making changes
;;;   :verify   - running tests, checking results
;;;   :complete - task finished
;;;
;;; Phase transitions are enforced to ensure forward progress:
;;;   inspect → edit | complete
;;;   edit → verify | complete
;;;   verify → edit | complete (can retry edits)
;;;   complete → (terminal, no transitions)
;;;
;;; Phase history is bounded to the most recent transitions to prevent
;;; unbounded memory growth in long-running sessions.

(defconstant +max-phase-history-entries+ 50
  "Maximum number of phase transitions to keep in history.
   Older entries are dropped to prevent unbounded memory growth.")

;;; --- Phase State Accessors ---

(defun get-current-phase (session)
  "Return current phase keyword or NIL if not set."
  (getf (claw-lisp.core.domain:agent-session-state session) :current-phase))

(defun get-phase-history (session)
  "Return list of phase transition records."
  (getf (claw-lisp.core.domain:agent-session-state session) :phase-history))

(defun get-phase-counter (session phase)
  "Return count of tool calls in PHASE, or 0 if not tracked."
  (let ((counters (getf (claw-lisp.core.domain:agent-session-state session) :phase-counters)))
    (getf counters phase 0)))

(defun get-phase-started-at (session)
  "Return universal time when current phase started, or NIL."
  (getf (claw-lisp.core.domain:agent-session-state session) :phase-started-at))

(defun get-turn-count (session)
  "Return total number of turns in session."
  (getf (claw-lisp.core.domain:agent-session-state session) :turn-count 0))

(defun get-last-verify-result (session)
  "Return T if last verify step passed, NIL otherwise."
  (getf (claw-lisp.core.domain:agent-session-state session) :last-verify-result))

(defun get-last-turn-tool-count (session)
  "Return number of tools called in last turn."
  (getf (claw-lisp.core.domain:agent-session-state session) :last-turn-tool-count 0))

;;; --- Phase State Mutators ---

(defun set-session-state-value (session key value)
  "Set KEY to VALUE in SESSION state plist and return SESSION."
  (let ((state (copy-list (claw-lisp.core.domain:agent-session-state session))))
    (setf (getf state key) value)
    (setf (claw-lisp.core.domain:agent-session-state session) state)
    session))

(defun increment-phase-counter (session phase)
  "Increment tool call counter for PHASE and return SESSION."
  (let* ((state (claw-lisp.core.domain:agent-session-state session))
         (counters (copy-list (getf state :phase-counters)))
         (current-count (getf counters phase 0)))
    (setf (getf counters phase) (1+ current-count))
    (set-session-state-value session :phase-counters counters)))

(defun increment-turn-count (session)
  "Increment total turn count and return SESSION."
  (set-session-state-value session :turn-count (1+ (get-turn-count session))))

(defun set-last-turn-tool-count (session count)
  "Set the number of tools called in last turn."
  (set-session-state-value session :last-turn-tool-count count))

(defun set-last-verify-result (session passed-p)
  "Set whether the last verify step passed."
  (set-session-state-value session :last-verify-result passed-p))

;;; --- Phase Transition Validation ---

(defun valid-transition-p (from-phase to-phase)
  "Return T if transition from FROM-PHASE to TO-PHASE is valid.

   Valid transitions:
   - NIL → any phase (initialization)
   - :inspect → :edit | :complete
   - :edit → :verify | :complete
   - :verify → :edit | :complete (can retry edits after failed verify)
   - :complete → (none, terminal state)"
  (or (null from-phase) ;; Starting from no phase is always valid
      (case from-phase
        (:inspect (member to-phase '(:edit :complete)))
        (:edit (member to-phase '(:verify :complete)))
        (:verify (member to-phase '(:edit :complete)))
        (:complete nil)))) ;; Cannot transition from complete

(defun validate-transition (from-phase to-phase)
  "Validate transition and signal error if invalid."
  (unless (valid-transition-p from-phase to-phase)
    (error 'claw-lisp.core.conditions:invalid-phase-transition
           :from from-phase
           :to to-phase)))

;;; --- Phase Transition Function ---

(defun transition-phase (session new-phase reason)
  "Transition SESSION to NEW-PHASE with REASON.

   This is the main phase transition function. It:
   1. Validates the transition is legal
   2. Records the transition in phase history (bounded to +max-phase-history-entries+)
   3. Updates the current phase
   4. Resets phase-specific state
   5. Returns the updated session

   Signals INVALID-PHASE-TRANSITION error if transition is not allowed.

   Optimized to batch all state mutations into a single plist rebuild."
  (let* ((old-phase (get-current-phase session))
         (timestamp (get-universal-time))
         (old-state (claw-lisp.core.domain:agent-session-state session)))

    ;; Validate transition
    (validate-transition old-phase new-phase)

    ;; Build new state in one pass (avoiding multiple O(n) copies)
    (let* ((old-history (getf old-state :phase-history))
           (new-history-entry (list :phase new-phase
                                    :from old-phase
                                    :timestamp timestamp
                                    :trigger reason))
           ;; Bound history to max entries (keep most recent)
           (new-history (let ((full-history (cons new-history-entry old-history)))
                          (if (> (length full-history) +max-phase-history-entries+)
                              (subseq full-history 0 +max-phase-history-entries+)
                              full-history)))
           (old-counters (getf old-state :phase-counters))
           ;; Initialize counter for new phase if not present
           (new-counters (if (getf old-counters new-phase)
                            old-counters
                            (list* new-phase 0 old-counters)))
           ;; Build complete new state
           (new-state (copy-list old-state)))

      ;; Update all fields in the copied state
      (setf (getf new-state :phase-history) new-history)
      (setf (getf new-state :current-phase) new-phase)
      (setf (getf new-state :phase-started-at) timestamp)
      (setf (getf new-state :phase-counters) new-counters)

      ;; Single state replacement
      (setf (claw-lisp.core.domain:agent-session-state session) new-state)
      session)))

;;; --- Phase Queries ---

(defun phase-duration-seconds (session)
  "Return how many seconds the session has been in current phase, or 0."
  (let ((started-at (get-phase-started-at session)))
    (if started-at
        (- (get-universal-time) started-at)
        0)))

(defun phase-transition-count (session)
  "Return total number of phase transitions."
  (length (get-phase-history session)))

(defun in-phase-p (session phase)
  "Return T if session is currently in PHASE."
  (eq (get-current-phase session) phase))

(defun has-entered-phase-p (session phase)
  "Return T if session has entered PHASE at any point in its history."
  (find phase (get-phase-history session) :key (lambda (entry) (getf entry :phase))))

;;; --- Initialization ---

(defun initialize-phase-state (session)
  "Initialize phase tracking state in SESSION if not present.

   This should be called when starting a new coding session to ensure
   phase state fields exist. Safe to call multiple times (idempotent)."
  (let ((state (claw-lisp.core.domain:agent-session-state session)))
    ;; Only initialize if not already present
    (unless (getf state :phase-counters)
      (set-session-state-value session :phase-counters
        '(:inspect 0 :edit 0 :verify 0 :complete 0)))
    (unless (getf state :phase-history)
      (set-session-state-value session :phase-history nil))
    (unless (getf state :turn-count)
      (set-session-state-value session :turn-count 0))
    session))

;;; --- Phase Summary ---

(defun phase-summary (session)
  "Return a plist summarizing the current phase state.

   Useful for debugging and logging."
  (list :current-phase (get-current-phase session)
        :phase-duration-seconds (phase-duration-seconds session)
        :phase-transition-count (phase-transition-count session)
        :turn-count (get-turn-count session)
        :inspect-tool-count (get-phase-counter session :inspect)
        :edit-tool-count (get-phase-counter session :edit)
        :verify-tool-count (get-phase-counter session :verify)
        :last-verify-result (get-last-verify-result session)))

(in-package #:claw-lisp.core.tool-phases)

;;; ============================================================
;;; Tool Phase Compatibility Protocol (FND-004)
;;; ============================================================
;;;
;;; Defines which tools are valid in which coding CLI phases.
;;; Phase validation is checked before tool execution to enforce
;;; the inspect → edit → verify → complete workflow.
;;;
;;; Default: all tools valid in all phases (permissive).
;;; Specific tools restrict themselves to appropriate phases.

;;; --- Phase Compatibility Generic ---

(defgeneric tool-valid-phases (tool)
  (:documentation "Return list of phases TOOL is valid for.
   Phases: :inspect | :edit | :verify | :complete
   Default permits all phases unless a tool specializes this method."))

(defmethod tool-valid-phases ((tool claw-lisp.core.protocols:tool))
  '(:inspect :edit :verify :complete))

;;; --- Per-Tool Phase Restrictions ---

(defmethod tool-valid-phases ((tool claw-lisp.tools.file-read::file-read-tool))
  '(:inspect :edit :verify))

(defmethod tool-valid-phases ((tool claw-lisp.tools.file-write::file-write-tool))
  '(:edit))

(defmethod tool-valid-phases ((tool claw-lisp.tools.file-replace::file-replace-tool))
  '(:edit))

(defmethod tool-valid-phases ((tool claw-lisp.tools.grep::grep-tool))
  '(:inspect :edit :verify))

(defmethod tool-valid-phases ((tool claw-lisp.tools.glob::glob-tool))
  '(:inspect :edit :verify))

(defmethod tool-valid-phases ((tool claw-lisp.tools.shell-command::shell-command-tool))
  '(:edit :verify))

;;; --- Phase Validation ---

(defun tool-valid-for-phase-p (tool current-phase)
  "Return T if TOOL is valid for CURRENT-PHASE."
  (member current-phase (tool-valid-phases tool)))

(defun check-tool-phase-compatibility (tool session)
  "Check if TOOL is valid for SESSION's current phase.
   Signals PHASE-VIOLATION-ERROR if not valid.
   Does nothing if no phase is set (permissive before phase init)."
  (let ((current-phase (claw-lisp.core.phases:get-current-phase session)))
    (when (and current-phase
               (not (tool-valid-for-phase-p tool current-phase)))
      (error 'claw-lisp.core.conditions:phase-violation-error
             :tool (claw-lisp.core.protocols:tool-name tool)
             :current-phase current-phase
             :valid-phases (tool-valid-phases tool)))))

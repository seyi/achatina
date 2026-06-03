(in-package #:claw-lisp.core.tool-capability)

;;; ============================================================
;;; Tool Capability — Single Source of Truth for Loop Control (FND-004, Step A)
;;; ============================================================
;;;
;;; A tool's loop-control semantics (read vs write vs exec, and which phases it
;;; is valid in) are a property of the TOOL, declared once via TOOL-CAPABILITY.
;;;
;;; Before this module, three subsystems each hardcoded their own tool-name
;;; lists and could drift:
;;;   - runtime.lisp stagnation guard (+read-only-tool-names+/+write-tool-names+)
;;;   - tool-envelope.lisp envelope-is-read-only-p/-mutation-p
;;;   - phase-progression.lisp (via the envelope predicates)
;;;
;;; Now all three resolve classification through this one module. Object-aware
;;; call sites use the TOOL-CAPABILITY generic; name-only call sites (the
;;; stagnation guard, envelope predicates) use TOOL-NAME-CAPABILITY, which reads
;;; a registry seeded at tool load. Each built-in tool declares its capability
;;; once as a constant used by BOTH its method and its registration, so the
;;; object path and the name path cannot disagree.
;;;
;;; A capability is a plist:
;;;   :class        — :read | :write | :exec | :meta
;;;   :valid-phases — list of (:inspect :edit :verify :complete)
;;;   :mutates-fs   — boolean; true => eligible to satisfy "loop progress"

(defparameter *default-tool-capability*
  '(:class :exec
    :valid-phases (:inspect :edit :verify :complete)
    :mutates-fs nil)
  "Permissive capability for tools that do not declare one. Treated as :exec —
   neither read-only nor a mutation — so unknown tools never accidentally count
   as inspection (stalling) or as progress (resetting the loop).")

(defvar *tool-capability-registry* (make-hash-table :test #'equal)
  "Maps tool-name string -> capability plist. Seeded by REGISTER-TOOL-CAPABILITY
   at tool load and refreshed when a tool is registered into a runtime.")

(defgeneric tool-capability (tool)
  (:documentation
   "Return the capability plist for TOOL (a protocols:tool instance).
    Default is *DEFAULT-TOOL-CAPABILITY*; built-in tools specialize this."))

(defmethod tool-capability ((tool t))
  (declare (ignore tool))
  *default-tool-capability*)

(defun register-tool-capability (name capability)
  "Record CAPABILITY (a plist) under tool NAME in the global registry.
   Idempotent: capability is a static property of a tool class, so repeated
   registration of the same name with the same plist is a no-op in effect."
  (check-type name string)
  (setf (gethash name *tool-capability-registry*) capability)
  name)

(defun tool-name-capability (name)
  "Return the capability plist registered for tool NAME, or the permissive
   default when NAME is unknown. This is the name-only resolution path used by
   the stagnation guard and the envelope predicates."
  (gethash name *tool-capability-registry* *default-tool-capability*))

;;; --- Capability predicates (operate on a plist) ---

(defun capability-class (capability)
  (getf capability :class :exec))

(defun capability-read-only-p (capability)
  (eq (capability-class capability) :read))

(defun capability-mutation-p (capability)
  (eq (capability-class capability) :write))

(defun capability-valid-phases (capability)
  (getf capability :valid-phases
        (getf *default-tool-capability* :valid-phases)))

(defun capability-mutates-fs-p (capability)
  (and (getf capability :mutates-fs) t))

;;; --- Name-based predicates (the single classifier all consumers share) ---

(defun tool-name-read-only-p (name)
  "Return T when the tool named NAME is a read-only inspection action."
  (capability-read-only-p (tool-name-capability name)))

(defun tool-name-mutation-p (name)
  "Return T when the tool named NAME is a file-mutation action."
  (capability-mutation-p (tool-name-capability name)))

(defun tool-name-valid-for-phase-p (name phase)
  "Return T when the tool named NAME is valid for PHASE. Permissive for unknown."
  (member phase (capability-valid-phases (tool-name-capability name))))

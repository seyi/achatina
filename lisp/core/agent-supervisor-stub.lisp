(in-package #:claw-lisp.core.agent-supervisor)

;;; Public stub for enterprise agent-supervisor.
;;; Child-agent orchestration is not included in the public Achatina build.
;;; These stubs allow the runtime to compile and CLI commands to report
;;; gracefully that the feature is unavailable.

(defun ensure-agent-supervisor (session)
  (declare (ignore session))
  nil)

(defun spawn-child-agent (runtime session &rest args &key &allow-other-keys)
  (declare (ignore runtime session args))
  (error "Child-agent orchestration is not available in the public Achatina build."))

(defun send-agent-message (supervisor message &rest args &key &allow-other-keys)
  (declare (ignore supervisor message args))
  nil)

(defun receive-agent-message (supervisor &rest args &key &allow-other-keys)
  (declare (ignore supervisor args))
  nil)

(defun await-child-agent (supervisor child-id &rest args &key &allow-other-keys)
  (declare (ignore supervisor child-id args))
  nil)

(defun find-child-handle (supervisor child-id)
  (declare (ignore supervisor child-id))
  nil)

(defun list-child-agents (supervisor)
  (declare (ignore supervisor))
  nil)

(defun child-progress-snapshot (supervisor child-id)
  (declare (ignore supervisor child-id))
  nil)

(defun list-child-progress-snapshots (supervisor)
  (declare (ignore supervisor))
  nil)

(defun cancel-child-agent (supervisor child-id &rest args &key &allow-other-keys)
  (declare (ignore supervisor child-id args))
  nil)

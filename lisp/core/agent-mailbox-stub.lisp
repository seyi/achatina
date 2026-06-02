(in-package #:claw-lisp.core.agent-mailbox)

;;; Public stub for enterprise agent-mailbox.
;;; Child-agent orchestration is not included in the public Achatina build.

(defun make-agent-mailbox (&rest args &key &allow-other-keys)
  (declare (ignore args))
  nil)

(defun mailbox-send (mailbox message &rest args &key &allow-other-keys)
  (declare (ignore mailbox message args))
  nil)

(defun mailbox-receive (mailbox &rest args &key &allow-other-keys)
  (declare (ignore mailbox args))
  nil)

(defun mailbox-close (mailbox)
  (declare (ignore mailbox))
  nil)

(defun mailbox-depth (mailbox)
  (declare (ignore mailbox))
  0)

(defun mailbox-dead-letters (mailbox)
  (declare (ignore mailbox))
  nil)

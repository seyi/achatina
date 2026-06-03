(in-package #:claw-lisp.core.tool-envelope)

;;; ============================================================
;;; Normalized Tool Result Envelope for Coding CLI (FND-003)
;;; ============================================================
;;;
;;; This module wraps tool execution results in a normalized envelope
;;; that captures success/failure state, error classification, timing
;;; metadata, and phase compatibility information.
;;;
;;; The envelope is layered ON TOP of the existing tool-result struct
;;; in domain.lisp — it does not replace it. The runtime continues to
;;; produce tool-result structs; this module interprets and classifies
;;; them for the coding CLI's phase-aware decision-making.
;;;
;;; Tool categorization is provisional here. FND-004 will introduce a
;;; proper registry-based category system on tool registration.

(defstruct (tool-envelope
            (:constructor %make-tool-envelope
                (&key success tool-name content
                      error-type error-message
                      duration-ms timestamp
                      phase-at-execution)))
  (success nil :type boolean)
  (tool-name "" :type string)
  (content nil :type t)
  (error-type nil :type (or null keyword))
  (error-message nil :type (or null string))
  (duration-ms 0 :type number)
  (timestamp 0 :type integer)
  (phase-at-execution nil :type (or null keyword)))

(defun make-tool-envelope (&key success tool-name content
                                error-type error-message
                                duration-ms timestamp
                                phase-at-execution)
  "Construct a tool-envelope. Failure is derived from (not success)."
  (%make-tool-envelope
   :success success
   :tool-name (or tool-name "")
   :content content
   :error-type error-type
   :error-message error-message
   :duration-ms (or duration-ms 0)
   :timestamp (or timestamp (get-universal-time))
   :phase-at-execution phase-at-execution))

;;; --- Error Classification ---

(defun classify-tool-error (condition)
  "Classify CONDITION into a normalized error category keyword."
  (typecase condition
    (claw-lisp.core.conditions:permission-error :permission)
    (claw-lisp.core.conditions:tool-error :execution)
    (claw-lisp.core.conditions:config-error :validation)
    (t :execution)))

;;; --- Envelope Construction ---

(defun wrap-tool-success (tool-name content &key duration-ms phase)
  "Create a success envelope for TOOL-NAME with CONTENT."
  (make-tool-envelope
   :success t
   :tool-name tool-name
   :content content
   :duration-ms (or duration-ms 0)
   :timestamp (get-universal-time)
   :phase-at-execution phase))

(defun wrap-tool-failure (tool-name condition &key duration-ms phase)
  "Create a failure envelope for TOOL-NAME from error CONDITION."
  (make-tool-envelope
   :success nil
   :tool-name tool-name
   :error-type (classify-tool-error condition)
   :error-message (princ-to-string condition)
   :duration-ms (or duration-ms 0)
   :timestamp (get-universal-time)
   :phase-at-execution phase))

;;; --- Envelope from Existing Tool Result ---

(defun envelope-from-tool-result (tool-result &key phase)
  "Create a tool-envelope from an existing domain tool-result struct.

   Interprets the tool-result content to determine success/error state.
   A tool-result whose content starts with 'error:' (case-insensitive)
   is treated as a failure. Empty or non-string content is treated as
   success (empty file reads, nil results are valid)."
  (let* ((tool-name (claw-lisp.core.domain:tool-result-tool-name tool-result))
         (content (claw-lisp.core.domain:tool-result-content tool-result))
         (is-error (and (stringp content)
                        (>= (length content) 6)
                        (string-equal "error:" (subseq content 0 6)))))
    (make-tool-envelope
     :success (not is-error)
     :tool-name tool-name
     :content content
     :error-type (when is-error :execution)
     :error-message (when is-error content)
     :timestamp (get-universal-time)
     :phase-at-execution phase)))

;;; --- Envelope Queries ---

(defun envelope-succeeded-p (envelope)
  "Return T if ENVELOPE represents a successful tool execution."
  (tool-envelope-success envelope))

(defun envelope-failed-p (envelope)
  "Return T if ENVELOPE represents a failed tool execution."
  (not (tool-envelope-success envelope)))

(defun envelope-is-read-only-p (envelope)
  "Return T if ENVELOPE's tool is a read-only operation.
   Used by phase logic to distinguish inspection from mutation.
   Classification is resolved through the single capability source of truth
   (claw-lisp.core.tool-capability); tools of class :exec/:meta or unknown
   tools return NIL from both this and ENVELOPE-IS-MUTATION-P."
  (tool-name-read-only-p (tool-envelope-tool-name envelope)))

(defun envelope-is-mutation-p (envelope)
  "Return T if ENVELOPE's tool is a write/mutation operation.
   Resolved through claw-lisp.core.tool-capability; see ENVELOPE-IS-READ-ONLY-P."
  (tool-name-mutation-p (tool-envelope-tool-name envelope)))

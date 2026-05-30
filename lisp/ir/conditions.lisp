(in-package #:claw-lisp.ir.conditions)

(defparameter +ir-condition-report-payload-limit+ 160
  "Maximum number of characters to include when rendering IR payloads in condition reports.")

(defun %summarize-ir-report-payload (payload)
  (let* ((printed (with-standard-io-syntax
                    (let ((*print-length* 6)
                          (*print-level* 4)
                          (*print-circle* t)
                          (*print-pretty* nil))
                      (prin1-to-string payload))))
         (limit +ir-condition-report-payload-limit+))
    (if (> (length printed) limit)
        (format nil "~A..." (subseq printed 0 limit))
        printed)))

(define-condition ir-error (error)
  ()
  (:documentation "Base condition for IR schema and CAS bridge failures."))

(define-condition ir-storage-error (ir-error)
  ((operation :initarg :operation :reader ir-storage-error-operation)
   (reason :initarg :reason :reader ir-storage-error-reason))
  (:report (lambda (condition stream)
             (format stream "IR storage operation ~S failed: ~A"
                     (ir-storage-error-operation condition)
                     (ir-storage-error-reason condition)))))

(define-condition ir-serialization-error (ir-error)
  ((object :initarg :object :reader ir-serialization-error-object)
   (reason :initarg :reason :reader ir-serialization-error-reason))
  (:report (lambda (condition stream)
             (format stream "IR serialization failed for ~S: ~A"
                     (ir-serialization-error-object condition)
                     (ir-serialization-error-reason condition)))))

(define-condition ir-deserialization-error (ir-error)
  ((payload :initarg :payload :reader ir-deserialization-error-payload)
   (reason :initarg :reason :reader ir-deserialization-error-reason))
  (:report (lambda (condition stream)
             (format stream "IR deserialization failed for ~A: ~A"
                     (%summarize-ir-report-payload
                      (ir-deserialization-error-payload condition))
                     (ir-deserialization-error-reason condition)))))

(define-condition ir-version-mismatch-error (ir-error)
  ((expected :initarg :expected :reader ir-version-mismatch-error-expected)
   (actual :initarg :actual :reader ir-version-mismatch-error-actual)
   (object-type :initarg :object-type :reader ir-version-mismatch-error-object-type))
  (:report (lambda (condition stream)
             (format stream "Unsupported IR version ~S for ~S (expected ~S)"
                     (ir-version-mismatch-error-actual condition)
                     (ir-version-mismatch-error-object-type condition)
                     (ir-version-mismatch-error-expected condition)))))

(define-condition ir-validation-error (ir-error)
  ((subject :initarg :subject :reader ir-validation-error-subject)
   (reason :initarg :reason :reader ir-validation-error-reason))
  (:report (lambda (condition stream)
             (format stream "IR validation failed for ~S: ~A"
                     (ir-validation-error-subject condition)
                     (ir-validation-error-reason condition)))))

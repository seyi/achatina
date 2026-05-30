(in-package #:claw-lisp.cas.integrity)

(define-condition cas-integrity-error (error)
  ()
  (:documentation "Base condition for CAS integrity verification failures."))

(define-condition cas-integrity-missing-object-error (cas-integrity-error)
  ((hash :initarg :hash :reader cas-integrity-missing-object-error-hash))
  (:report (lambda (condition stream)
             (format stream "CAS object is missing: ~A"
                     (cas-integrity-missing-object-error-hash condition)))))

(define-condition cas-integrity-corrupt-object-error (cas-integrity-error)
  ((hash :initarg :hash :reader cas-integrity-corrupt-object-error-hash)
   (expected :initarg :expected :reader cas-integrity-corrupt-object-error-expected)
   (actual :initarg :actual :reader cas-integrity-corrupt-object-error-actual))
  (:report (lambda (condition stream)
             (format stream "CAS object ~A is corrupt (expected ~A, actual ~A)"
                     (cas-integrity-corrupt-object-error-hash condition)
                     (cas-integrity-corrupt-object-error-expected condition)
                     (cas-integrity-corrupt-object-error-actual condition)))))

(define-condition cas-integrity-missing-ref-error (cas-integrity-error)
  ((name :initarg :name :reader cas-integrity-missing-ref-error-name))
  (:report (lambda (condition stream)
             (format stream "CAS ref is missing: ~A"
                     (cas-integrity-missing-ref-error-name condition)))))

(define-condition cas-integrity-dangling-ref-error (cas-integrity-error)
  ((name :initarg :name :reader cas-integrity-dangling-ref-error-name)
   (hash :initarg :hash :reader cas-integrity-dangling-ref-error-hash))
  (:report (lambda (condition stream)
             (format stream "CAS ref ~A points to missing object ~A"
                     (cas-integrity-dangling-ref-error-name condition)
                     (cas-integrity-dangling-ref-error-hash condition)))))

(define-condition cas-integrity-manifest-error (cas-integrity-error)
  ((cause :initarg :cause :reader cas-integrity-manifest-error-cause))
  (:report (lambda (condition stream)
             (format stream "Manifest verification failed: ~A"
                     (cas-integrity-manifest-error-cause condition)))))

(defstruct (integrity-failure (:conc-name integrity-failure-))
  kind
  subject
  expected
  actual
  context)

(defstruct (integrity-report (:conc-name integrity-report-)
                             (:constructor %make-integrity-report))
  target-kind
  target
  verified-count
  failures
  metadata)

(defun integrity-report-failure-count (report)
  (length (integrity-report-failures report)))

(defun integrity-report-ok-p (report)
  (zerop (integrity-report-failure-count report)))

(defun %make-report (target-kind target verified-count failures &key metadata)
  (%make-integrity-report :target-kind target-kind
                          :target target
                          :verified-count verified-count
                          :failures (nreverse failures)
                          :metadata metadata))

(defun %missing-object-failure (cas-hash &key context)
  (make-integrity-failure :kind :missing-object
                          :subject cas-hash
                          :expected cas-hash
                          :actual nil
                          :context context))

(defun %corrupt-object-failure (cas-hash actual-hash &key context)
  (make-integrity-failure :kind :corrupt-object
                          :subject cas-hash
                          :expected cas-hash
                          :actual actual-hash
                          :context context))

(defun %signal-object-failure (failure)
  (ecase (integrity-failure-kind failure)
    (:missing-object
     (error 'cas-integrity-missing-object-error
            :hash (integrity-failure-subject failure)))
    (:corrupt-object
     (error 'cas-integrity-corrupt-object-error
            :hash (integrity-failure-subject failure)
            :expected (integrity-failure-expected failure)
            :actual (integrity-failure-actual failure)))))

(defun %verify-object (cas-root cas-hash &key context error-p)
  (cond
    ((not (cas-exists-p cas-root cas-hash))
     (let ((failure (%missing-object-failure cas-hash :context context)))
       (when error-p
         (%signal-object-failure failure))
       (values nil failure)))
    (t
     (let* ((octets (cas-get-bytes cas-root cas-hash))
            (actual-hash (and octets (cas-hash-bytes octets))))
       (cond
         ((null octets)
          (let ((failure (%missing-object-failure cas-hash :context context)))
            (when error-p
              (%signal-object-failure failure))
            (values nil failure)))
         ((string= actual-hash cas-hash)
          (values t nil))
         (t
          (let ((failure (%corrupt-object-failure cas-hash actual-hash :context context)))
            (when error-p
              (%signal-object-failure failure))
            (values nil failure))))))))

(defun verify-cas-object-integrity (cas-root cas-hash &key error-p)
  "Verify that CAS-HASH exists and the stored object content matches its hash."
  (multiple-value-bind (ok failure)
      (%verify-object cas-root cas-hash :error-p error-p)
    (%make-report :object
                  cas-hash
                  (if ok 1 0)
                  (if failure (list failure) nil))))

(defun verify-cas-ref-integrity (ref-root cas-root ref-name &key error-p)
  "Verify that REF-NAME exists and resolves to a valid CAS object."
  (let ((record (read-cas-ref ref-root ref-name)))
    (if (null record)
        (progn
          (when error-p
            (error 'cas-integrity-missing-ref-error :name ref-name))
          (%make-report :ref
                        ref-name
                        0
                        (list (make-integrity-failure :kind :missing-ref
                                                      :subject ref-name
                                                      :expected ref-name
                                                      :actual nil
                                                      :context nil))))
        (let ((cas-hash (getf record :cas-hash)))
          (multiple-value-bind (ok failure)
              (%verify-object cas-root cas-hash
                              :context (list :ref-name ref-name)
                              :error-p nil)
            (when (and failure error-p)
              (if (eq (integrity-failure-kind failure) :missing-object)
                  (error 'cas-integrity-dangling-ref-error
                         :name ref-name
                         :hash cas-hash)
                  (%signal-object-failure failure)))
            (%make-report :ref
                          ref-name
                          (if ok 2 1)
                          (if failure
                              (list (make-integrity-failure
                                     :kind (if (eq (integrity-failure-kind failure)
                                                   :missing-object)
                                               :dangling-ref
                                               (integrity-failure-kind failure))
                                     :subject ref-name
                                     :expected cas-hash
                                     :actual (integrity-failure-actual failure)
                                     :context (list :ref-name ref-name
                                                    :cas-hash cas-hash)))
                              nil)
                          :metadata (list :cas-hash cas-hash)))))))

(defun verify-manifest-graph-integrity (cas-root manifest-or-hash
                                        &key error-p verify-signature-p)
  "Verify a manifest object and every CAS object referenced by its entries.
When MANIFEST-OR-HASH is a string, verify both the manifest object and the
manifest graph. Otherwise treat it as an already-loaded manifest."
  (let ((failures nil)
        (verified-count 0)
        (manifest nil)
        (target manifest-or-hash))
    (flet ((manifest-error-report (condition)
             (%make-report :manifest
                           target
                           verified-count
                           (list (make-integrity-failure :kind :manifest-error
                                                         :subject target
                                                         :expected t
                                                         :actual nil
                                                         :context (list :cause condition))))))
      (cond
        ((stringp manifest-or-hash)
         (multiple-value-bind (ok failure)
             (%verify-object cas-root manifest-or-hash
                             :context (list :role :manifest)
                             :error-p error-p)
           (if ok
               (incf verified-count)
               (progn
                 (push failure failures)
                 (return-from verify-manifest-graph-integrity
                   (%make-report :manifest target verified-count failures)))))
         (handler-case
             (setf manifest (load-manifest cas-root manifest-or-hash
                                           :verify-integrity-p t
                                           :verify-signature-p verify-signature-p))
           (claw-lisp.cas.manifest:cas-manifest-integrity-error (condition)
             (when error-p
               (error 'cas-integrity-manifest-error :cause condition))
             (return-from verify-manifest-graph-integrity (manifest-error-report condition)))
           (claw-lisp.cas.manifest:cas-manifest-signature-error (condition)
             (when error-p
               (error 'cas-integrity-manifest-error :cause condition))
             (return-from verify-manifest-graph-integrity (manifest-error-report condition)))
           (claw-lisp.cas.manifest:cas-manifest-parse-error (condition)
             (when error-p
               (error 'cas-integrity-manifest-error :cause condition))
             (return-from verify-manifest-graph-integrity (manifest-error-report condition)))))
        (t
         (setf manifest manifest-or-hash)
         (unless (verify-manifest-integrity manifest)
           (let ((failure (make-integrity-failure :kind :manifest-integrity
                                                 :subject manifest-or-hash
                                                 :expected t
                                                 :actual nil
                                                 :context nil)))
             (when error-p
               (error 'cas-integrity-manifest-error
                      :cause "Manifest root digest does not match its contents"))
             (push failure failures)
             (return-from verify-manifest-graph-integrity
               (%make-report :manifest target verified-count failures))))))
      (dolist (entry (manifest-entries manifest))
        (multiple-value-bind (ok failure)
            (%verify-object cas-root
                            (manifest-entry-cas-hash entry)
                            :context (list :entry-role
                                           (claw-lisp.cas.manifest:manifest-entry-role entry)
                                           :entry-type
                                           (claw-lisp.cas.manifest:manifest-entry-type entry))
                            :error-p error-p)
          (if ok
              (incf verified-count)
              (push failure failures))))
      (%make-report :manifest
                    target
                    verified-count
                    failures
                    :metadata (list :entry-count (length (manifest-entries manifest))
                                    :verify-signature-p verify-signature-p)))))

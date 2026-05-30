(in-package #:claw-lisp.ir.cas-bridge)

(defparameter +max-ir-payload-chars+ (* 10 1024 1024)
  "Maximum accepted character count for a serialized IR payload.")

(defun %ir-object-id (ir-object)
  (typecase ir-object
    (ir-graph (ir-graph-id ir-object))
    (ir-node (ir-node-id ir-object))
    (t nil)))

(defun %ir-object-kind (ir-object)
  (typecase ir-object
    (ir-graph (ir-graph-node-type ir-object))
    (ir-node :ir-node)
    (t nil)))

(defun %ir-object-version (ir-object)
  (typecase ir-object
    (ir-graph (ir-graph-ir-version ir-object))
    (t nil)))

(defun %ir-artifact-metadata (ir-object metadata)
  (canonicalize-plist
   (append metadata
           (when (%ir-object-version ir-object)
             (list :ir-version (%ir-object-version ir-object)))
           (list :ir-object-kind (%ir-object-kind ir-object)))))

(defun materialize-ir-to-cas (runtime ir-object &key ref-name metadata)
  "Persist IR-OBJECT into CAS and return an ARTIFACT handle plus summary metadata.
Signals `ir-storage-error` when the runtime has no CAS root configured."
  (let ((cas-root (runtime-effective-cas-root runtime)))
    (when (and ref-name (not (valid-cas-ref-name-p ref-name)))
      (error 'ir-storage-error
             :operation :materialize-ir-to-cas
             :reason (format nil "Invalid IR CAS ref name: ~S" ref-name)))
    (let* ((artifact-kind (%ir-object-kind ir-object))
           (artifact-id (%ir-object-id ir-object))
           (artifact-metadata (%ir-artifact-metadata ir-object metadata)))
      (unless artifact-kind
        (error 'ir-serialization-error
               :object ir-object
               :reason (format nil "Unsupported IR object type ~S" (type-of ir-object))))
      (unless cas-root
        (error 'ir-storage-error
               :operation :materialize-ir-to-cas
               :reason "Runtime has no CAS root configured for IR persistence."))
      (let* ((serialized (ir-object->canonical-string ir-object))
             (cas-hash (cas-put cas-root serialized))
             (byte-count
               #+sbcl (length (sb-ext:string-to-octets serialized :external-format :utf-8))
               #-sbcl (length serialized))
             (artifact (make-artifact :id artifact-id
                                      :kind artifact-kind
                                      :cas-hash cas-hash
                                      :cas-type :sexp
                                      :cas-ref-name ref-name
                                      :metadata artifact-metadata))
             (ref-root (runtime-effective-cas-ref-root runtime)))
        (when (and ref-name ref-root)
          (write-cas-ref ref-root ref-name cas-hash
                         :record-history-p t
                         :metadata artifact-metadata))
        (values artifact
                (list :id artifact-id
                      :kind artifact-kind
                      :cas-hash cas-hash
                      :cas-type :sexp
                      :cas-ref-name ref-name
                      :bytes byte-count
                      :metadata artifact-metadata))))))

(defun %validate-read-top-level-form (form payload)
  (unless (and (consp form)
               (keywordp (first form))
               (member (first form) '(:ir-graph :ir-node :ir-edge :artifact) :test #'eq))
    (error 'ir-deserialization-error
           :payload payload
           :reason (format nil "Invalid top-level IR form ~S" form)))
  form)

(defun load-ir-from-cas (runtime cas-hash)
  "Load and decode an IR object directly from CAS-HASH."
  (unless (valid-versioned-hash-p cas-hash)
    (error 'ir-storage-error
           :operation :load-ir-from-cas
           :reason (format nil "Invalid IR CAS hash: ~S" cas-hash)))
  (let* ((cas-root (runtime-effective-cas-root runtime))
         (text (and cas-root (cas-get cas-root cas-hash))))
    (unless cas-root
      (error 'ir-storage-error
             :operation :load-ir-from-cas
             :reason "Runtime has no CAS root configured for IR load."))
    (unless text
      (error 'ir-storage-error
             :operation :load-ir-from-cas
             :reason (format nil "IR CAS object is missing for hash: ~A" cas-hash)))
    (when (> (length text) +max-ir-payload-chars+)
      (error 'ir-deserialization-error
             :payload (subseq text 0 (min 256 (length text)))
             :reason (format nil "IR payload exceeds maximum size of ~D characters"
                             +max-ir-payload-chars+)))
    (handler-case
        (with-standard-io-syntax
          (let ((*read-eval* nil)
                (*package* (find-package '#:cl-user)))
            (canonical-sexp->ir-object
             (%validate-read-top-level-form (read-from-string text) text))))
      (ir-version-mismatch-error (condition)
        (error condition))
      (ir-deserialization-error (condition)
        (error condition))
      (error (condition)
        (error 'ir-deserialization-error
               :payload text
               :reason condition)))))

(defun resolve-ir-from-cas (runtime artifact)
  "Resolve ARTIFACT through refs when needed and decode the stored IR object."
  (let* ((cas-root (runtime-effective-cas-root runtime))
         (ref-root (runtime-effective-cas-ref-root runtime))
         (cas-hash (artifact-cas-hash artifact))
         (ref-name (artifact-cas-ref-name artifact))
         (resolved-hash (or (and ref-name ref-root cas-root
                                 (resolve-cas-ref ref-root cas-root ref-name
                                                  :require-object-p t))
                            cas-hash)))
    (unless resolved-hash
      (error 'ir-storage-error
             :operation :resolve-ir-from-cas
             :reason (format nil "IR artifact has no CAS hash or resolvable ref: ~S" artifact)))
    (load-ir-from-cas runtime resolved-hash)))

(defun resolve-ir-ref (runtime ref-name &key (require-object-p t))
  "Resolve REF-NAME to an IR object through the runtime's CAS roots."
  (let* ((ref-root (runtime-effective-cas-ref-root runtime))
         (cas-root (runtime-effective-cas-root runtime)))
    (unless ref-root
      (error 'ir-storage-error
             :operation :resolve-ir-ref
             :reason "Runtime has no CAS ref root configured for IR refs."))
    (unless cas-root
      (error 'ir-storage-error
             :operation :resolve-ir-ref
             :reason "Runtime has no CAS root configured for IR refs."))
    (let ((record (read-cas-ref ref-root ref-name)))
      (unless record
        (return-from resolve-ir-ref nil))
      (let ((cas-hash (resolve-cas-ref ref-root cas-root ref-name
                                       :require-object-p require-object-p)))
        (and cas-hash
             (load-ir-from-cas runtime cas-hash))))))

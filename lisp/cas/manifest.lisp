(in-package #:claw-lisp.cas.manifest)

(define-condition cas-manifest-error (error)
  ()
  (:documentation "Base condition for manifest-related errors."))

(define-condition cas-manifest-integrity-error (cas-manifest-error)
  ((expected :initarg :expected :reader cas-manifest-integrity-error-expected)
   (actual :initarg :actual :reader cas-manifest-integrity-error-actual))
  (:report (lambda (condition stream)
             (format stream "Manifest integrity check failed: expected ~S, actual ~S"
                     (cas-manifest-integrity-error-expected condition)
                     (cas-manifest-integrity-error-actual condition)))))

(define-condition cas-manifest-signature-error (cas-manifest-error)
  ((digest :initarg :digest :reader cas-manifest-signature-error-digest)
   (signature :initarg :signature :reader cas-manifest-signature-error-signature))
  (:report (lambda (condition stream)
             (format stream "Manifest signature verification failed for digest ~S"
                     (cas-manifest-signature-error-digest condition)))))

(define-condition cas-manifest-parse-error (cas-manifest-error)
  ((text :initarg :text :reader cas-manifest-parse-error-text)
   (reason :initarg :reason :reader cas-manifest-parse-error-reason))
  (:report (lambda (condition stream)
             (format stream "Manifest parse failed: ~A"
                     (cas-manifest-parse-error-reason condition)))))

(defstruct (manifest-entry (:conc-name manifest-entry-))
  role        ;; e.g. :session-memory, :compaction-ir, :tool-result, :hitl-decision
  cas-hash    ;; versioned hash string, e.g. "sha256:..."
  type        ;; e.g. :sexp, :json, :markdown, :binary
  metadata)   ;; plist

(defstruct (manifest (:conc-name manifest-)
                     (:constructor %make-manifest))
  root-digest  ;; hash over canonicalized entries + metadata
  entries      ;; list of MANIFEST-ENTRY
  metadata     ;; plist (e.g. :session-id, :kind :compaction, :timestamp ...)
  signature)   ;; opaque signature blob or NIL

(defun %json-string (value)
  (with-output-to-string (stream)
    (yason:encode value stream)))

(defun %write-json-value (value stream)
  "Write VALUE as JSON using a stable, explicit object key order."
  (cond
    ((null value) (write-string "null" stream))
    ((eq value t) (write-string "true" stream))
    ((stringp value) (write-string (%json-string value) stream))
    ((numberp value) (princ value stream))
    ((keywordp value)
     (write-string (%json-string (string-downcase (symbol-name value))) stream))
    ((and (listp value) (evenp (length value)) (keywordp (first value)))
     (write-char #\{ stream)
     (loop for (key val) on value by #'cddr
           for first-p = t then nil
           do (unless first-p (write-char #\, stream))
              (write-string (%json-string (string-downcase (symbol-name key))) stream)
              (write-char #\: stream)
              (%write-json-value val stream))
     (write-char #\} stream))
    ((vectorp value)
     (write-char #\[ stream)
     (loop for i from 0 below (length value)
           do (when (> i 0) (write-char #\, stream))
              (%write-json-value (aref value i) stream))
     (write-char #\] stream))
    ((listp value)
     (write-char #\[ stream)
     (loop for item in value
           for first-p = t then nil
           do (unless first-p (write-char #\, stream))
              (%write-json-value item stream))
     (write-char #\] stream))
    (t (error "Unsupported manifest JSON value: ~S" value))))

(defun %json-value->lisp (value)
  "Convert a JSON-parsed VALUE into primitive Lisp data structures."
  (cond
    ((null value) nil)
    ((stringp value) value)
    ((numberp value) value)
    ((eq value t) t)
    ((eq value :json-false) nil)
    ((vectorp value)
     (loop for i from 0 below (length value)
           collect (%json-value->lisp (aref value i))))
    ((and (consp value) (consp (car value)) (stringp (caar value)))
     (loop for cell in value
           collect (intern (string-upcase (car cell)) "KEYWORD")
           collect (%json-value->lisp (cdr cell))))
    ((listp value)
     (mapcar #'%json-value->lisp value))
    (t (error "Unsupported JSON value in manifest: ~S" value))))

(defun %manifest-json-object->plist (text)
  (handler-case
      (let ((yason:*parse-object-as* :alist))
        (let ((parsed (yason:parse text)))
          (unless (and (listp parsed)
                       (every (lambda (cell)
                                (and (consp cell) (stringp (car cell))))
                              parsed))
            (error 'cas-manifest-parse-error
                   :text text
                   :reason "Top-level manifest value must be a JSON object"))
          (loop for (key . value) in parsed
                collect (intern (string-upcase key) "KEYWORD")
                collect (%json-value->lisp value))))
    (error (e)
      (error 'cas-manifest-parse-error
             :text text
             :reason e))))

(defun %expect-json-keys (plist expected-keys text)
  (let ((keys (loop for (k v) on plist by #'cddr collect k)))
    (when (or (not (every #'keywordp keys))
              (not (= (length keys) (length (remove-duplicates keys :test #'eq))))
              (not (every (lambda (k) (member k expected-keys :test #'eq)) keys))
              (not (every (lambda (k) (member k keys :test #'eq)) expected-keys)))
      (error 'cas-manifest-parse-error
             :text text
             :reason "Manifest JSON object has unexpected or missing keys"))))

(defun %entry-from-json (entry text)
  (unless (and (listp entry)
               (every #'keywordp (loop for (k v) on entry by #'cddr collect k)))
    (error 'cas-manifest-parse-error
           :text text
           :reason "Manifest entry must be a JSON object"))
  (destructuring-bind (&key role cas-hash type metadata &allow-other-keys) entry
    (unless (and (or (keywordp role) (stringp role))
                 (stringp cas-hash)
                 (or (keywordp type) (stringp type)))
      (error 'cas-manifest-parse-error
             :text text
             :reason "Manifest entry has invalid field types"))
    (unless (claw-lisp.storage.cas:valid-versioned-hash-p cas-hash)
      (error 'cas-manifest-parse-error
             :text text
             :reason "Manifest entry contains invalid CAS hash"))
    (make-manifest-entry :role (if (keywordp role)
                                   role
                                   (intern (string-upcase role) "KEYWORD"))
                         :cas-hash cas-hash
                         :type (if (keywordp type)
                                   type
                                   (intern (string-upcase type) "KEYWORD"))
                         :metadata metadata)))

(defun compute-manifest-root-digest (entries metadata)
  "Compute deterministic root digest from ENTRIES and METADATA."
  (let* ((*print-circle* nil)
         (*print-readably* t)
         (*print-pretty* nil)
         ;; canonical sexp representation
         (sexp (list :manifest
                     :metadata (claw-lisp.storage.cas:canonicalize-plist metadata)
                     :entries
                     (mapcar (lambda (e)
                               (list :role (manifest-entry-role e)
                                     :cas-hash (manifest-entry-cas-hash e)
                                     :type (manifest-entry-type e)
                                     :metadata (claw-lisp.storage.cas:canonicalize-plist
                                                (manifest-entry-metadata e))))
                             (sort (copy-list entries)
                                   #'string<
                                   :key #'manifest-entry-cas-hash))))
         (text (with-output-to-string (s)
                 (write sexp :stream s))))
    (claw-lisp.storage.cas:cas-hash text)))

(defun make-manifest (&key entries metadata signature)
  "Create a new manifest, computing the root digest automatically.
Entries are sorted by CAS hash to ensure a canonical in-memory state."
  (let ((sorted-entries (sort (copy-list entries) #'string< :key #'manifest-entry-cas-hash)))
    (dolist (e sorted-entries)
      (unless (claw-lisp.storage.cas:valid-versioned-hash-p (manifest-entry-cas-hash e))
        (error "Invalid CAS hash in manifest entry: ~S" e)))
    (let ((root (compute-manifest-root-digest sorted-entries metadata)))
      (%make-manifest :root-digest root
                      :entries sorted-entries
                      :metadata metadata
                      :signature signature))))

(defun serialize-manifest (m)
  "Serialize manifest M to a deterministic string."
  (let ((*print-circle* nil)
        (*print-readably* t)
        (*print-pretty* nil))
    (with-output-to-string (s)
      (%write-json-value
       (list :root-digest (manifest-root-digest m)
             :entries (mapcar (lambda (e)
                                (list :role (manifest-entry-role e)
                                      :cas-hash (manifest-entry-cas-hash e)
                                      :type (manifest-entry-type e)
                                      :metadata (claw-lisp.storage.cas:canonicalize-plist
                                                 (manifest-entry-metadata e))))
                              (manifest-entries m))
             :metadata (claw-lisp.storage.cas:canonicalize-plist (manifest-metadata m))
             :signature (manifest-signature m))
       s))))

(defun deserialize-manifest (text &key (verify-integrity-p t)
                                  (preserve-stored-root-digest-p nil))
  "Deserialize manifest from TEXT and verify its structure.
   When VERIFY-INTEGRITY-P is true, signal cas-manifest-integrity-error if the
   stored root-digest does not match the recomputed one.
   When PRESERVE-STORED-ROOT-DIGEST-P is true, return the stored root digest
   on the manifest object instead of the recomputed digest."
  (let* ((data (%manifest-json-object->plist text)))
    (%expect-json-keys data '(:root-digest :entries :metadata :signature) text)
    (destructuring-bind (&key root-digest entries metadata signature &allow-other-keys) data
      (unless (and (stringp root-digest)
                   (claw-lisp.storage.cas:valid-versioned-hash-p root-digest))
        (error 'cas-manifest-parse-error
               :text text
               :reason "Manifest root digest is missing or invalid"))
      (unless (listp entries)
        (error 'cas-manifest-parse-error
               :text text
               :reason "Manifest entries must be a JSON array"))
      (let* ((entry-objs (mapcar (lambda (e) (%entry-from-json e text)) entries))
             (manifest (make-manifest :entries entry-objs
                                      :metadata metadata
                                      :signature signature)))
        (when (and verify-integrity-p
                   (not (string= root-digest (manifest-root-digest manifest))))
          (error 'cas-manifest-integrity-error
                 :expected root-digest
                 :actual (manifest-root-digest manifest)))
        (when preserve-stored-root-digest-p
          (setf (manifest-root-digest manifest) root-digest))
        manifest))))

(defun store-manifest (cas-root manifest)
  "Serialize MANIFEST and store in CAS. Return its CAS hash."
  (claw-lisp.storage.cas:cas-put cas-root (serialize-manifest manifest)))

(defun load-manifest (cas-root cas-hash &key (verify-integrity-p t)
                                      (verify-signature-p nil)
                                      (preserve-stored-root-digest-p nil))
  "Load manifest from CAS-HASH and deserialize.
   When VERIFY-SIGNATURE-P is true, signal cas-manifest-signature-error if the
   signature is invalid.
   When PRESERVE-STORED-ROOT-DIGEST-P is true, the returned manifest keeps the
   stored root digest instead of the recomputed one."
  (let ((text (claw-lisp.storage.cas:cas-get cas-root cas-hash)))
    (when text
      (let ((manifest (deserialize-manifest text
                                            :verify-integrity-p verify-integrity-p
                                            :preserve-stored-root-digest-p
                                            preserve-stored-root-digest-p)))
        (when (and verify-signature-p
                   (not (claw-lisp.cas.crypto:verify-manifest-root-signature
                         (manifest-root-digest manifest)
                         (manifest-signature manifest))))
          (error 'cas-manifest-signature-error
                 :digest (manifest-root-digest manifest)
                 :signature (manifest-signature manifest)))
        manifest))))

(defun verify-manifest-integrity (manifest)
  "Return T if MANIFEST's root-digest matches recomputed digest."
  (let ((expected (manifest-root-digest manifest))
        (actual (compute-manifest-root-digest
                 (manifest-entries manifest)
                 (manifest-metadata manifest))))
    (string= expected actual)))

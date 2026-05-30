(in-package #:claw-lisp.storage.cas-ref)

(define-condition cas-ref-error (error)
  ()
  (:documentation "Base condition for CAS ref-store errors."))

(define-condition cas-ref-invalid-name-error (cas-ref-error)
  ((name :initarg :name :reader cas-ref-invalid-name-error-name))
  (:report (lambda (condition stream)
             (format stream "Invalid CAS ref name: ~S"
                     (cas-ref-invalid-name-error-name condition)))))

(define-condition cas-ref-invalid-hash-error (cas-ref-error)
  ((name :initarg :name :reader cas-ref-invalid-hash-error-name)
   (hash :initarg :hash :reader cas-ref-invalid-hash-error-hash))
  (:report (lambda (condition stream)
             (format stream "Invalid CAS hash for ref ~A: ~S"
                     (cas-ref-invalid-hash-error-name condition)
                     (cas-ref-invalid-hash-error-hash condition)))))

(define-condition cas-ref-conflict-error (cas-ref-error)
  ((name :initarg :name :reader cas-ref-conflict-error-name)
   (expected :initarg :expected :reader cas-ref-conflict-error-expected)
   (actual :initarg :actual :reader cas-ref-conflict-error-actual))
  (:report (lambda (condition stream)
             (format stream "CAS ref conflict for ~A (expected ~S, actual ~S)"
                     (cas-ref-conflict-error-name condition)
                     (cas-ref-conflict-error-expected condition)
                     (cas-ref-conflict-error-actual condition)))))

(define-condition cas-ref-dangling-error (cas-ref-error)
  ((name :initarg :name :reader cas-ref-dangling-error-name)
   (hash :initarg :hash :reader cas-ref-dangling-error-hash))
  (:report (lambda (condition stream)
             (format stream "CAS ref ~A points to missing object ~A"
                     (cas-ref-dangling-error-name condition)
                     (cas-ref-dangling-error-hash condition)))))

(defun valid-cas-ref-name-p (name)
  (and (stringp name)
       (> (length name) 0)
       (loop for ch across name
             always (or (alphanumericp ch)
                        (member ch '(#\- #\_ #\. #\/) :test #'char=)))
       (not (search ".." name))
       (not (char= (char name 0) #\/))
       (not (char= (char name (1- (length name))) #\/))))

(defun %ensure-valid-cas-ref-name (name)
  (unless (valid-cas-ref-name-p name)
    (error 'cas-ref-invalid-name-error :name name))
  name)

(defun %refs-root-path (ref-root)
  (uiop:ensure-directory-pathname ref-root))

(defun %ref-relative-file (ref-name extension)
  (format nil "refs/~A.~A" ref-name extension))

(defun cas-ref-path (ref-root ref-name)
  (%ensure-valid-cas-ref-name ref-name)
  (merge-pathnames (%ref-relative-file ref-name "ref")
                   (%refs-root-path ref-root)))

(defun %ref-history-path (ref-root ref-name)
  (%ensure-valid-cas-ref-name ref-name)
  (merge-pathnames (format nil "refs-history/~A.history" ref-name)
                   (%refs-root-path ref-root)))

(defun %normalize-ref-relative-name (name)
  (substitute #\/ #\\ name))

(defun %now-universal-time ()
  (get-universal-time))

(defun %read-ref-record (path)
  (when (probe-file path)
    (with-open-file (stream path :direction :input :if-does-not-exist nil)
      (when stream
        (read stream nil nil)))))

(defun %write-ref-record (path record)
  (ensure-directories-exist path)
  (with-open-file (stream path
                          :direction :output
                          :if-exists :supersede
                          :if-does-not-exist :create)
    (let ((*print-circle* nil)
          (*print-readably* t)
          (*print-pretty* nil))
      (write (claw-lisp.storage.cas:canonicalize-plist record) :stream stream))
    (terpri stream)
    (finish-output stream)))

(defun %append-ref-history (path record)
  (ensure-directories-exist path)
  (with-open-file (stream path
                          :direction :output
                          :if-exists :append
                          :if-does-not-exist :create)
    (let ((*print-circle* nil)
          (*print-readably* t)
          (*print-pretty* nil))
      (write (claw-lisp.storage.cas:canonicalize-plist record) :stream stream))
    (terpri stream)
    (finish-output stream)))

(defun read-cas-ref (ref-root ref-name)
  "Return ref record for REF-NAME, or NIL when absent."
  (%read-ref-record (cas-ref-path ref-root ref-name)))

(defun write-cas-ref (ref-root ref-name cas-hash
                      &key expected-current-hash record-history-p metadata)
  "Create/update REF-NAME to CAS-HASH and return the new ref record.
When EXPECTED-CURRENT-HASH is non-NIL, mismatch signals cas-ref-conflict-error."
  (%ensure-valid-cas-ref-name ref-name)
  (unless (claw-lisp.storage.cas:valid-versioned-hash-p cas-hash)
    (error 'cas-ref-invalid-hash-error :name ref-name :hash cas-hash))
  (let* ((ref-path (cas-ref-path ref-root ref-name))
         (old-record (%read-ref-record ref-path))
         (old-hash (and old-record (getf old-record :cas-hash))))
    (when (and expected-current-hash
               (not (equal expected-current-hash old-hash)))
      (error 'cas-ref-conflict-error
             :name ref-name
             :expected expected-current-hash
             :actual old-hash))
    (let ((new-record (list :name ref-name
                            :cas-hash cas-hash
                            :updated-at-universal-time (%now-universal-time)
                            :version (1+ (or (and old-record
                                                  (getf old-record :version))
                                             0))
                            :metadata (claw-lisp.storage.cas:canonicalize-plist metadata))))
      (%write-ref-record ref-path new-record)
      (when record-history-p
        (%append-ref-history (%ref-history-path ref-root ref-name) new-record))
      new-record)))

(defun delete-cas-ref (ref-root ref-name)
  "Delete REF-NAME. Return T if deleted, NIL when absent."
  (let ((path (cas-ref-path ref-root ref-name)))
    (when (probe-file path)
      (delete-file path)
      t)))

(defun list-cas-refs (ref-root)
  "Return sorted list of ref names present in REF-ROOT."
  (let* ((refs-dir (merge-pathnames
                    (make-pathname :directory '(:relative "refs"))
                    (%refs-root-path ref-root))))
    (unless (probe-file refs-dir)
      (return-from list-cas-refs nil))
    (sort
     (labels ((collect (dir)
                (append
                 (loop for path in (uiop:directory-files dir)
                       for type = (pathname-type path)
                       when (and type (string= type "ref"))
                         collect (let ((rel (enough-namestring path refs-dir)))
                                   (%normalize-ref-relative-name
                                    (subseq rel 0 (- (length rel) 4)))))
                 (loop for subdir in (uiop:subdirectories dir)
                       append (collect subdir)))))
       (collect refs-dir))
     #'string<)))

(defun resolve-cas-ref (ref-root cas-root ref-name &key require-object-p)
  "Resolve REF-NAME to CAS hash.
When REQUIRE-OBJECT-P is true, signal cas-ref-dangling-error if object is absent."
  (let* ((record (read-cas-ref ref-root ref-name))
         (cas-hash (and record (getf record :cas-hash))))
    (when (null cas-hash)
      (return-from resolve-cas-ref nil))
    (when (and require-object-p
               (not (cas-exists-p cas-root cas-hash)))
      (error 'cas-ref-dangling-error :name ref-name :hash cas-hash))
    cas-hash))

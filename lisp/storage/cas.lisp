(in-package #:claw-lisp.storage.cas)

(defparameter +hash-algorithm-prefix+ "sha256")
(defparameter +hash-algorithm-digest-hex-lengths+
  '(("sha256" . 64)))

;; NOTE ON PORTABILITY:
;; This module currently targets SBCL as the operational baseline. Use wrapper
;; helpers for implementation-specific functionality so non-SBCL fallbacks can
;; be added in one place if/when multi-implementation support is required.

(define-condition cas-error (error)
  ()
  (:documentation "Base condition for CAS-related errors."))

(define-condition cas-invalid-hash-error (cas-error)
  ((hash :initarg :hash :reader cas-invalid-hash-error-hash))
  (:report (lambda (condition stream)
             (format stream "Invalid CAS hash format: ~S"
                     (cas-invalid-hash-error-hash condition)))))

(define-condition cas-write-error (cas-error)
  ((path :initarg :path :reader cas-write-error-path)
   (cause :initarg :cause :reader cas-write-error-cause))
  (:report (lambda (condition stream)
             (format stream "CAS write failed for ~A: ~A"
                     (cas-write-error-path condition)
                     (cas-write-error-cause condition)))))

;;; ============================================================
;;; Hash Utilities
;;; ============================================================

(defun %string-to-utf8-octets (content)
  "Encode CONTENT to UTF-8 octets.
Current implementation is SBCL-specific via SB-EXT."
  #+sbcl
  (sb-ext:string-to-octets content :external-format :utf-8)
  #-sbcl
  (error "CAS UTF-8 encoding fallback not implemented for this Lisp: ~A"
         (lisp-implementation-type)))

(defun %process-id ()
  "Return current process id.
Current implementation is SBCL-specific via SB-POSIX."
  #+sbcl
  (sb-posix:getpid)
  #-sbcl
  (error "CAS process-id fallback not implemented for this Lisp: ~A"
         (lisp-implementation-type)))

(defun cas-hash (content)
  (let* ((octets (%string-to-utf8-octets content))
         (digest (ironclad:digest-sequence :sha256 octets))
         (hex (ironclad:byte-array-to-hex-string digest)))
    (concatenate 'string +hash-algorithm-prefix+ ":" hex)))

(defun cas-hash-bytes (octets)
  (unless (typep octets '(array (unsigned-byte 8) (*)))
    (error "cas-hash-bytes expects an (array (unsigned-byte 8) (*)), got ~S"
           (type-of octets)))
  (let* ((digest (ironclad:digest-sequence :sha256 octets))
         (hex (ironclad:byte-array-to-hex-string digest)))
    (concatenate 'string +hash-algorithm-prefix+ ":" hex)))

(defun canonicalize-plist (plist)
  "Return a new plist with keys sorted alphabetically by symbol name.
Ensures deterministic serialization for hashes and storage records."
  (let ((pairs '()))
    (loop for (key value) on plist by #'cddr
          do (push (cons key value) pairs))
    (setf pairs (sort pairs #'string< :key (lambda (p) (symbol-name (car p)))))
    (loop for (key . value) in pairs
          append (list key value))))

(defun parse-versioned-hash (versioned-hash)
  (let ((pos (position #\: versioned-hash)))
    (if (and pos (> pos 0) (< (1+ pos) (length versioned-hash)))
        (values (subseq versioned-hash 0 pos)
                (subseq versioned-hash (1+ pos)))
        (values nil nil))))

(defun hash-algorithm (versioned-hash)
  (nth-value 0 (parse-versioned-hash versioned-hash)))

(defun hash-digest (versioned-hash)
  (nth-value 1 (parse-versioned-hash versioned-hash)))

(defun hash-shard-prefix (versioned-hash)
  (let ((digest (hash-digest versioned-hash)))
    (when (and digest (>= (length digest) 2))
      (subseq digest 0 2))))

(defun hash-shard-remainder (versioned-hash)
  (let ((digest (hash-digest versioned-hash)))
    (when (and digest (> (length digest) 2))
      (subseq digest 2))))

(defun %hex-char-p (ch)
  (or (and (char>= ch #\0) (char<= ch #\9))
      (and (char>= ch #\a) (char<= ch #\f))))

(defun valid-versioned-hash-p (versioned-hash)
  (and (stringp versioned-hash)
       (multiple-value-bind (algorithm digest)
           (parse-versioned-hash versioned-hash)
         (let ((expected-digest-length (cdr (assoc algorithm
                                                   +hash-algorithm-digest-hex-lengths+
                                                   :test #'string=))))
           (and (string= algorithm +hash-algorithm-prefix+)
                expected-digest-length
                (= (length digest) expected-digest-length)
                (loop for ch across digest
                      always (%hex-char-p ch)))))))

(defun %ensure-valid-versioned-hash (versioned-hash)
  (unless (valid-versioned-hash-p versioned-hash)
    (error 'cas-invalid-hash-error :hash versioned-hash))
  versioned-hash)

;;; ============================================================
;;; Object Store — Path Resolution
;;; ============================================================

(defun cas-object-path (cas-root versioned-hash)
  (%ensure-valid-versioned-hash versioned-hash)
  (let ((prefix (hash-shard-prefix versioned-hash))
        (remainder (hash-shard-remainder versioned-hash)))
    (merge-pathnames
     (make-pathname :directory `(:relative ,prefix)
                    :name remainder)
     (uiop:ensure-directory-pathname cas-root))))

;;; ============================================================
;;; Object Store — Atomic Write
;;; ============================================================

(defun %temp-object-path (final-path)
  (let ((dir (uiop:pathname-directory-pathname final-path))
        (temp-name (format nil ".cas-tmp-~D-~D-~A"
                           (%process-id)
                           (get-internal-real-time)
                           (ironclad:byte-array-to-hex-string
                            (ironclad:random-data 8)))))
    (merge-pathnames (make-pathname :name temp-name) dir)))

(defun %absolute-pathname (path)
  "Resolve PATH against the current working directory when it is relative."
  (merge-pathnames path (uiop:ensure-directory-pathname (uiop:getcwd))))

(defun %cas-temp-file-p (path)
  (let ((name (pathname-name path)))
    (and (stringp name)
         (>= (length name) 8)
         (string= ".cas-tmp" name :end1 8 :end2 8))))

(defun %collect-cas-temp-files (root &key (recursive-p t))
  (labels ((collect-from-dir (dir)
             (let ((files '()))
               (dolist (file (uiop:directory-files dir))
                 (when (%cas-temp-file-p file)
                   (push file files)))
               (when recursive-p
                 (dolist (subdir (uiop:subdirectories dir))
                   (setf files (nconc files (collect-from-dir subdir)))))
               files)))
    (collect-from-dir (uiop:ensure-directory-pathname root))))

(defun cas-cleanup-temp-files (cas-root &key (recursive-p t))
  "Delete orphaned CAS temporary files under CAS-ROOT.
These files may remain after abrupt process termination (e.g., SIGKILL).
Returns the number of files successfully deleted."
  (let ((root (uiop:ensure-directory-pathname cas-root))
        (deleted 0))
    (unless (probe-file root)
      (return-from cas-cleanup-temp-files 0))
    (dolist (path (%collect-cas-temp-files root :recursive-p recursive-p))
      (when (probe-file path)
        (ignore-errors
          (delete-file path)
          (incf deleted))))
    deleted))

(defun %write-object-atomically (object-path writer-fn)
  (let ((object-path (%absolute-pathname object-path)))
    (ensure-directories-exist object-path)
  (handler-case
        (let ((temp-path (%temp-object-path object-path)))
        (unwind-protect
            (progn
              (funcall writer-fn temp-path)
              (handler-case
                  (rename-file temp-path object-path)
                (file-error (rename-error)
                  ;; Some container filesystems reject atomic rename across
                  ;; the temp/object path boundary. Fall back to a plain copy
                  ;; so CAS writes still succeed in Docker validation.
                  (warn "CAS rename failed for ~A; falling back to copy: ~A"
                        object-path rename-error)
                  (handler-case
                      (uiop:copy-file temp-path object-path)
                    (file-error (copy-error)
                      (error 'cas-write-error
                             :path object-path
                             :cause (list :rename rename-error
                                          :copy copy-error))))
                  (ignore-errors (delete-file temp-path)))))
          (when (probe-file temp-path)
            (ignore-errors (delete-file temp-path)))))
      (error (e)
        (error 'cas-write-error :path object-path :cause e)))))

(defun cas-put (cas-root content)
  "Store UTF-8 text CONTENT in CAS and return its versioned hash.
Temporary files are cleaned up on normal unwind; abrupt termination can
leave orphan `.cas-tmp-*` files. Run `cas-cleanup-temp-files` periodically."
  (let* ((versioned-hash (cas-hash content))
         (object-path (cas-object-path cas-root versioned-hash)))
    (when (probe-file object-path)
      (return-from cas-put versioned-hash))
    (%write-object-atomically
     object-path
     (lambda (temp-path)
       (with-open-file (stream temp-path
                               :direction :output
                               :if-exists :supersede
                               :if-does-not-exist :create
                               :element-type 'character
                               :external-format :utf-8)
         (write-string content stream)
         (finish-output stream))))
    versioned-hash))

(defun cas-put-bytes (cas-root octets)
  "Store OCTETS in CAS and return its versioned hash."
  (unless (typep octets '(array (unsigned-byte 8) (*)))
    (error "cas-put-bytes expects an (array (unsigned-byte 8) (*)), got ~S"
           (type-of octets)))
  (let* ((versioned-hash (cas-hash-bytes octets))
         (object-path (cas-object-path cas-root versioned-hash)))
    (when (probe-file object-path)
      (return-from cas-put-bytes versioned-hash))
    (%write-object-atomically
     object-path
     (lambda (temp-path)
       (with-open-file (stream temp-path
                               :direction :output
                               :if-exists :supersede
                               :if-does-not-exist :create
                               :element-type '(unsigned-byte 8))
         (write-sequence octets stream)
         (finish-output stream))))
    versioned-hash))

;;; ============================================================
;;; Object Store — Read
;;; ============================================================

(defun cas-get (cas-root versioned-hash)
  "Read UTF-8 text content from CAS for VERSIONED-HASH, or NIL if absent.
This API is text-only; use a separate byte-oriented API for binary payloads."
  (let ((object-path (cas-object-path cas-root versioned-hash)))
    (when (probe-file object-path)
      (uiop:read-file-string object-path))))

(defun cas-get-bytes (cas-root versioned-hash)
  "Read CAS object OCTETS for VERSIONED-HASH, or NIL if absent."
  (let ((object-path (cas-object-path cas-root versioned-hash)))
    (when (probe-file object-path)
      (with-open-file (stream object-path
                              :direction :input
                              :element-type '(unsigned-byte 8))
        (let* ((size (file-length stream))
               (octets (make-array size :element-type '(unsigned-byte 8))))
          (read-sequence octets stream)
          octets)))))

;;; ============================================================
;;; Object Store — Delete
;;; ============================================================

(defun cas-delete (cas-root versioned-hash)
  "Delete CAS object for VERSIONED-HASH.
Return T if an object was deleted, NIL if it did not exist."
  (let ((object-path (cas-object-path cas-root versioned-hash)))
    (when (probe-file object-path)
      (delete-file object-path)
      t)))

;;; ============================================================
;;; Object Store — Existence Check
;;; ============================================================

(defun cas-exists-p (cas-root versioned-hash)
  "Return T when VERSIONED-HASH exists in CAS, otherwise NIL."
  (and (probe-file (cas-object-path cas-root versioned-hash)) t))

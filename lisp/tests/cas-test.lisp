(in-package #:claw-lisp.tests)

;;; ============================================================
;;; Test Isolation
;;; ============================================================

(defmacro %with-temp-cas-root (var &body body)
  `(let ((,var (merge-pathnames
                (format nil "cas-test-~D-~D/"
                        (sb-posix:getpid)
                        (get-internal-real-time))
                #P"/tmp/")))
     (unwind-protect
         (progn
           (ensure-directories-exist
            (uiop:ensure-directory-pathname ,var))
           ,@body)
       (when (probe-file ,var)
         (uiop:delete-directory-tree
          (uiop:ensure-directory-pathname ,var) :validate t)))))

;;; ============================================================
;;; Hash Utility Tests
;;; ============================================================

(defun test-cas-hash-deterministic ()
  (let ((h1 (claw-lisp.storage.cas:cas-hash "hello world"))
        (h2 (claw-lisp.storage.cas:cas-hash "hello world")))
    (%assert (string= h1 h2)
             "Same content should produce same hash: ~A vs ~A" h1 h2))
  (format t "~&+ test-cas-hash-deterministic passed~%")
  t)

(defun test-cas-hash-different-content ()
  (let ((h1 (claw-lisp.storage.cas:cas-hash "hello"))
        (h2 (claw-lisp.storage.cas:cas-hash "world")))
    (%assert (not (string= h1 h2))
             "Different content should produce different hashes"))
  (format t "~&+ test-cas-hash-different-content passed~%")
  t)

(defun test-cas-hash-format ()
  (let ((h (claw-lisp.storage.cas:cas-hash "test")))
    (%assert (eql 0 (search "sha256:" h))
             "Hash should start with sha256: prefix")
    (%assert (= 71 (length h))
             "Hash should be 71 chars (sha256: + 64 hex), got ~D" (length h)))
  (format t "~&+ test-cas-hash-format passed~%")
  t)

(defun test-cas-hash-known-value ()
  (let ((h (claw-lisp.storage.cas:cas-hash "")))
    (%assert (string= h "sha256:e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855")
             "Empty string SHA-256 mismatch: ~A" h))
  (format t "~&+ test-cas-hash-known-value passed~%")
  t)

(defun test-cas-hash-bytes-contract ()
  (let* ((octets (sb-ext:string-to-octets "hello world" :external-format :utf-8))
         (h1 (claw-lisp.storage.cas:cas-hash "hello world"))
         (h2 (claw-lisp.storage.cas:cas-hash-bytes octets)))
    (%assert (string= h1 h2)
             "String hash and byte hash should match: ~A vs ~A" h1 h2))
  (handler-case
      (progn
        (claw-lisp.storage.cas:cas-hash-bytes "hello world")
        (%assert nil "Expected cas-hash-bytes to reject non-byte input"))
    (error () nil))
  (format t "~&+ test-cas-hash-bytes-contract passed~%")
  t)

(defun test-cas-parse-versioned-hash ()
  (multiple-value-bind (algo digest)
      (claw-lisp.storage.cas:parse-versioned-hash "sha256:abcdef1234567890")
    (%assert (string= algo "sha256")
             "Algorithm should be sha256, got ~A" algo)
    (%assert (string= digest "abcdef1234567890")
             "Digest should be abcdef1234567890, got ~A" digest))
  (multiple-value-bind (algo digest)
      (claw-lisp.storage.cas:parse-versioned-hash "nocolon")
    (%assert (null algo) "Invalid hash should return NIL algorithm")
    (%assert (null digest) "Invalid hash should return NIL digest"))
  (format t "~&+ test-cas-parse-versioned-hash passed~%")
  t)

(defun test-cas-shard-prefix-and-remainder ()
  (let ((h "sha256:abcdef1234567890abcdef1234567890abcdef1234567890abcdef1234567890"))
    (%assert (string= "ab" (claw-lisp.storage.cas:hash-shard-prefix h))
             "Shard prefix should be 'ab'")
    (%assert (string= "cdef1234567890abcdef1234567890abcdef1234567890abcdef1234567890"
                       (claw-lisp.storage.cas:hash-shard-remainder h))
             "Shard remainder mismatch"))
  (format t "~&+ test-cas-shard-prefix-and-remainder passed~%")
  t)

;;; ============================================================
;;; Object Store Tests
;;; ============================================================

(defun test-cas-put-and-get-roundtrip ()
  (%with-temp-cas-root cas-root
    (let* ((content "Hello, CAS world!")
           (hash (claw-lisp.storage.cas:cas-put cas-root content))
           (retrieved (claw-lisp.storage.cas:cas-get cas-root hash)))
      (%assert (string= content retrieved)
               "Retrieved content should match original: got ~S" retrieved)))
  (format t "~&+ test-cas-put-and-get-roundtrip passed~%")
  t)

(defun test-cas-put-deduplication ()
  (%with-temp-cas-root cas-root
    (let* ((content "duplicate me")
           (h1 (claw-lisp.storage.cas:cas-put cas-root content))
           (h2 (claw-lisp.storage.cas:cas-put cas-root content)))
      (%assert (string= h1 h2)
               "Duplicate put should return same hash")))
  (format t "~&+ test-cas-put-deduplication passed~%")
  t)

(defun test-cas-exists-p ()
  (%with-temp-cas-root cas-root
    (let ((hash (claw-lisp.storage.cas:cas-put cas-root "exists test")))
      (%assert (claw-lisp.storage.cas:cas-exists-p cas-root hash)
               "Object should exist after put")
      (%assert (not (claw-lisp.storage.cas:cas-exists-p
                     cas-root "sha256:0000000000000000000000000000000000000000000000000000000000000000"))
               "Nonexistent hash should return NIL")))
  (format t "~&+ test-cas-exists-p passed~%")
  t)

(defun test-cas-delete ()
  (%with-temp-cas-root cas-root
    (let ((hash (claw-lisp.storage.cas:cas-put cas-root "delete me")))
      (%assert (claw-lisp.storage.cas:cas-exists-p cas-root hash)
               "Object should exist before delete")
      (%assert (claw-lisp.storage.cas:cas-delete cas-root hash)
               "First delete should return T")
      (%assert (not (claw-lisp.storage.cas:cas-exists-p cas-root hash))
               "Object should not exist after delete")
      (%assert (not (claw-lisp.storage.cas:cas-delete cas-root hash))
               "Second delete should return NIL")))
  (format t "~&+ test-cas-delete passed~%")
  t)

(defun test-cas-get-nonexistent ()
  (%with-temp-cas-root cas-root
    (let ((result (claw-lisp.storage.cas:cas-get
                   cas-root
                   "sha256:0000000000000000000000000000000000000000000000000000000000000000")))
      (%assert (null result)
               "Get of nonexistent hash should return NIL")))
  (format t "~&+ test-cas-get-nonexistent passed~%")
  t)

(defun test-cas-object-path-sharding ()
  (let* ((hash "sha256:abcdef1234567890abcdef1234567890abcdef1234567890abcdef1234567890")
         (path (claw-lisp.storage.cas:cas-object-path "/tmp/cas/" hash))
         (ns (namestring path)))
    (%assert (search "/ab/" ns)
             "Path should contain shard directory /ab/: ~A" ns)
    (%assert (search "cdef1234567890abcdef1234567890abcdef1234567890abcdef1234567890" ns)
             "Path should contain remainder as filename: ~A" ns))
  (format t "~&+ test-cas-object-path-sharding passed~%")
  t)

(defun test-cas-put-creates-shard-directory ()
  (%with-temp-cas-root cas-root
    (let* ((hash (claw-lisp.storage.cas:cas-put cas-root "shard test"))
           (prefix (claw-lisp.storage.cas:hash-shard-prefix hash))
           (shard-dir (merge-pathnames
                       (make-pathname :directory `(:relative ,prefix))
                       (uiop:ensure-directory-pathname cas-root))))
      (%assert (probe-file shard-dir)
               "Shard directory ~A should exist after put" shard-dir)))
  (format t "~&+ test-cas-put-creates-shard-directory passed~%")
  t)

(defun test-cas-empty-content ()
  (%with-temp-cas-root cas-root
    (let* ((hash (claw-lisp.storage.cas:cas-put cas-root ""))
           (retrieved (claw-lisp.storage.cas:cas-get cas-root hash)))
      (%assert (string= "" retrieved)
               "Empty content should roundtrip correctly")))
  (format t "~&+ test-cas-empty-content passed~%")
  t)

(defun test-cas-large-content ()
  (%with-temp-cas-root cas-root
    (let* ((content (make-string 102400 :initial-element #\x))
           (hash (claw-lisp.storage.cas:cas-put cas-root content))
           (retrieved (claw-lisp.storage.cas:cas-get cas-root hash)))
      (%assert (string= content retrieved)
               "Large content should roundtrip correctly")
      (%assert (= (length content) (length retrieved))
               "Retrieved length should match: ~D vs ~D"
               (length content) (length retrieved))))
  (format t "~&+ test-cas-large-content passed~%")
  t)

(defun test-cas-unicode-content ()
  (%with-temp-cas-root cas-root
    (let* ((content "Hello, world! 日本語テスト Farsi: سلام")
           (hash (claw-lisp.storage.cas:cas-put cas-root content))
           (retrieved (claw-lisp.storage.cas:cas-get cas-root hash)))
      (%assert (string= content retrieved)
               "Unicode content should roundtrip correctly")))
  (format t "~&+ test-cas-unicode-content passed~%")
  t)

(defun test-cas-put-bytes-and-get-bytes-roundtrip ()
  (%with-temp-cas-root cas-root
    (let* ((octets (make-array 6 :element-type '(unsigned-byte 8)
                               :initial-contents '(0 1 2 253 254 255)))
           (hash (claw-lisp.storage.cas:cas-put-bytes cas-root octets))
           (retrieved (claw-lisp.storage.cas:cas-get-bytes cas-root hash)))
      (%assert (equalp octets retrieved)
               "Binary roundtrip mismatch: ~S vs ~S" octets retrieved)
      (%assert (typep retrieved '(array (unsigned-byte 8) (*)))
               "Expected byte-array result, got ~S" (type-of retrieved))))
  (format t "~&+ test-cas-put-bytes-and-get-bytes-roundtrip passed~%")
  t)

(defun test-cas-put-bytes-rejects-non-octets ()
  (%with-temp-cas-root cas-root
    (handler-case
        (progn
          (claw-lisp.storage.cas:cas-put-bytes cas-root "not-bytes")
          (%assert nil "Expected cas-put-bytes to reject string input"))
      (error () nil)))
  (format t "~&+ test-cas-put-bytes-rejects-non-octets passed~%")
  t)

#+sb-thread
(defun test-cas-concurrent-put-and-exists-stress ()
  (%with-temp-cas-root cas-root
    (let* ((content "concurrent-shared-payload")
           (expected-hash (claw-lisp.storage.cas:cas-hash content))
           (errors nil)
           (errors-lock (sb-thread:make-mutex :name "cas-concurrency-errors"))
           (threads
             (loop repeat 20
                   collect (sb-thread:make-thread
                            (lambda ()
                              (handler-case
                                  (loop repeat 50 do
                                    (let ((hash (claw-lisp.storage.cas:cas-put cas-root content)))
                                      (%assert (claw-lisp.storage.cas:cas-exists-p cas-root hash)
                                               "Expected hash to exist during concurrent writes: ~A"
                                               hash)))
                                (error (e)
                                  (sb-thread:with-mutex (errors-lock)
                                    (push (princ-to-string e) errors)))))))))
      (dolist (thread threads)
        (sb-thread:join-thread thread))
      (%assert (null errors)
               "Concurrent CAS writer threads reported errors: ~S" errors)
      (%assert (claw-lisp.storage.cas:cas-exists-p cas-root expected-hash)
               "Expected shared hash to exist after concurrent writes: ~A" expected-hash)
      (%assert (string= content
                        (claw-lisp.storage.cas:cas-get cas-root expected-hash))
               "Concurrent final content mismatch for shared hash ~A"
               expected-hash)))
  (format t "~&+ test-cas-concurrent-put-and-exists-stress passed~%")
  t)

(defun test-cas-config-accessor ()
  (let ((config (claw-lisp.config:make-default-runtime-config)))
    (%assert (string= ".achatina/cas/objects/"
                       (claw-lisp.config:runtime-config-cas-objects-root config))
             "Default CAS objects root mismatch"))
  (format t "~&+ test-cas-config-accessor passed~%")
  t)

(defun test-cas-invalid-versioned-hash-rejected ()
  (%with-temp-cas-root cas-root
    (dolist (bad-hash (list
                       "sha256:../../etc/passwd"
                       "sha256:ABCDEF1234567890abcdef1234567890abcdef1234567890abcdef1234567890"
                       "sha256:abc"
                       "md5:0123456789012345678901234567890123456789012345678901234567890123"))
      (handler-case
          (progn
            (claw-lisp.storage.cas:cas-object-path cas-root bad-hash)
            (%assert nil "Expected invalid hash rejection for ~S" bad-hash))
        (claw-lisp.storage.cas:cas-invalid-hash-error () nil))
      (handler-case
          (progn
            (claw-lisp.storage.cas:cas-get cas-root bad-hash)
            (%assert nil "Expected cas-get rejection for ~S" bad-hash))
        (claw-lisp.storage.cas:cas-invalid-hash-error () nil))
      (handler-case
          (progn
            (claw-lisp.storage.cas:cas-delete cas-root bad-hash)
            (%assert nil "Expected cas-delete rejection for ~S" bad-hash))
        (claw-lisp.storage.cas:cas-invalid-hash-error () nil))
      (handler-case
          (progn
            (claw-lisp.storage.cas:cas-exists-p cas-root bad-hash)
            (%assert nil "Expected cas-exists-p rejection for ~S" bad-hash))
        (claw-lisp.storage.cas:cas-invalid-hash-error () nil))))
  (format t "~&+ test-cas-invalid-versioned-hash-rejected passed~%")
  t)

(defun test-cas-cleanup-temp-files ()
  (%with-temp-cas-root cas-root
    (let* ((root (uiop:ensure-directory-pathname cas-root))
           (subdir (merge-pathnames "ab/" root))
           (tmp-1 (merge-pathnames ".cas-tmp-orphan-1" root))
           (tmp-2 (merge-pathnames ".cas-tmp-orphan-2" subdir))
           (keep (merge-pathnames "keep.txt" subdir)))
      (ensure-directories-exist subdir)
      (with-open-file (stream tmp-1 :direction :output :if-exists :supersede :if-does-not-exist :create)
        (write-line "orphan" stream))
      (with-open-file (stream tmp-2 :direction :output :if-exists :supersede :if-does-not-exist :create)
        (write-line "orphan" stream))
      (with-open-file (stream keep :direction :output :if-exists :supersede :if-does-not-exist :create)
        (write-line "keep" stream))
      (%assert (= 2 (claw-lisp.storage.cas:cas-cleanup-temp-files root))
               "Expected cleanup to delete exactly two orphan temp files")
      (%assert (not (probe-file tmp-1))
               "Expected root-level orphan temp file to be deleted")
      (%assert (not (probe-file tmp-2))
               "Expected nested orphan temp file to be deleted")
      (%assert (probe-file keep)
               "Expected non-temp file to remain after cleanup")
      (%assert (= 0 (claw-lisp.storage.cas:cas-cleanup-temp-files root))
               "Expected no temp files on second cleanup pass")))
  (format t "~&+ test-cas-cleanup-temp-files passed~%")
  t)

(defun test-cas-atomic-write-writer-failure ()
  (%with-temp-cas-root cas-root
    (let* ((hash (claw-lisp.storage.cas:cas-hash "writer-failure"))
           (object-path (claw-lisp.storage.cas:cas-object-path cas-root hash))
           (signaled nil))
      (handler-case
          (claw-lisp.storage.cas::%write-object-atomically
           object-path
           (lambda (_temp-path)
             (declare (ignore _temp-path))
             (error "simulated disk-full writer failure")))
        (claw-lisp.storage.cas:cas-write-error ()
          (setf signaled t)))
      (%assert signaled
               "Expected cas-write-error on writer failure")
      (%assert (= 0 (claw-lisp.storage.cas:cas-cleanup-temp-files cas-root))
               "Expected no orphan temp files after writer failure cleanup")))
  (format t "~&+ test-cas-atomic-write-writer-failure passed~%")
  t)

(defun test-cas-atomic-write-rename-failure ()
  (%with-temp-cas-root cas-root
    (let* ((hash (claw-lisp.storage.cas:cas-hash "rename-failure"))
           (object-path (claw-lisp.storage.cas:cas-object-path cas-root hash))
           (signaled nil))
      (handler-case
          (claw-lisp.storage.cas::%write-object-atomically
           object-path
           (lambda (temp-path)
             (with-open-file (stream temp-path
                                     :direction :output
                                     :if-exists :supersede
                                     :if-does-not-exist :create
                                     :element-type 'character
                                     :external-format :utf-8)
               (write-string "content" stream)
               (finish-output stream))
             ;; Force rename-file to fail by removing the temp source first.
             (delete-file temp-path)))
        (claw-lisp.storage.cas:cas-write-error ()
          (setf signaled t)))
      (%assert signaled
               "Expected cas-write-error on rename failure")
      (%assert (= 0 (claw-lisp.storage.cas:cas-cleanup-temp-files cas-root))
               "Expected no orphan temp files after rename failure cleanup")))
  (format t "~&+ test-cas-atomic-write-rename-failure passed~%")
  t)

;;; ============================================================
;;; Test Runner
;;; ============================================================

(defun run-cas-tests ()
  (format t "~&=== Phase 10 CAS Core Object Store Tests ===~%")
  (test-cas-hash-deterministic)
  (test-cas-hash-different-content)
  (test-cas-hash-format)
  (test-cas-hash-known-value)
  (test-cas-hash-bytes-contract)
  (test-cas-parse-versioned-hash)
  (test-cas-shard-prefix-and-remainder)
  (test-cas-put-and-get-roundtrip)
  (test-cas-put-deduplication)
  (test-cas-exists-p)
  (test-cas-delete)
  (test-cas-get-nonexistent)
  (test-cas-object-path-sharding)
  (test-cas-put-creates-shard-directory)
  (test-cas-empty-content)
  (test-cas-large-content)
  (test-cas-unicode-content)
  (test-cas-put-bytes-and-get-bytes-roundtrip)
  (test-cas-put-bytes-rejects-non-octets)
  #+sb-thread
  (test-cas-concurrent-put-and-exists-stress)
  (test-cas-config-accessor)
  (test-cas-invalid-versioned-hash-rejected)
  (test-cas-cleanup-temp-files)
  (test-cas-atomic-write-writer-failure)
  (test-cas-atomic-write-rename-failure)
  (format t "~&=== All CAS tests passed ===~%"))

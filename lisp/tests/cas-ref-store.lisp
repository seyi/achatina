(in-package #:claw-lisp.tests)

(defmacro %with-temp-cas-ref-root (cas-root ref-root &body body)
  `(let* ((,cas-root (merge-pathnames
                      (format nil "cas-ref-cas-~D-~D/"
                              (sb-posix:getpid)
                              (get-internal-real-time))
                      #P"/tmp/"))
          (,ref-root (merge-pathnames
                      (format nil "cas-ref-store-~D-~D/"
                              (sb-posix:getpid)
                              (get-internal-real-time))
                      #P"/tmp/")))
     (unwind-protect
          (progn
            (ensure-directories-exist (uiop:ensure-directory-pathname ,cas-root))
            (ensure-directories-exist (uiop:ensure-directory-pathname ,ref-root))
            ,@body)
       (when (probe-file ,cas-root)
         (uiop:delete-directory-tree (uiop:ensure-directory-pathname ,cas-root)
                                     :validate t))
       (when (probe-file ,ref-root)
         (uiop:delete-directory-tree (uiop:ensure-directory-pathname ,ref-root)
                                     :validate t)))))

(defun test-cas-ref-write-read-roundtrip ()
  (%with-temp-cas-ref-root cas-root ref-root
    (let* ((hash (claw-lisp.storage.cas:cas-put cas-root "ref-target"))
           (record (claw-lisp.storage.cas-ref:write-cas-ref
                    ref-root "session/latest" hash))
           (read-back (claw-lisp.storage.cas-ref:read-cas-ref
                       ref-root "session/latest")))
      (%assert (equal hash (getf record :cas-hash))
               "Expected written ref hash to match")
      (%assert (equal hash (getf read-back :cas-hash))
               "Expected read ref hash to match")
      (%assert (= 1 (getf read-back :version))
               "Expected initial ref version to be 1")))
  (format t "~&+ test-cas-ref-write-read-roundtrip passed~%")
  t)

(defun test-cas-ref-update-and-history ()
  (%with-temp-cas-ref-root cas-root ref-root
    (let* ((hash-a (claw-lisp.storage.cas:cas-put cas-root "a"))
           (hash-b (claw-lisp.storage.cas:cas-put cas-root "b"))
           (first (claw-lisp.storage.cas-ref:write-cas-ref
                   ref-root "artifact/current" hash-a :record-history-p t))
           (second (claw-lisp.storage.cas-ref:write-cas-ref
                    ref-root "artifact/current" hash-b
                    :expected-current-hash hash-a
                    :record-history-p t)))
      (%assert (= 1 (getf first :version))
               "Expected first ref version 1")
      (%assert (= 2 (getf second :version))
               "Expected second ref version 2")
      (let ((history-path (merge-pathnames
                           #P"refs-history/artifact/current.history"
                           (uiop:ensure-directory-pathname ref-root))))
        (%assert (probe-file history-path)
                 "Expected ref history file to exist")
        (let ((lines (uiop:read-file-lines history-path)))
          (%assert (= 2 (length lines))
                   "Expected two history entries, got ~D" (length lines))))))
  (format t "~&+ test-cas-ref-update-and-history passed~%")
  t)

(defun test-cas-ref-conflict-detection ()
  (%with-temp-cas-ref-root cas-root ref-root
    (let ((hash-a (claw-lisp.storage.cas:cas-put cas-root "a"))
          (hash-b (claw-lisp.storage.cas:cas-put cas-root "b"))
          (conflict-signaled nil))
      (claw-lisp.storage.cas-ref:write-cas-ref ref-root "active" hash-a)
      (handler-case
          (claw-lisp.storage.cas-ref:write-cas-ref
           ref-root "active" hash-b :expected-current-hash "sha256:0000000000000000000000000000000000000000000000000000000000000000")
        (claw-lisp.storage.cas-ref:cas-ref-conflict-error ()
          (setf conflict-signaled t)))
      (%assert conflict-signaled
               "Expected cas-ref-conflict-error on stale expected hash")))
  (format t "~&+ test-cas-ref-conflict-detection passed~%")
  t)

(defun test-cas-ref-dangling-resolution ()
  (%with-temp-cas-ref-root cas-root ref-root
    (let* ((hash (claw-lisp.storage.cas:cas-put cas-root "to-be-removed"))
           (dangling-signaled nil))
      (claw-lisp.storage.cas-ref:write-cas-ref ref-root "dangling" hash)
      (%assert (claw-lisp.storage.cas:cas-delete cas-root hash)
               "Expected CAS delete to remove target object")
      (%assert (equal hash (claw-lisp.storage.cas-ref:resolve-cas-ref
                            ref-root cas-root "dangling"))
               "Expected non-strict resolve to return dangling hash")
      (handler-case
          (claw-lisp.storage.cas-ref:resolve-cas-ref
           ref-root cas-root "dangling" :require-object-p t)
        (claw-lisp.storage.cas-ref:cas-ref-dangling-error ()
          (setf dangling-signaled t)))
      (%assert dangling-signaled
               "Expected cas-ref-dangling-error in strict resolve mode")))
  (format t "~&+ test-cas-ref-dangling-resolution passed~%")
  t)

(defun test-cas-ref-missing-resolution ()
  (%with-temp-cas-ref-root cas-root ref-root
    (%assert (null (claw-lisp.storage.cas-ref:resolve-cas-ref
                    ref-root cas-root "missing"))
             "Expected missing ref to resolve to NIL")
    (%assert (null (claw-lisp.storage.cas-ref:resolve-cas-ref
                    ref-root cas-root "missing" :require-object-p t))
             "Expected missing ref with strict mode to still resolve to NIL"))
  (format t "~&+ test-cas-ref-missing-resolution passed~%")
  t)

(defun test-cas-ref-list-and-delete ()
  (%with-temp-cas-ref-root cas-root ref-root
    (let ((hash (claw-lisp.storage.cas:cas-put cas-root "list")))
      (claw-lisp.storage.cas-ref:write-cas-ref ref-root "b/ref" hash)
      (claw-lisp.storage.cas-ref:write-cas-ref ref-root "a/ref" hash)
      (%assert (equal '("a/ref" "b/ref")
                      (claw-lisp.storage.cas-ref:list-cas-refs ref-root))
               "Expected sorted ref list")
      (%assert (claw-lisp.storage.cas-ref:delete-cas-ref ref-root "a/ref")
               "Expected delete to return T for existing ref")
      (%assert (null (claw-lisp.storage.cas-ref:delete-cas-ref ref-root "a/ref"))
               "Expected second delete to return NIL")
      (%assert (equal '("b/ref")
                      (claw-lisp.storage.cas-ref:list-cas-refs ref-root))
               "Expected one ref after delete")))
  (format t "~&+ test-cas-ref-list-and-delete passed~%")
  t)

(defun test-cas-ref-invalid-inputs ()
  (%with-temp-cas-ref-root cas-root ref-root
    (let ((hash (claw-lisp.storage.cas:cas-put cas-root "valid"))
          (bad-names '("" "../escape" "/leading" "trailing/" "bad\\name" "a..b")))
      (dolist (name bad-names)
        (let ((path-error nil))
          (handler-case
              (claw-lisp.storage.cas-ref:cas-ref-path ref-root name)
            (claw-lisp.storage.cas-ref:cas-ref-invalid-name-error ()
              (setf path-error t)))
          (%assert path-error "Expected invalid-name error for cas-ref-path name ~S" name))
        (let ((name-error nil))
          (handler-case
              (claw-lisp.storage.cas-ref:write-cas-ref ref-root name hash)
            (claw-lisp.storage.cas-ref:cas-ref-invalid-name-error ()
              (setf name-error t)))
          (%assert name-error "Expected invalid-name error for write name ~S" name)))
      
      (let ((hash-error nil))
        (handler-case
            (claw-lisp.storage.cas-ref:write-cas-ref ref-root "valid" "not-a-hash")
          (claw-lisp.storage.cas-ref:cas-ref-invalid-hash-error ()
            (setf hash-error t)))
        (%assert hash-error "Expected invalid-hash error for malformed hash input"))))
  (format t "~&+ test-cas-ref-invalid-inputs passed~%")
  t)

(defun test-cas-ref-determinism ()
  (%with-temp-cas-ref-root cas-root ref-root
    (let* ((hash (claw-lisp.storage.cas:cas-put cas-root "det"))
           ;; Write ref with unsorted metadata
           (record (claw-lisp.storage.cas-ref:write-cas-ref
                    ref-root "det-ref" hash :metadata '(:z 1 :a 2)))
           (path (claw-lisp.storage.cas-ref:cas-ref-path ref-root "det-ref"))
           (lines (uiop:read-file-lines path))
           (record-text (first lines)))
      ;; Check that :A precedes :Z in the serialized string
      (%assert (search ":A" record-text) "Expected :A in serialized record")
      (%assert (search ":Z" record-text) "Expected :Z in serialized record")
      (%assert (< (search ":A" record-text) (search ":Z" record-text))
               "Expected alphabetical key order in serialized ref record")))
  (format t "~&+ test-cas-ref-determinism passed~%")
  t)

(defun run-cas-ref-store-tests ()
  (format t "~&=== Phase 10 CAS Ref Store Tests ===~%")
  (test-cas-ref-write-read-roundtrip)
  (test-cas-ref-update-and-history)
  (test-cas-ref-conflict-detection)
  (test-cas-ref-dangling-resolution)
  (test-cas-ref-missing-resolution)
  (test-cas-ref-list-and-delete)
  (test-cas-ref-invalid-inputs)
  (test-cas-ref-determinism)
  (format t "~&=== All CAS ref-store tests passed ===~%")
  t)

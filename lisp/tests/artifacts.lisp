(in-package #:claw-lisp.tests)

(defun make-test-runtime-config (&key
                                   (cas-objects-root nil cas-objects-root-supplied-p)
                                   (cas-ref-root nil cas-ref-root-supplied-p)
                                   (tool-result-dedup-p nil tool-result-dedup-p-supplied-p))
  "Build a runtime config for tests without depending on internal constructors."
  (let ((config (claw-lisp.config:make-default-runtime-config)))
    (when cas-objects-root-supplied-p
      (setf (claw-lisp.config:runtime-config-cas-objects-root config) cas-objects-root))
    (when cas-ref-root-supplied-p
      (setf (claw-lisp.config:runtime-config-cas-ref-root config) cas-ref-root))
    (when tool-result-dedup-p-supplied-p
      (setf (claw-lisp.config:runtime-config-tool-result-dedup-p config)
            tool-result-dedup-p))
    config))

(defmacro %with-temp-cas-artifact-roots ((cas-root ref-root) &body body)
  `(let* ((temp-root (uiop:temporary-directory))
          (,cas-root (merge-pathnames
                      (format nil "artifact-cas-~D-~D/"
                              (sb-posix:getpid)
                              (get-internal-real-time))
                      temp-root))
          (,ref-root (merge-pathnames
                      (format nil "artifact-refs-~D-~D/"
                              (sb-posix:getpid)
                              (get-internal-real-time))
                      temp-root)))
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

(defun %write-octets (pathname octets)
  (ensure-directories-exist pathname)
  (with-open-file (stream pathname
                          :direction :output
                          :if-exists :supersede
                          :if-does-not-exist :create
                          :element-type '(unsigned-byte 8))
    (write-sequence octets stream)))

(defun %migration-script-path ()
  (merge-pathnames "tools/migrate-to-cas.lisp"
                   (asdf:system-source-directory :claw-lisp)))

(defun %run-migration-script (args)
  (multiple-value-bind (stdout stderr status)
      (uiop:run-program (append (list "sbcl"
                                      "--script"
                                      (namestring (%migration-script-path))
                                      "--")
                                args)
                        :output '(:string :stripped t)
                        :error-output '(:string :stripped t)
                        :ignore-error-status t)
    (list :stdout stdout
          :stderr stderr
          :status status)))

(defun %decode-script-report-file (pathname)
  (%assert (probe-file pathname)
           "Expected script report file at ~A"
           pathname)
  (claw-lisp.providers.http-utils:json-decode
   (uiop:read-file-string pathname)))

(defun %decode-script-json-output (result)
  (let* ((stdout (getf result :stdout))
         (lines (and stdout (uiop:split-string stdout :separator '(#\Newline))))
         (json-line (find-if (lambda (line)
                               (and (> (length line) 0)
                                    (char= #\{ (char line 0))))
                             (reverse lines))))
    (%assert json-line
             "Expected script stdout to contain a JSON line, got: ~S"
             result)
    (claw-lisp.providers.http-utils:json-decode json-line)))

(defun test-tool-result-cas-roundtrip ()
  (%with-temp-cas-artifact-roots (cas-root ref-root)
    (let* ((config (make-test-runtime-config
                    :cas-objects-root (namestring cas-root)
                    :cas-ref-root (namestring ref-root)
                    :tool-result-dedup-p t))
           (runtime (make-runtime :config config))
           (result (claw-lisp.core.domain:make-tool-result
                    :call-id "call-1"
                    :tool-name "echo"
                    :content "hello from cas"
                    :bytes 15)))
      (claw-lisp.core.artifacts:clear-tool-result-dedup-index runtime)
      (multiple-value-bind (stored artifact)
          (claw-lisp.core.artifacts:store-tool-result-cas runtime result)
        (%assert (string= "hello from cas"
                          (claw-lisp.core.artifacts:resolve-tool-result-cas
                           runtime stored))
                 "Expected CAS roundtrip to return original tool content")
        (%assert (string= (claw-lisp.core.domain:tool-result-cas-hash stored)
                          (claw-lisp.core.domain:artifact-cas-hash artifact))
                 "Stored tool-result hash should match artifact hash")
        (%assert (string= (claw-lisp.core.domain:tool-result-cas-ref-name stored)
                          (claw-lisp.core.domain:artifact-cas-ref-name artifact))
                 "Stored tool-result ref should match artifact ref")
        (%assert (claw-lisp.storage.cas-ref:resolve-cas-ref
                  ref-root cas-root
                  (claw-lisp.core.domain:tool-result-cas-ref-name stored)
                  :require-object-p t)
                 "CAS ref should resolve to a stored object"))))
  (format t "~&+ test-tool-result-cas-roundtrip passed~%")
  t)

(defun test-tool-result-ref-history-recorded ()
  (%with-temp-cas-artifact-roots (cas-root ref-root)
    (let* ((config (make-test-runtime-config
                    :cas-objects-root (namestring cas-root)
                    :cas-ref-root (namestring ref-root)
                    :tool-result-dedup-p t))
           (runtime (make-runtime :config config))
           (result (claw-lisp.core.domain:make-tool-result
                    :call-id "call-history"
                    :tool-name "echo"
                    :content "history me"
                    :bytes 10)))
      (claw-lisp.core.artifacts:clear-tool-result-dedup-index runtime)
      (multiple-value-bind (stored artifact)
          (claw-lisp.core.artifacts:store-tool-result-cas runtime result)
        (declare (ignore artifact))
        (let ((history-path (merge-pathnames
                             (format nil "refs-history/~A.history"
                                     (claw-lisp.core.domain:tool-result-cas-ref-name stored))
                             (uiop:ensure-directory-pathname ref-root))))
          (%assert (probe-file history-path)
                   "Expected tool-result ref history to be written")
          (%assert (= 1 (length (uiop:read-file-lines history-path)))
                   "Expected a single history entry after the first ref write")))))
  (format t "~&+ test-tool-result-ref-history-recorded passed~%")
  t)

(defun test-artifact-persistence-roundtrip ()
  (%with-temp-cas-artifact-roots (cas-root ref-root)
    (let* ((config (make-test-runtime-config
                    :cas-objects-root (namestring cas-root)
                    :cas-ref-root (namestring ref-root)))
           (runtime (make-runtime :config config))
           (payload '(:session-id "session-x"
                       :kind :compaction
                       :entries (:a 1 :b 2)))
           (artifact (claw-lisp.core.artifacts:persist-artifact-to-cas
                      runtime :compaction-manifest payload
                      :type :sexp
                      :ref-name "sessions/session-x/current-manifest"
                      :metadata '(:session-id "session-x"))))
      (%assert (claw-lisp.storage.cas:valid-versioned-hash-p
                (claw-lisp.core.domain:artifact-cas-hash artifact))
               "Artifact should have a valid CAS hash")
      (%assert (equal payload
                      (claw-lisp.core.artifacts:resolve-artifact-from-cas
                       runtime artifact))
               "Artifact payload should round-trip through CAS")
      (%assert (equal (claw-lisp.storage.cas-ref:resolve-cas-ref
                       ref-root cas-root
                       "sessions/session-x/current-manifest"
                       :require-object-p t)
                      (claw-lisp.core.domain:artifact-cas-hash artifact))
               "Artifact ref should point to the stored hash")))
  (format t "~&+ test-artifact-persistence-roundtrip passed~%")
  t)

(defun test-tool-result-cas-without-ref-root-best-effort ()
  (%with-temp-cas-artifact-roots (cas-root ref-root)
    (let* ((config (make-test-runtime-config
                    :cas-objects-root (namestring cas-root)
                    :cas-ref-root ""))
           (runtime (make-runtime :config config))
           (result (claw-lisp.core.domain:make-tool-result
                    :call-id "call-no-ref-root"
                    :tool-name "echo"
                    :content "ref-root optional"
                    :bytes 17)))
      (claw-lisp.core.artifacts:clear-tool-result-dedup-index runtime)
      (multiple-value-bind (stored artifact)
          (claw-lisp.core.artifacts:store-tool-result-cas runtime result)
        (%assert (claw-lisp.storage.cas:valid-versioned-hash-p
                  (claw-lisp.core.domain:tool-result-cas-hash stored))
                 "Expected CAS-backed tool result even without a ref root")
        (%assert (null (claw-lisp.core.domain:tool-result-cas-ref-name stored))
                 "Expected no ref name when ref root is unavailable")
        (%assert (string= "ref-root optional"
                          (claw-lisp.core.artifacts:resolve-tool-result-cas
                           runtime stored))
                 "Expected CAS resolution to work without a ref root")
        (%assert (null (claw-lisp.core.domain:artifact-cas-ref-name artifact))
                 "Expected artifact handle to omit ref name when unavailable")
        (%assert (claw-lisp.storage.cas:cas-exists-p
                  cas-root
                  (claw-lisp.core.domain:tool-result-cas-hash stored))
                "Expected CAS object to be stored even without a ref root"))))
  (format t "~&+ test-tool-result-cas-without-ref-root-best-effort passed~%")
  t)

(defun test-tool-result-cas-without-cas-root-best-effort ()
  (let* ((config (make-test-runtime-config
                  :cas-objects-root ""
                  :cas-ref-root ""))
         (runtime (make-runtime :config config))
         (result (claw-lisp.core.domain:make-tool-result
                  :call-id "call-no-cas-root"
                  :tool-name "echo"
                  :content "offline tool result"
                  :bytes 19)))
    (claw-lisp.core.artifacts:clear-tool-result-dedup-index runtime)
    (multiple-value-bind (stored artifact)
        (claw-lisp.core.artifacts:store-tool-result-cas runtime result)
      (%assert (null (claw-lisp.core.domain:tool-result-cas-hash stored))
               "Expected no CAS hash when CAS root is unavailable")
      (%assert (eq :tool-result (claw-lisp.core.domain:artifact-kind artifact))
               "Expected a best-effort tool-result artifact")
      (%assert (equal '(:tool-name "echo" :bytes 19 :truncated-p nil)
                      (claw-lisp.core.domain:artifact-metadata artifact))
               "Expected best-effort metadata to be preserved")))
  (format t "~&+ test-tool-result-cas-without-cas-root-best-effort passed~%")
  t)

(defun test-tool-result-dedup-reuses-existing-cas ()
  (%with-temp-cas-artifact-roots (cas-root ref-root)
    (let* ((config (make-test-runtime-config
                    :cas-objects-root (namestring cas-root)
                    :cas-ref-root (namestring ref-root)
                    :tool-result-dedup-p t))
           (runtime (make-runtime :config config))
           (result (claw-lisp.core.domain:make-tool-result
                    :call-id "call-dedup"
                    :tool-name "echo"
                    :content "dedup me"
                    :bytes 8)))
      (claw-lisp.core.artifacts:clear-tool-result-dedup-index runtime)
      (multiple-value-bind (stored-1 artifact-1)
          (claw-lisp.core.artifacts:store-tool-result-cas
           runtime result :write-ref-p nil)
        (multiple-value-bind (stored-2 artifact-2)
            (claw-lisp.core.artifacts:store-tool-result-cas
             runtime result :write-ref-p nil)
          (%assert (= 1 (claw-lisp.core.artifacts:tool-result-dedup-index-size runtime))
                   "Expected exactly one dedup entry after repeated identical writes")
          (%assert (string= (claw-lisp.core.domain:tool-result-cas-hash stored-1)
                            (claw-lisp.core.domain:tool-result-cas-hash stored-2))
                   "Expected repeated identical tool results to share the same CAS hash")
          (%assert (string= (claw-lisp.core.domain:artifact-cas-hash artifact-1)
                            (claw-lisp.core.domain:artifact-cas-hash artifact-2))
                   "Expected artifact handles to reference the same CAS hash")))
      (claw-lisp.core.artifacts:clear-tool-result-dedup-index runtime)))
  (format t "~&+ test-tool-result-dedup-reuses-existing-cas passed~%")
  t)

(defun test-tool-result-dedup-index-prunes-to-limit ()
  (%with-temp-cas-artifact-roots (cas-root ref-root)
    (let ((claw-lisp.core.artifacts::*tool-result-dedup-index-max-entries* 1))
      (let* ((config (make-test-runtime-config
                      :cas-objects-root (namestring cas-root)
                      :cas-ref-root (namestring ref-root)
                      :tool-result-dedup-p t))
             (runtime (make-runtime :config config))
             (first (claw-lisp.core.domain:make-tool-result
                     :call-id "call-first"
                     :tool-name "echo"
                     :content "first"
                     :bytes 5))
             (second (claw-lisp.core.domain:make-tool-result
                      :call-id "call-second"
                      :tool-name "echo"
                      :content "second"
                      :bytes 6)))
        (claw-lisp.core.artifacts:clear-tool-result-dedup-index runtime)
        (claw-lisp.core.artifacts:store-tool-result-cas runtime first :write-ref-p nil)
        (claw-lisp.core.artifacts:store-tool-result-cas runtime second :write-ref-p nil)
        (%assert (= 1 (claw-lisp.core.artifacts:tool-result-dedup-index-size runtime))
                 "Expected dedup index to stay within the configured limit")
        (%assert (null (gethash (claw-lisp.core.artifacts:compute-tool-result-dedup-key
                                 first)
                                (claw-lisp.core.runtime:runtime-tool-result-dedup-index
                                 runtime)))
                 "Expected the oldest dedup entry to be pruned"))))
  (format t "~&+ test-tool-result-dedup-index-prunes-to-limit passed~%")
  t)

(defun test-tool-result-dedup-lru-promotes-reused-entry ()
  (%with-temp-cas-artifact-roots (cas-root ref-root)
    (let ((claw-lisp.core.artifacts::*tool-result-dedup-index-max-entries* 2))
      (let* ((config (make-test-runtime-config
                      :cas-objects-root (namestring cas-root)
                      :cas-ref-root (namestring ref-root)
                      :tool-result-dedup-p t))
             (runtime (make-runtime :config config))
             (first (claw-lisp.core.domain:make-tool-result
                     :call-id "call-first"
                     :tool-name "echo"
                     :content "first"
                     :bytes 5))
             (second (claw-lisp.core.domain:make-tool-result
                      :call-id "call-second"
                      :tool-name "echo"
                      :content "second"
                      :bytes 6))
             (third (claw-lisp.core.domain:make-tool-result
                     :call-id "call-third"
                     :tool-name "echo"
                     :content "third"
                     :bytes 5)))
        (claw-lisp.core.artifacts:clear-tool-result-dedup-index runtime)
        (claw-lisp.core.artifacts:store-tool-result-cas runtime first :write-ref-p nil)
        (claw-lisp.core.artifacts:store-tool-result-cas runtime second :write-ref-p nil)
        (claw-lisp.core.artifacts:store-tool-result-cas runtime first :write-ref-p nil)
        (claw-lisp.core.artifacts:store-tool-result-cas runtime third :write-ref-p nil)
        (%assert (= 2 (claw-lisp.core.artifacts:tool-result-dedup-index-size runtime))
                 "Expected dedup index to remain bounded after promotion")
        (%assert (null (gethash (claw-lisp.core.artifacts:compute-tool-result-dedup-key
                                 second)
                                (claw-lisp.core.runtime:runtime-tool-result-dedup-index
                                 runtime)))
                 "Expected the least recently used entry to be evicted")
        (%assert (gethash (claw-lisp.core.artifacts:compute-tool-result-dedup-key first)
                          (claw-lisp.core.runtime:runtime-tool-result-dedup-index runtime))
                 "Expected the reused entry to remain in the cache"))))
  (format t "~&+ test-tool-result-dedup-lru-promotes-reused-entry passed~%")
  t)

(defun test-tool-result-large-truncated-roundtrip ()
  (%with-temp-cas-artifact-roots (cas-root ref-root)
    (let* ((config (make-test-runtime-config
                    :cas-objects-root (namestring cas-root)
                    :cas-ref-root (namestring ref-root)))
           (runtime (make-runtime :config config))
           (content (make-string 8192 :initial-element #\x))
           (result (claw-lisp.core.domain:make-tool-result
                    :call-id "call-large"
                    :tool-name "echo"
                    :content content
                    :truncated-p t
                    :bytes (length content))))
      (claw-lisp.core.artifacts:clear-tool-result-dedup-index runtime)
      (multiple-value-bind (stored artifact)
          (claw-lisp.core.artifacts:store-tool-result-cas runtime result)
        (%assert (string= content
                          (claw-lisp.core.artifacts:resolve-tool-result-cas
                           runtime stored))
                 "Expected large tool result to round-trip through CAS")
        (%assert (claw-lisp.core.domain:tool-result-truncated-p stored)
                 "Expected truncation flag to be preserved on stored tool result")
        (%assert (claw-lisp.core.domain:artifact-metadata artifact)
                 "Expected artifact metadata to be populated for large tool result")
        (%assert (eq t (getf (claw-lisp.core.domain:artifact-metadata artifact)
                             :truncated-p))
                 "Expected artifact metadata to record truncation"))))
  (format t "~&+ test-tool-result-large-truncated-roundtrip passed~%")
  t)

(defun test-tool-result-resolves-after-runtime-reload ()
  (%with-temp-cas-artifact-roots (cas-root ref-root)
    (let* ((config (make-test-runtime-config
                    :cas-objects-root (namestring cas-root)
                    :cas-ref-root (namestring ref-root)
                    :tool-result-dedup-p t))
           (runtime-1 (make-runtime :config config))
           (runtime-2 (make-runtime :config config))
           (result (claw-lisp.core.domain:make-tool-result
                    :call-id "call-reload"
                    :tool-name "echo"
                    :content "reloadable content"
                    :bytes 18)))
      (claw-lisp.core.artifacts:clear-tool-result-dedup-index runtime-1)
      (claw-lisp.core.artifacts:clear-tool-result-dedup-index runtime-2)
      (multiple-value-bind (stored artifact)
          (claw-lisp.core.artifacts:store-tool-result-cas runtime-1 result)
        (%assert (string= "reloadable content"
                          (claw-lisp.core.artifacts:resolve-tool-result-cas
                           runtime-2 stored))
                 "Expected a stored tool result to remain resolvable after runtime reload")
        (%assert (string= (claw-lisp.core.domain:tool-result-cas-hash stored)
                          (claw-lisp.core.domain:artifact-cas-hash artifact))
                 "Expected artifact hash to remain stable across reloads"))))
  (format t "~&+ test-tool-result-resolves-after-runtime-reload passed~%")
  t)

(defun test-legacy-tool-result-compatibility-migrates-to-cas ()
  (%with-temp-cas-artifact-roots (cas-root ref-root)
    (let* ((artifacts-root (merge-pathnames
                            (format nil "legacy-artifacts-~D-~D/"
                                    (get-universal-time)
                                    (get-internal-real-time))
                            (uiop:temporary-directory)))
           (legacy-path (merge-pathnames "tool-results/session-1/call-legacy.txt"
                                         artifacts-root))
           (full-content "legacy oversized content")
           (config (make-test-runtime-config
                    :cas-objects-root (namestring cas-root)
                    :cas-ref-root (namestring ref-root)
                    :tool-result-dedup-p t))
           (runtime nil)
           (result (claw-lisp.core.domain:make-tool-result
                    :call-id "call-legacy"
                    :tool-name "echo"
                    :content "legacy preview"
                    :persisted-path (namestring legacy-path)
                    :truncated-p t
                    :bytes (length full-content))))
      (setf (claw-lisp.config:runtime-config-artifacts-root config)
            (namestring artifacts-root))
      (setf runtime (make-runtime :config config))
      (unwind-protect
          (progn
            (claw-lisp.storage.tool-results::write-artifact legacy-path full-content)
            (claw-lisp.core.artifacts:clear-tool-result-dedup-index runtime)
            (multiple-value-bind (compatible artifact descriptor)
                (claw-lisp.core.artifacts:ensure-tool-result-cas-compatibility runtime result)
              (%assert (string= "legacy preview"
                                (claw-lisp.core.domain:tool-result-content compatible))
                       "Compatibility migration should preserve preview content")
              (%assert (claw-lisp.core.domain:tool-result-cas-hash compatible)
                       "Expected compatibility migration to attach a CAS hash")
              (%assert (claw-lisp.core.domain:tool-result-cas-ref-name compatible)
                       "Expected compatibility migration to attach a CAS ref")
              (%assert (string= (claw-lisp.core.domain:tool-result-cas-hash compatible)
                                (claw-lisp.core.domain:artifact-cas-hash artifact))
                       "Expected artifact hash to match migrated tool-result hash")
              (%assert (eq :tool-result (getf descriptor :kind))
                       "Expected tool-result descriptor for compatibility migration")
              (%assert (string= full-content
                                (claw-lisp.core.artifacts:resolve-tool-result-cas runtime compatible))
                       "Expected CAS compatibility result to resolve full legacy content")
              (delete-file legacy-path)
              (%assert (string= full-content
                                (claw-lisp.core.artifacts:resolve-tool-result-cas runtime compatible))
                       "Expected migrated tool-result to remain resolvable after legacy file removal")
              t))
        (when (uiop:directory-exists-p artifacts-root)
          (uiop:delete-directory-tree artifacts-root :validate t)))))
  (format t "~&+ test-legacy-tool-result-compatibility-migrates-to-cas passed~%"))

(defun test-legacy-tool-result-compatibility-falls-back-on-cas-write-failure ()
  (%with-temp-cas-artifact-roots (cas-root ref-root)
    (let* ((artifacts-root (merge-pathnames
                            (format nil "legacy-artifacts-failure-~D-~D/"
                                    (get-universal-time)
                                    (get-internal-real-time))
                            (uiop:temporary-directory)))
           (legacy-path (merge-pathnames "tool-results/session-1/call-failure.txt"
                                         artifacts-root))
           (full-content "legacy fallback content")
           (config (make-test-runtime-config
                    :cas-objects-root (namestring cas-root)
                    :cas-ref-root (namestring ref-root)
                    :tool-result-dedup-p t))
           (runtime nil)
           (result (claw-lisp.core.domain:make-tool-result
                    :call-id "call-failure"
                    :tool-name "echo"
                    :content "legacy preview"
                    :persisted-path (namestring legacy-path)
                    :truncated-p t
                    :bytes (length full-content)))
           (original-cas-put (symbol-function 'claw-lisp.storage.cas:cas-put))
           (warnings '()))
      (setf (claw-lisp.config:runtime-config-artifacts-root config)
            (namestring artifacts-root))
      (setf runtime (make-runtime :config config))
      (unwind-protect
          (progn
            (claw-lisp.storage.tool-results::write-artifact legacy-path full-content)
            (setf (symbol-function 'claw-lisp.storage.cas:cas-put)
                  (lambda (&rest args)
                    (declare (ignore args))
                    (error 'claw-lisp.storage.cas:cas-write-error
                           :path legacy-path
                           :cause "synthetic CAS write failure")))
            (handler-bind ((warning (lambda (condition)
                                      (push (princ-to-string condition) warnings)
                                      (muffle-warning condition))))
              (%assert (string= full-content
                                (claw-lisp.core.artifacts:resolve-tool-result-cas runtime result))
                       "Expected legacy persisted-path read to survive CAS compatibility failure"))
            (%assert (some (lambda (warning)
                             (search "Tool-result CAS compatibility migration failed" warning))
                           warnings)
                     "Expected compatibility migration warning, got ~S"
                     warnings)
            t)
        (setf (symbol-function 'claw-lisp.storage.cas:cas-put) original-cas-put)
        (when (uiop:directory-exists-p artifacts-root)
          (uiop:delete-directory-tree artifacts-root :validate t)))))
  (format t "~&+ test-legacy-tool-result-compatibility-falls-back-on-cas-write-failure passed~%"))

(defun test-legacy-tool-result-invalid-cas-hash-still-rejected ()
  (%with-temp-cas-artifact-roots (cas-root ref-root)
    (let* ((artifacts-root (merge-pathnames
                            (format nil "legacy-artifacts-invalid-hash-~D-~D/"
                                    (get-universal-time)
                                    (get-internal-real-time))
                            (uiop:temporary-directory)))
           (legacy-path (merge-pathnames "tool-results/session-1/call-invalid-hash.txt"
                                         artifacts-root))
           (full-content "legacy invalid hash content")
           (config (make-test-runtime-config
                    :cas-objects-root (namestring cas-root)
                    :cas-ref-root (namestring ref-root)
                    :tool-result-dedup-p t))
           (runtime nil)
           (result (claw-lisp.core.domain:make-tool-result
                    :call-id "call-invalid-hash"
                    :tool-name "echo"
                    :content "legacy preview"
                    :persisted-path (namestring legacy-path)
                    :truncated-p t
                    :bytes (length full-content)
                    :cas-hash "not-a-valid-hash"))
           (signaled nil))
      (setf (claw-lisp.config:runtime-config-artifacts-root config)
            (namestring artifacts-root))
      (setf runtime (make-runtime :config config))
      (unwind-protect
          (progn
            (claw-lisp.storage.tool-results::write-artifact legacy-path full-content)
            (handler-case
                (claw-lisp.core.artifacts:resolve-tool-result-cas runtime result)
              (claw-lisp.storage.cas:cas-invalid-hash-error ()
                (setf signaled t)))
            (%assert signaled
                     "Expected invalid legacy CAS hash to be rejected before compatibility migration")
            t)
        (when (uiop:directory-exists-p artifacts-root)
          (uiop:delete-directory-tree artifacts-root :validate t)))))
  (format t "~&+ test-legacy-tool-result-invalid-cas-hash-still-rejected passed~%"))

(defun test-legacy-tool-result-unexpected-compatibility-error-propagates ()
  (%with-temp-cas-artifact-roots (cas-root ref-root)
    (let* ((artifacts-root (merge-pathnames
                            (format nil "legacy-artifacts-unexpected-error-~D-~D/"
                                    (get-universal-time)
                                    (get-internal-real-time))
                            (uiop:temporary-directory)))
           (legacy-path (merge-pathnames "tool-results/session-1/call-unexpected.txt"
                                         artifacts-root))
           (full-content "legacy unexpected error content")
           (config (make-test-runtime-config
                    :cas-objects-root (namestring cas-root)
                    :cas-ref-root (namestring ref-root)
                    :tool-result-dedup-p t))
           (runtime nil)
           (result (claw-lisp.core.domain:make-tool-result
                    :call-id "call-unexpected"
                    :tool-name "echo"
                    :content "legacy preview"
                    :persisted-path (namestring legacy-path)
                    :truncated-p t
                    :bytes (length full-content)))
           (original-cas-put (symbol-function 'claw-lisp.storage.cas:cas-put))
           (signaled nil))
      (setf (claw-lisp.config:runtime-config-artifacts-root config)
            (namestring artifacts-root))
      (setf runtime (make-runtime :config config))
      (unwind-protect
          (progn
            (claw-lisp.storage.tool-results::write-artifact legacy-path full-content)
            (setf (symbol-function 'claw-lisp.storage.cas:cas-put)
                  (lambda (&rest args)
                    (declare (ignore args))
                    (error "synthetic unexpected compatibility failure")))
            (handler-case
                (claw-lisp.core.artifacts:resolve-tool-result-cas runtime result)
              (error ()
                (setf signaled t)))
            (%assert signaled
                     "Expected unexpected compatibility errors to propagate")
            t)
        (setf (symbol-function 'claw-lisp.storage.cas:cas-put) original-cas-put)
        (when (uiop:directory-exists-p artifacts-root)
          (uiop:delete-directory-tree artifacts-root :validate t)))))
  (format t "~&+ test-legacy-tool-result-unexpected-compatibility-error-propagates passed~%"))

(defun test-legacy-tool-result-batch-migration-preserves-compatibility ()
  (%with-temp-cas-artifact-roots (cas-root ref-root)
    (let* ((artifacts-root (merge-pathnames
                            (format nil "legacy-artifacts-batch-~D-~D/"
                                    (get-universal-time)
                                    (get-internal-real-time))
                            (uiop:temporary-directory)))
           (legacy-path-a (merge-pathnames "tool-results/session-a/call-1.txt"
                                           artifacts-root))
           (legacy-path-b (merge-pathnames "tool-results/session-b/call-2.txt"
                                           artifacts-root))
           (shared-content "same payload for dedup")
           (config (make-test-runtime-config
                    :cas-objects-root (namestring cas-root)
                    :cas-ref-root (namestring ref-root)
                    :tool-result-dedup-p t))
           (runtime nil))
      (setf (claw-lisp.config:runtime-config-artifacts-root config)
            (namestring artifacts-root))
      (setf runtime (make-runtime :config config))
      (unwind-protect
          (progn
            (claw-lisp.storage.tool-results::write-artifact legacy-path-a shared-content)
            (claw-lisp.storage.tool-results::write-artifact legacy-path-b shared-content)
            (let ((summary (claw-lisp.core.artifacts:migrate-legacy-tool-results-to-cas runtime)))
              (%assert (= 2 (getf summary :scanned-count))
                       "Expected both legacy files to be scanned")
              (%assert (= 2 (getf summary :migrated-count))
                       "Expected both legacy files to migrate")
              (%assert (= 0 (getf summary :failure-count))
                       "Expected migration to complete without failures")
              (let* ((result-a (claw-lisp.core.domain:make-tool-result
                                :call-id "call-1"
                                :tool-name "echo"
                                :content "preview a"
                                :persisted-path (namestring legacy-path-a)
                                :truncated-p t
                                :bytes (length shared-content)))
                     (result-b (claw-lisp.core.domain:make-tool-result
                                :call-id "call-2"
                                :tool-name "echo"
                                :content "preview b"
                                :persisted-path (namestring legacy-path-b)
                                :truncated-p t
                                :bytes (length shared-content)))
                     (compatible-a
                      (nth-value 0
                                 (claw-lisp.core.artifacts:ensure-tool-result-cas-compatibility
                                  runtime result-a)))
                     (compatible-b
                      (nth-value 0
                                 (claw-lisp.core.artifacts:ensure-tool-result-cas-compatibility
                                  runtime result-b))))
                (%assert (string= (claw-lisp.core.domain:tool-result-cas-hash compatible-a)
                                  (claw-lisp.core.domain:tool-result-cas-hash compatible-b))
                         "Expected identical legacy payloads to resolve to the same CAS hash")
                (delete-file legacy-path-a)
                (delete-file legacy-path-b)
                (%assert (string= shared-content
                                  (claw-lisp.core.artifacts:resolve-tool-result-cas runtime compatible-a))
                         "Expected migrated session-a result to remain readable after legacy file deletion")
                (%assert (string= shared-content
                                  (claw-lisp.core.artifacts:resolve-tool-result-cas runtime compatible-b))
                         "Expected migrated session-b result to remain readable after legacy file deletion"))))
        (when (uiop:directory-exists-p artifacts-root)
          (uiop:delete-directory-tree artifacts-root :validate t)))))
  (format t "~&+ test-legacy-tool-result-batch-migration-preserves-compatibility passed~%")
  t)

(defun test-legacy-tool-result-batch-migration-is-rollback-safe-on-failure ()
  (%with-temp-cas-artifact-roots (cas-root ref-root)
    (let* ((artifacts-root (merge-pathnames
                            (format nil "legacy-artifacts-rollback-~D-~D/"
                                    (get-universal-time)
                                    (get-internal-real-time))
                            (uiop:temporary-directory)))
           (legacy-path-ok (merge-pathnames "tool-results/session-z/call-ok.txt"
                                            artifacts-root))
           (legacy-path-fail (merge-pathnames "tool-results/session-z/call-fail.txt"
                                              artifacts-root))
           (config (make-test-runtime-config
                    :cas-objects-root (namestring cas-root)
                    :cas-ref-root (namestring ref-root)
                    :tool-result-dedup-p t))
           (runtime nil)
           (original-cas-put (symbol-function 'claw-lisp.storage.cas:cas-put)))
      (setf (claw-lisp.config:runtime-config-artifacts-root config)
            (namestring artifacts-root))
      (setf runtime (make-runtime :config config))
      (unwind-protect
          (progn
            (claw-lisp.storage.tool-results::write-artifact legacy-path-ok "ok payload")
            (claw-lisp.storage.tool-results::write-artifact legacy-path-fail "fail payload")
            (setf (symbol-function 'claw-lisp.storage.cas:cas-put)
                  (lambda (cas-root content)
                    (if (string= content "fail payload")
                        (error 'claw-lisp.storage.cas:cas-write-error
                               :path legacy-path-fail
                               :cause "synthetic batch migration failure")
                        (funcall original-cas-put cas-root content))))
            (let ((summary (claw-lisp.core.artifacts:migrate-legacy-tool-results-to-cas runtime)))
              (%assert (= 2 (getf summary :scanned-count))
                       "Expected rollback safety test to scan both files")
              (%assert (= 1 (getf summary :migrated-count))
                       "Expected only one file to migrate successfully")
              (%assert (= 1 (getf summary :failure-count))
                       "Expected one migration failure to be recorded")
              (%assert (probe-file legacy-path-ok)
                       "Expected successful source file to remain for rollback safety")
              (%assert (probe-file legacy-path-fail)
                       "Expected failed source file to remain for rollback safety")
              (%assert (null (nth-value
                              0
                              (claw-lisp.core.artifacts::%resolve-legacy-path-cas-mapping
                               runtime legacy-path-fail :require-object-p nil)))
                       "Expected failed legacy path not to gain a CAS mapping")))
        (setf (symbol-function 'claw-lisp.storage.cas:cas-put) original-cas-put)
        (when (uiop:directory-exists-p artifacts-root)
          (uiop:delete-directory-tree artifacts-root :validate t)))))
  (format t "~&+ test-legacy-tool-result-batch-migration-is-rollback-safe-on-failure passed~%")
  t)

(defun test-migration-script-is-idempotent ()
  (%with-temp-cas-artifact-roots (cas-root ref-root)
    (let* ((artifacts-root (merge-pathnames
                            (format nil "migration-script-idempotent-~D-~D/"
                                    (get-universal-time)
                                    (get-internal-real-time))
                            (uiop:temporary-directory)))
           (legacy-path (merge-pathnames "tool-results/session-script/call-1.txt"
                                         artifacts-root))
           (report-path-1 (merge-pathnames "reports/first/report.json" artifacts-root))
           (report-path-2 (merge-pathnames "reports/second/report.json" artifacts-root))
           (payload "script idempotent payload"))
      (unwind-protect
          (progn
            (claw-lisp.storage.tool-results::write-artifact legacy-path payload)
            (let* ((first nil)
                   (second nil)
                   (first-run (%run-migration-script
                               (list "--artifacts-root" (namestring artifacts-root)
                                     "--cas-objects-root" (namestring cas-root)
                                     "--cas-ref-root" (namestring ref-root)
                                     "--session" "session-script"
                                     "--report-file" (namestring report-path-1)))))
              (%assert (= 0 (getf first-run :status))
                       "Expected first migration script run to exit 0, got ~S"
                       first-run)
              (setf first (%decode-script-report-file report-path-1))
              (%assert (= 1 (getf first :scanned-count))
                       "Expected first run to scan one file")
              (%assert (= 1 (getf first :migrated-count))
                       "Expected first run to migrate one file")
              (%assert (= 0 (getf first :already-mapped-count))
                       "Expected first run to have no already-mapped files")
              (%assert (= 0 (getf first :failure-count))
                       "Expected first run to have no failures")
              (%assert (probe-file report-path-1)
                       "Expected first run to write its report file")
              (let* ((second-run (%run-migration-script
                                  (list "--artifacts-root" (namestring artifacts-root)
                                        "--cas-objects-root" (namestring cas-root)
                                        "--cas-ref-root" (namestring ref-root)
                                        "--session" "session-script"
                                        "--report-file" (namestring report-path-2))))
                     (migrated-entry (first (getf first :migrated))))
                (%assert (= 0 (getf second-run :status))
                         "Expected second migration script run to exit 0, got ~S"
                         second-run)
                (setf second (%decode-script-report-file report-path-2))
                (%assert (= 1 (getf second :scanned-count))
                         "Expected second run to scan one file")
                (%assert (= 0 (getf second :migrated-count))
                         "Expected second run not to migrate again")
                (%assert (= 1 (getf second :already-mapped-count))
                         "Expected second run to record one already-mapped file")
                (%assert (= 0 (getf second :failure-count))
                         "Expected second run to have no failures")
                (%assert (probe-file report-path-2)
                         "Expected second run to write its report file")
                (%assert (string=
                          (getf migrated-entry :cas-hash)
                          (getf (first (getf second :already-mapped)) :cas-hash))
                         "Expected idempotent re-run to preserve the CAS hash"))))
        (when (uiop:directory-exists-p artifacts-root)
          (uiop:delete-directory-tree artifacts-root :validate t)))))
  (format t "~&+ test-migration-script-is-idempotent passed~%")
  t)

(defun test-migration-script-strict-and-nonstrict-exit-codes ()
  (%with-temp-cas-artifact-roots (cas-root ref-root)
    (labels ((exercise (strict-p)
               (let* ((artifacts-root (merge-pathnames
                                       (format nil "migration-script-strict-~A-~D-~D/"
                                               (if strict-p "strict" "nonstrict")
                                               (get-universal-time)
                                               (get-internal-real-time))
                                       (uiop:temporary-directory)))
                      (legacy-path-ok (merge-pathnames "tool-results/session-strict/call-1.txt"
                                                       artifacts-root))
                      (legacy-path-fail (merge-pathnames "tool-results/session-strict/call-2.txt"
                                                         artifacts-root))
                      (report-path (merge-pathnames "reports/result.json" artifacts-root))
                      (args (append (list "--artifacts-root" (namestring artifacts-root)
                                          "--cas-objects-root" (namestring cas-root)
                                          "--cas-ref-root" (namestring ref-root)
                                          "--session" "session-strict"
                                          "--report-file" (namestring report-path))
                                    (when strict-p (list "--strict")))))
                 (unwind-protect
                     (progn
                       (claw-lisp.storage.tool-results::write-artifact legacy-path-ok "ok payload")
                       (%write-octets legacy-path-fail #(255 254 253 252))
                       (let ((result (%run-migration-script args))
                             (summary nil))
                         (setf summary (%decode-script-report-file report-path))
                         (%assert (probe-file report-path)
                                  "Expected script run to write a report file")
                         (values result summary artifacts-root)))
                   (when (uiop:directory-exists-p artifacts-root)
                     (uiop:delete-directory-tree artifacts-root :validate t))))))
      (multiple-value-bind (strict-result strict-summary strict-root)
          (exercise t)
        (declare (ignore strict-root))
        (%assert (= 1 (getf strict-result :status))
                 "Expected strict migration run to exit 1, got ~S"
                 strict-result)
        (%assert (= 1 (getf strict-summary :migrated-count))
                 "Expected strict run to retain the successful migration")
        (%assert (= 1 (getf strict-summary :failure-count))
                 "Expected strict run to record the aborting failure")
        (%assert (getf strict-summary :aborted-p)
                 "Expected strict run summary to mark the migration as aborted")
        (%assert (= 1 (length (getf strict-summary :migrated)))
                 "Expected one migrated entry before strict abort")
        (%assert (= 1 (length (getf strict-summary :failures)))
                 "Expected one failure entry in strict abort summary"))
      (multiple-value-bind (nonstrict-result nonstrict-summary nonstrict-root)
          (exercise nil)
        (declare (ignore nonstrict-root))
        (%assert (= 2 (getf nonstrict-result :status))
                 "Expected non-strict migration run to exit 2, got ~S"
                 nonstrict-result)
        (%assert (= 1 (getf nonstrict-summary :migrated-count))
                 "Expected non-strict run to migrate one file")
        (%assert (= 1 (getf nonstrict-summary :failure-count))
                 "Expected non-strict run to record one failure")
        (%assert (= 1 (length (getf nonstrict-summary :migrated)))
                 "Expected one migrated entry in non-strict summary")
        (%assert (= 1 (length (getf nonstrict-summary :failures)))
                 "Expected one failure entry in non-strict summary"))))
  (format t "~&+ test-migration-script-strict-and-nonstrict-exit-codes passed~%")
  t)

(defun test-migration-script-cli-edge-cases ()
  (%with-temp-cas-artifact-roots (cas-root ref-root)
    (let* ((artifacts-root (merge-pathnames
                            (format nil "migration-script-cli-~D-~D/"
                                    (get-universal-time)
                                    (get-internal-real-time))
                            (uiop:temporary-directory)))
           (legacy-path (merge-pathnames "tool-results/session-cli/call-1.txt"
                                         artifacts-root))
           (nested-report-path (merge-pathnames "nested/reports/output.json"
                                                artifacts-root)))
      (unwind-protect
          (progn
            (let ((help-result (%run-migration-script (list "--help"))))
              (%assert (= 0 (getf help-result :status))
                       "Expected --help to exit 0, got ~S"
                       help-result)
              (%assert (search "Usage: sbcl --script tools/migrate-to-cas.lisp -- [options]"
                               (or (getf help-result :stdout) ""))
                       "Expected --help to print usage text")
              (%assert (not (search "Migration failed:" (or (getf help-result :stderr) "")))
                       "Did not expect --help to attempt migration"))
            (let ((unknown-result (%run-migration-script (list "--not-a-real-flag"))))
              (%assert (= 1 (getf unknown-result :status))
                       "Expected unknown flag to exit 1, got ~S"
                       unknown-result)
              (%assert (search "Unknown argument: --not-a-real-flag"
                               (or (getf unknown-result :stderr) ""))
                       "Expected unknown flag error on stderr")
              (%assert (not (search "Unknown argument: --not-a-real-flag"
                                    (or (getf unknown-result :stdout) "")))
                       "Did not expect unknown flag error on stdout"))
            (claw-lisp.storage.tool-results::write-artifact legacy-path "report payload")
            (let* ((result (%run-migration-script
                            (list "--artifacts-root" (namestring artifacts-root)
                                  "--cas-objects-root" (namestring cas-root)
                                  "--cas-ref-root" (namestring ref-root)
                                  "--session" "session-cli"
                                  "--report-file" (namestring nested-report-path))))
                   (summary (%decode-script-report-file nested-report-path)))
              (%assert (= 0 (getf result :status))
                       "Expected nested report-file run to exit 0, got ~S"
                       result)
              (%assert (probe-file nested-report-path)
                       "Expected nested report-file path to be created")
              (%assert (= 1 (getf summary :migrated-count))
                       "Expected nested report-file run to migrate one file")))
        (when (uiop:directory-exists-p artifacts-root)
          (uiop:delete-directory-tree artifacts-root :validate t)))))
  (format t "~&+ test-migration-script-cli-edge-cases passed~%")
  t)

(defun test-tool-result-dedup-isolated-per-runtime ()
  (%with-temp-cas-artifact-roots (cas-root ref-root)
    (let* ((config-a (make-test-runtime-config
                      :cas-objects-root (namestring cas-root)
                      :cas-ref-root (namestring ref-root)
                      :tool-result-dedup-p t))
           (config-b (make-test-runtime-config
                      :cas-objects-root (namestring cas-root)
                      :cas-ref-root (namestring ref-root)
                      :tool-result-dedup-p t))
           (runtime-a (make-runtime :config config-a))
           (runtime-b (make-runtime :config config-b))
           (result (claw-lisp.core.domain:make-tool-result
                    :call-id "call-isolated"
                    :tool-name "echo"
                    :content "isolated"
                    :bytes 8)))
      (claw-lisp.core.artifacts:clear-tool-result-dedup-index runtime-a)
      (claw-lisp.core.artifacts:clear-tool-result-dedup-index runtime-b)
      (claw-lisp.core.artifacts:store-tool-result-cas runtime-a result :write-ref-p nil)
      (%assert (= 1 (claw-lisp.core.artifacts:tool-result-dedup-index-size runtime-a))
               "Expected runtime A to retain its own dedup entry")
      (%assert (= 0 (claw-lisp.core.artifacts:tool-result-dedup-index-size runtime-b))
               "Expected runtime B dedup index to remain isolated")))
  (format t "~&+ test-tool-result-dedup-isolated-per-runtime passed~%")
  t)

(defun test-persist-artifact-without-cas-root-best-effort ()
  (let* ((config (make-test-runtime-config
                  :cas-objects-root ""
                  :cas-ref-root "")))
    (let* ((runtime (make-runtime :config config))
           (artifact (claw-lisp.core.artifacts:persist-artifact-to-cas
                      runtime :note "offline artifact" :type :markdown
                      :metadata '(:source :test))))
      (%assert (null (claw-lisp.core.domain:artifact-cas-hash artifact))
               "Expected no CAS hash when CAS root is unavailable")
      (%assert (eq :note (claw-lisp.core.domain:artifact-kind artifact))
               "Expected artifact kind to be preserved")
      (%assert (equal '(:source :test)
                      (claw-lisp.core.domain:artifact-metadata artifact))
               "Expected artifact metadata to be preserved"))
    (format t "~&+ test-persist-artifact-without-cas-root-best-effort passed~%")
    t))

(defun test-resolve-artifact-invalid-hash-rejected ()
  (%with-temp-cas-artifact-roots (cas-root ref-root)
    (let* ((config (make-test-runtime-config
                    :cas-objects-root (namestring cas-root)
                    :cas-ref-root (namestring ref-root)))
           (runtime (make-runtime :config config))
           (artifact (claw-lisp.core.domain:make-artifact
                      :kind :note
                      :cas-hash "not-a-valid-hash"
                      :cas-type :markdown))
           (signaled nil))
      (handler-case
          (claw-lisp.core.artifacts:resolve-artifact-from-cas runtime artifact)
        (claw-lisp.storage.cas:cas-invalid-hash-error ()
          (setf signaled t)))
      (%assert signaled
               "Expected invalid artifact hash to be rejected before resolution")))
  (format t "~&+ test-resolve-artifact-invalid-hash-rejected passed~%")
  t)

(defun test-resolve-artifact-falls-back-to-legacy-state-root ()
  (let* ((temp-root (merge-pathnames
                     (format nil "claw-lisp-legacy-cas-~D-~D/"
                             (get-universal-time)
                             (get-internal-real-time))
                     (uiop:temporary-directory))))
    (unwind-protect
         (progn
           (ensure-directories-exist temp-root)
           (uiop:with-current-directory (temp-root)
             (let* ((legacy-config (claw-lisp.config:make-default-runtime-config))
                    (legacy-runtime (make-runtime :config legacy-config))
                    (artifact (claw-lisp.core.artifacts:persist-artifact-to-cas
                               legacy-runtime :note "legacy artifact" :type :markdown))
                    (new-config (claw-lisp.config:load-runtime-config
                                 :overrides '(:state-root ".achatina/")))
                    (new-runtime (make-runtime :config new-config)))
               (%assert (string= "legacy artifact"
                                 (claw-lisp.core.artifacts:resolve-artifact-from-cas
                                  new-runtime artifact))
                        "Expected CAS artifact resolution to fall back to legacy state root"))))
      (when (uiop:directory-exists-p temp-root)
        (uiop:delete-directory-tree temp-root :validate t))))
  (format t "~&+ test-resolve-artifact-falls-back-to-legacy-state-root passed~%")
  t)

(defun test-binary-artifact-payload-rejected-consistently ()
  (let ((serialized nil)
        (signaled nil))
    (handler-case
        (setf serialized (claw-lisp.core.artifacts::%serialize-artifact-payload
                          #(1 2 3) :binary))
      (error ()
        (setf signaled t)))
    (%assert signaled
             "Expected binary artifact serialization to be rejected")
    (setf signaled nil)
    (handler-case
        (claw-lisp.core.artifacts::%deserialize-artifact-payload "raw-bytes" :binary)
      (error ()
        (setf signaled t)))
    (%assert signaled
             "Expected binary artifact deserialization to be rejected")
    (%assert (null serialized)
             "Expected binary serialization to fail before producing output"))
  (format t "~&+ test-binary-artifact-payload-rejected-consistently passed~%")
  t)

(defun run-artifacts-tests ()
  (format t "~&=== Phase 10 CAS Artifact Facade Tests ===~%")
  (test-tool-result-cas-roundtrip)
  (test-tool-result-ref-history-recorded)
  (test-artifact-persistence-roundtrip)
  (test-tool-result-cas-without-ref-root-best-effort)
  (test-tool-result-cas-without-cas-root-best-effort)
  (test-tool-result-dedup-reuses-existing-cas)
  (test-tool-result-dedup-index-prunes-to-limit)
  (test-tool-result-dedup-lru-promotes-reused-entry)
  (test-tool-result-large-truncated-roundtrip)
  (test-tool-result-resolves-after-runtime-reload)
  (test-legacy-tool-result-compatibility-migrates-to-cas)
  (test-legacy-tool-result-compatibility-falls-back-on-cas-write-failure)
  (test-legacy-tool-result-invalid-cas-hash-still-rejected)
  (test-legacy-tool-result-unexpected-compatibility-error-propagates)
  (test-legacy-tool-result-batch-migration-preserves-compatibility)
  (test-legacy-tool-result-batch-migration-is-rollback-safe-on-failure)
  (test-migration-script-is-idempotent)
  (test-migration-script-strict-and-nonstrict-exit-codes)
  (test-migration-script-cli-edge-cases)
  (test-tool-result-dedup-isolated-per-runtime)
  (test-persist-artifact-without-cas-root-best-effort)
  (test-resolve-artifact-invalid-hash-rejected)
  (test-resolve-artifact-falls-back-to-legacy-state-root)
  (test-binary-artifact-payload-rejected-consistently)
  (format t "~&=== CAS artifact facade tests passed ===~%"))

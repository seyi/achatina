(in-package #:claw-lisp.core.artifacts)

(defparameter *tool-result-dedup-index-max-entries* 1024
  "Maximum number of dedup index entries retained in memory.")

(defun %tool-result-dedup-index (runtime)
  (claw-lisp.core.runtime:runtime-tool-result-dedup-index runtime))

(defstruct (tool-result-dedup-entry
            (:constructor %make-tool-result-dedup-entry (key cas-hash)))
  key
  cas-hash
  prev
  next)

(defun %tool-result-dedup-index-head (runtime)
  (claw-lisp.core.runtime::runtime-tool-result-dedup-index-head runtime))

(defun %tool-result-dedup-index-tail (runtime)
  (claw-lisp.core.runtime::runtime-tool-result-dedup-index-tail runtime))

(defun (setf %tool-result-dedup-index-head) (value runtime)
  (setf (claw-lisp.core.runtime::runtime-tool-result-dedup-index-head runtime) value))

(defun (setf %tool-result-dedup-index-tail) (value runtime)
  (setf (claw-lisp.core.runtime::runtime-tool-result-dedup-index-tail runtime) value))

(defun clear-tool-result-dedup-index (runtime)
  "Clear the in-memory tool-result dedup index."
  (clrhash (%tool-result-dedup-index runtime))
  (setf (%tool-result-dedup-index-head runtime) nil
        (%tool-result-dedup-index-tail runtime) nil)
  t)

(defun tool-result-dedup-index-size (runtime)
  "Return the number of dedup entries currently retained in memory."
  (hash-table-count (%tool-result-dedup-index runtime)))

(defun %tool-result-dedup-index-prune (runtime)
  (loop while (> (hash-table-count (%tool-result-dedup-index runtime))
                 *tool-result-dedup-index-max-entries*)
        do (let ((oldest (%tool-result-dedup-index-head runtime)))
             (when oldest
               (remhash (tool-result-dedup-entry-key oldest)
                        (%tool-result-dedup-index runtime))
               (%tool-result-dedup-entry-detach runtime oldest)))))

(defun %tool-result-dedup-entry-detach (runtime entry)
  (let ((prev (tool-result-dedup-entry-prev entry))
        (next (tool-result-dedup-entry-next entry)))
    (cond
      (prev
       (setf (tool-result-dedup-entry-next prev) next))
      (t
       (setf (%tool-result-dedup-index-head runtime) next)))
    (cond
      (next
       (setf (tool-result-dedup-entry-prev next) prev))
      (t
       (setf (%tool-result-dedup-index-tail runtime) prev)))
    (setf (tool-result-dedup-entry-prev entry) nil
          (tool-result-dedup-entry-next entry) nil)
    entry))

(defun %tool-result-dedup-entry-attach-tail (runtime entry)
  (setf (tool-result-dedup-entry-prev entry) (%tool-result-dedup-index-tail runtime)
        (tool-result-dedup-entry-next entry) nil)
  (if (%tool-result-dedup-index-tail runtime)
      (setf (tool-result-dedup-entry-next (%tool-result-dedup-index-tail runtime)) entry)
      (setf (%tool-result-dedup-index-head runtime) entry))
  (setf (%tool-result-dedup-index-tail runtime) entry)
  entry)

(defun %tool-result-dedup-index-touch (runtime key cas-hash)
  (let ((entry (gethash key (%tool-result-dedup-index runtime))))
    (if entry
        (progn
          (setf (tool-result-dedup-entry-cas-hash entry) cas-hash)
          (%tool-result-dedup-entry-detach runtime entry)
          (%tool-result-dedup-entry-attach-tail runtime entry))
        (let ((new-entry (%make-tool-result-dedup-entry key cas-hash)))
          (setf (gethash key (%tool-result-dedup-index runtime)) new-entry)
          (%tool-result-dedup-entry-attach-tail runtime new-entry))))
  (%tool-result-dedup-index-prune runtime)
  cas-hash)

(defun runtime-effective-cas-root (runtime)
  "Return the configured CAS object root for RUNTIME."
  (let ((root (claw-lisp.config:runtime-config-cas-objects-root
               (claw-lisp.core.runtime:runtime-settings runtime))))
    (unless (or (null root) (string= "" root))
      root)))

(defun runtime-effective-cas-ref-root (runtime)
  "Return the configured CAS ref root for RUNTIME."
  (let ((root (claw-lisp.config:runtime-config-cas-ref-root
               (claw-lisp.core.runtime:runtime-settings runtime))))
    (unless (or (null root) (string= "" root))
      root)))

(defun %normalize-tool-name (tool-name)
  (let ((text (string-downcase (string tool-name))))
    (map-into text
              (lambda (ch)
                (if (or (alphanumericp ch)
                        (member ch '(#\- #\_ #\. ) :test #'char=))
                    ch
                    #\-))
              text)))

(defun %artifact-ref-component (cas-hash)
  (let ((digest (claw-lisp.storage.cas:hash-digest cas-hash)))
    (subseq digest 0 (min 16 (length digest)))))

(defun %normalize-legacy-path (legacy-path)
  (typecase legacy-path
    (pathname (namestring legacy-path))
    (string legacy-path)
    (t (princ-to-string legacy-path))))

(defun legacy-path-cas-ref-name (legacy-path)
  "Return the deterministic CAS ref name used for LEGACY-PATH compatibility mappings."
  (let* ((normalized (%normalize-legacy-path legacy-path))
         (digest (claw-lisp.storage.cas:hash-digest
                  (claw-lisp.storage.cas:cas-hash normalized))))
    (format nil "legacy-paths/~A" digest)))

(defun %ensure-valid-versioned-hash-or-error (versioned-hash)
  (when versioned-hash
    (unless (claw-lisp.storage.cas:valid-versioned-hash-p versioned-hash)
      (error 'claw-lisp.storage.cas:cas-invalid-hash-error
             :hash versioned-hash))
    versioned-hash))

(defun %record-legacy-path-cas-mapping (runtime legacy-path cas-hash metadata)
  (let ((ref-root (runtime-effective-cas-ref-root runtime)))
    (when ref-root
      (let ((ref-name (legacy-path-cas-ref-name legacy-path)))
        (write-cas-ref ref-root ref-name cas-hash
                       :record-history-p t
                       :metadata (append
                                  (list :legacy-path (%normalize-legacy-path legacy-path))
                                  metadata))
        ref-name))))

(defun %resolve-legacy-path-cas-mapping (runtime legacy-path &key (require-object-p nil))
  (let ((ref-root (runtime-effective-cas-ref-root runtime))
        (cas-root (runtime-effective-cas-root runtime)))
    (when (and ref-root cas-root)
      (let* ((ref-name (legacy-path-cas-ref-name legacy-path))
             (record (claw-lisp.storage.cas-ref:read-cas-ref ref-root ref-name))
             (cas-hash (and record
                            (claw-lisp.storage.cas-ref:resolve-cas-ref
                             ref-root cas-root ref-name :require-object-p require-object-p))))
        (values cas-hash ref-name record)))))

(defun %legacy-tool-results-root (runtime &key session-id)
  (let ((artifacts-root
          (claw-lisp.config:runtime-config-artifacts-root
           (claw-lisp.core.runtime:runtime-settings runtime))))
    (unless (or (null artifacts-root) (string= "" artifacts-root))
      (merge-pathnames
       (if session-id
           (make-pathname :directory `(:relative "tool-results" ,session-id))
           (make-pathname :directory '(:relative "tool-results")))
       (uiop:ensure-directory-pathname artifacts-root)))))

(defun %file-size-bytes (path)
  (when (probe-file path)
    (with-open-file (stream path :direction :input :element-type '(unsigned-byte 8))
      (file-length stream))))

(defun %collect-files-recursively (root)
  (when (probe-file root)
    (sort
     (labels ((collect (dir)
                (append
                 (uiop:directory-files dir)
                 (loop for subdir in (uiop:subdirectories dir)
                       append (collect subdir)))))
       (collect root))
     #'string-lessp
     :key #'namestring)))

(defun tool-result->artifact (result)
  "Return an ARTIFACT handle for RESULT when it is CAS-backed."
  (let ((hash (tool-result-cas-hash result)))
    (when hash
      (%ensure-valid-versioned-hash-or-error hash)
      (make-artifact
       :id (tool-result-call-id result)
       :kind :tool-result
       :cas-hash hash
       :cas-type (tool-result-cas-type result)
       :cas-ref-name (tool-result-cas-ref-name result)
       :metadata (list :tool-name (tool-result-tool-name result)
                       :bytes (tool-result-bytes result)
                       :truncated-p (tool-result-truncated-p result))))))

(defun %tool-result-best-effort-artifact (tool-result)
  (make-artifact
   :id (tool-result-call-id tool-result)
   :kind :tool-result
   :cas-type :markdown
   :cas-ref-name nil
   :metadata (list :tool-name (tool-result-tool-name tool-result)
                   :bytes (tool-result-bytes tool-result)
                   :truncated-p (tool-result-truncated-p tool-result))))

(defun compute-tool-result-dedup-key (tool-result)
  "Return a canonical deduplication key for TOOL-RESULT."
  (let* ((tool-name (tool-result-tool-name tool-result))
         (content (or (tool-result-content tool-result) ""))
         (normalized-content (string-trim '(#\Space #\Tab #\Newline #\Return)
                                          content))
         (content-hash (claw-lisp.storage.cas:cas-hash normalized-content)))
    (claw-lisp.storage.cas:canonicalize-plist
     (list :kind :tool-result
           :tool-name tool-name
           :content-hash content-hash
           :truncated-p (tool-result-truncated-p tool-result)
           :bytes (tool-result-bytes tool-result)))))

(defun %tool-result-ref-name (tool-result cas-hash)
  (format nil "tool-results/~A/~A"
          (%normalize-tool-name (tool-result-tool-name tool-result))
          (%artifact-ref-component cas-hash)))

(defun store-tool-result-cas (runtime tool-result
                               &key
                                 (dedup-p t)
                                 (write-ref-p t))
  "Persist TOOL-RESULT into CAS and return updated TOOL-RESULT and ARTIFACT."
  (let ((cas-root (runtime-effective-cas-root runtime)))
    (unless cas-root
      (return-from store-tool-result-cas
        (values tool-result
                (%tool-result-best-effort-artifact tool-result)
                nil)))
    (let* ((content (or (tool-result-content tool-result) ""))
           (key (and dedup-p (compute-tool-result-dedup-key tool-result)))
           (existing-entry (and key (gethash key (%tool-result-dedup-index runtime))))
           (existing-hash (and existing-entry
                               (tool-result-dedup-entry-cas-hash existing-entry)))
           (deduplicated-p (and existing-hash
                                (claw-lisp.storage.cas:cas-exists-p cas-root existing-hash)))
           (cas-hash (or (and existing-hash
                              deduplicated-p
                              existing-hash)
                         (claw-lisp.storage.cas:cas-put cas-root content)))
           (ref-root (runtime-effective-cas-ref-root runtime))
           (ref-name (when (and write-ref-p ref-root)
                       (%tool-result-ref-name tool-result cas-hash)))
           (artifact (make-artifact
                      :id (tool-result-call-id tool-result)
                      :kind :tool-result
                      :cas-hash cas-hash
                      :cas-type :markdown
                      :cas-ref-name ref-name
                      :metadata (list :tool-name (tool-result-tool-name tool-result)
                                      :bytes (tool-result-bytes tool-result)
                                      :truncated-p (tool-result-truncated-p tool-result)))))
      (when (and dedup-p key)
        (%tool-result-dedup-index-touch runtime key cas-hash))
      (when ref-name
        (write-cas-ref ref-root ref-name cas-hash
                       :record-history-p t
                       :metadata (artifact-metadata artifact)))
      (when (tool-result-persisted-path tool-result)
        (%record-legacy-path-cas-mapping
         runtime
         (tool-result-persisted-path tool-result)
         cas-hash
         (list :kind :tool-result
               :call-id (tool-result-call-id tool-result)
               :tool-name (tool-result-tool-name tool-result)
               :bytes (tool-result-bytes tool-result))))
      (let ((updated (make-tool-result
                      :call-id (tool-result-call-id tool-result)
                      :tool-name (tool-result-tool-name tool-result)
                      :content (tool-result-content tool-result)
                      :persisted-path (tool-result-persisted-path tool-result)
                      :truncated-p (tool-result-truncated-p tool-result)
                      :bytes (tool-result-bytes tool-result)
                      :cas-hash cas-hash
                      :cas-type :markdown
                      :cas-ref-name ref-name
                      :artifact artifact)))
        (values updated artifact
                (list :kind :tool-result
                      :cas-hash cas-hash
                      :cas-type :markdown
                      :cas-ref-name ref-name
                      :bytes (tool-result-bytes tool-result)
                      :deduplicated-p (and deduplicated-p t)
                      :tool-name (tool-result-tool-name tool-result)
                      :call-id (tool-result-call-id tool-result)))))))

(defun ensure-tool-result-cas-compatibility (runtime tool-result
                                              &key
                                                (dedup-p t)
                                                (write-ref-p t))
  "Ensure legacy TOOL-RESULT persisted-path data is represented in CAS.

When TOOL-RESULT already has CAS metadata, return it unchanged. When the result
only has a legacy PERSISTED-PATH and CAS is configured, ingest the full legacy
payload into CAS and return an updated TOOL-RESULT that preserves the original
working-context fields while attaching CAS hash/ref metadata."
  (let ((path (tool-result-persisted-path tool-result)))
    (when (or (tool-result-cas-hash tool-result)
              (tool-result-cas-ref-name tool-result)
              (null path)
              (null (runtime-effective-cas-root runtime)))
      (return-from ensure-tool-result-cas-compatibility
        (values tool-result (tool-result->artifact tool-result) nil)))
    (multiple-value-bind (mapped-hash mapped-ref-name)
        (%resolve-legacy-path-cas-mapping runtime path :require-object-p nil)
      (when mapped-hash
        (let ((artifact (make-artifact
                         :id (tool-result-call-id tool-result)
                         :kind :tool-result
                         :cas-hash mapped-hash
                         :cas-type :markdown
                         :cas-ref-name mapped-ref-name
                         :metadata (list :tool-name (tool-result-tool-name tool-result)
                                         :bytes (tool-result-bytes tool-result)
                                         :truncated-p (tool-result-truncated-p tool-result)
                                         :legacy-path path))))
          (return-from ensure-tool-result-cas-compatibility
            (values
             (claw-lisp.core.domain::%copy-tool-result-with
              tool-result
              :cas-hash mapped-hash
              :cas-type :markdown
              :cas-ref-name mapped-ref-name
              :artifact artifact)
             artifact
             nil))))
      (unless (probe-file path)
        (return-from ensure-tool-result-cas-compatibility
          (values tool-result (tool-result->artifact tool-result) nil)))
      (let* ((full-content (uiop:read-file-string path))
             (compatibility-source
              (claw-lisp.core.domain::%copy-tool-result-with
               tool-result
               :content full-content
               :bytes (max (tool-result-bytes tool-result)
                           (length full-content)))))
        (multiple-value-bind (cas-backed artifact descriptor)
            (store-tool-result-cas runtime compatibility-source
                                   :dedup-p dedup-p
                                   :write-ref-p write-ref-p)
          (values
           (claw-lisp.core.domain::%copy-tool-result-with
            tool-result
            :bytes (tool-result-bytes cas-backed)
            :cas-hash (tool-result-cas-hash cas-backed)
            :cas-type (tool-result-cas-type cas-backed)
            :cas-ref-name (tool-result-cas-ref-name cas-backed)
            :artifact artifact)
           artifact
           descriptor))))))

(defun migrate-legacy-artifact-path-to-cas (runtime legacy-path
                                             &key
                                               metadata
                                               (allow-missing-p nil))
  "Ingest one legacy artifact file into CAS and write a path-compatibility ref.

Returns a plist describing the migrated object. When ALLOW-MISSING-P is true,
missing files return a failure descriptor instead of signaling."
  (let* ((normalized-path (%normalize-legacy-path legacy-path))
         (cas-root (runtime-effective-cas-root runtime))
         (ref-root (runtime-effective-cas-ref-root runtime)))
    (unless cas-root
      (error "Cannot migrate legacy artifacts without a configured CAS root."))
    (unless ref-root
      (error "Cannot migrate legacy artifacts without a configured CAS ref root."))
    (unless (probe-file normalized-path)
      (if allow-missing-p
          (return-from migrate-legacy-artifact-path-to-cas
            (list :legacy-path normalized-path
                  :status :missing))
          (error "Legacy artifact path does not exist: ~A" normalized-path)))
    (multiple-value-bind (existing-hash existing-ref-name)
        (%resolve-legacy-path-cas-mapping runtime normalized-path :require-object-p nil)
      (if (and existing-hash
               (claw-lisp.storage.cas:cas-exists-p cas-root existing-hash))
          (list :legacy-path normalized-path
                :cas-hash existing-hash
                :cas-ref-name existing-ref-name
                :bytes (%file-size-bytes normalized-path)
                :status :already-mapped)
          (let* ((content (uiop:read-file-string normalized-path))
                 (cas-hash (claw-lisp.storage.cas:cas-put cas-root content))
                 (ref-name (%record-legacy-path-cas-mapping
                            runtime normalized-path cas-hash
                            (append metadata
                                    (list :kind :legacy-tool-result
                                          :bytes (length content))))))
            (list :legacy-path normalized-path
                  :cas-hash cas-hash
                  :cas-ref-name ref-name
                  :bytes (length content)
                  :status :migrated))))))

(defun migrate-legacy-tool-results-to-cas (runtime
                                           &key
                                             session-id
                                             (continue-on-error-p t))
  "Batch-ingest legacy persisted tool-result files into CAS.

Returns a summary plist with migrated, already-mapped, and failed paths."
  (let ((root (%legacy-tool-results-root runtime :session-id session-id)))
    (unless root
      (error "Cannot migrate legacy tool results without an artifacts root."))
    (let ((files (%collect-files-recursively root))
          (migrated '())
          (already-mapped '())
          (failures '()))
      (dolist (path files)
        (handler-case
            (let ((result (migrate-legacy-artifact-path-to-cas
                           runtime path
                           :metadata (when session-id (list :session-id session-id)))))
              (case (getf result :status)
                (:already-mapped (push result already-mapped))
                (otherwise (push result migrated))))
          (error (condition)
            (let ((failure (list :legacy-path (%normalize-legacy-path path)
                                 :status :failed
                                 :reason (princ-to-string condition))))
              (unless continue-on-error-p
                (error 'legacy-tool-results-migration-aborted
                       :summary (list :root (namestring (uiop:ensure-directory-pathname root))
                                      :session-id session-id
                                      :scanned-count (length files)
                                      :migrated-count (length migrated)
                                      :already-mapped-count (length already-mapped)
                                      :failure-count 1
                                      :migrated (nreverse migrated)
                                      :already-mapped (nreverse already-mapped)
                                      :failures (list failure))
                       :cause condition))
              (push failure failures)))))
      (list :root (namestring (uiop:ensure-directory-pathname root))
            :session-id session-id
            :scanned-count (length files)
            :migrated-count (length migrated)
            :already-mapped-count (length already-mapped)
            :failure-count (length failures)
            :migrated (nreverse migrated)
            :already-mapped (nreverse already-mapped)
            :failures (nreverse failures)))))

(define-condition legacy-tool-results-migration-aborted (error)
  ((summary
    :initarg :summary
    :reader legacy-tool-results-migration-aborted-summary)
   (cause
    :initarg :cause
    :reader legacy-tool-results-migration-aborted-cause))
  (:report (lambda (condition stream)
             (format stream
                     "Legacy tool-result migration aborted: ~A"
                     (legacy-tool-results-migration-aborted-cause condition)))))

(defun resolve-tool-result-cas (runtime tool-result &key (prefer-cas-p t))
  "Return TOOL-RESULT content, preferring CAS, then persisted-path, then in-memory content."
  (when (and prefer-cas-p
             (tool-result-cas-hash tool-result))
    (%ensure-valid-versioned-hash-or-error (tool-result-cas-hash tool-result)))
  (let* ((compatible-result
           (if prefer-cas-p
               (handler-case
                   (nth-value 0 (ensure-tool-result-cas-compatibility runtime tool-result))
                 (error (condition)
                   (typecase condition
                     ((or claw-lisp.storage.cas:cas-error
                          claw-lisp.storage.cas-ref:cas-ref-error
                          file-error)
                      (warn "Tool-result CAS compatibility migration failed for ~A: ~A"
                            (tool-result-call-id tool-result)
                            condition)
                      tool-result)
                     (t
                      (error condition)))))
               tool-result))
         (cas-root (runtime-effective-cas-root runtime))
         (ref-root (runtime-effective-cas-ref-root runtime))
         (cas-hash (tool-result-cas-hash compatible-result))
         (ref-name (tool-result-cas-ref-name compatible-result)))
    (%ensure-valid-versioned-hash-or-error cas-hash)
    (when (and prefer-cas-p cas-root)
      (when (and ref-name ref-root)
        (let ((resolved (resolve-cas-ref ref-root cas-root ref-name
                                         :require-object-p nil)))
          (when resolved
            (let ((text (cas-get cas-root resolved)))
              (when text
                (return-from resolve-tool-result-cas text))))))
      (when (and cas-hash (claw-lisp.storage.cas:cas-exists-p cas-root cas-hash))
        (let ((text (cas-get cas-root cas-hash)))
          (when text
            (return-from resolve-tool-result-cas text)))))
    (or (let ((path (tool-result-persisted-path compatible-result)))
          (when (and path (probe-file path))
            (uiop:read-file-string path)))
        (tool-result-content compatible-result))))

(defun %serialize-artifact-payload (payload type)
  (ecase type
    (:sexp
     (with-output-to-string (stream)
       (let ((*print-circle* nil)
             (*print-readably* t)
             (*print-pretty* nil))
         (write payload :stream stream))))
    (:json
     (claw-lisp.providers.http-utils:json-encode-string payload))
    (:markdown
     (unless (stringp payload)
       (error "Markdown artifacts require string payloads, got ~S" payload))
     payload)
    (:binary
     (error "Binary artifact payloads are not supported yet."))))

(defun persist-artifact-to-cas (runtime kind payload
                                 &key (type :sexp) ref-name metadata)
  "Persist PAYLOAD as an artifact of KIND and return an ARTIFACT handle."
  (let ((cas-root (runtime-effective-cas-root runtime)))
    (unless cas-root
      (return-from persist-artifact-to-cas
        (values
         (make-artifact
          :kind kind
          :cas-type type
          :cas-ref-name ref-name
          :metadata metadata)
         nil)))
    (let* ((serialized (%serialize-artifact-payload payload type))
           (cas-hash (claw-lisp.storage.cas:cas-put cas-root serialized))
           (artifact (make-artifact
                      :kind kind
                      :cas-hash cas-hash
                      :cas-type type
                      :cas-ref-name ref-name
                      :metadata metadata))
           (ref-root (runtime-effective-cas-ref-root runtime)))
      (when (and ref-name ref-root)
        (write-cas-ref ref-root ref-name cas-hash
                       :record-history-p t
                       :metadata metadata))
      (values artifact
              (list :kind kind
                    :cas-hash cas-hash
                    :cas-type type
                    :cas-ref-name ref-name
                    :bytes (length serialized)
                    :metadata metadata)))))

(defun %deserialize-artifact-payload (text type)
  (ecase type
    (:sexp
     (let ((*read-eval* nil))
       (multiple-value-bind (value position)
           (read-from-string text)
         (declare (ignore position))
         value)))
    (:json
     (claw-lisp.providers.http-utils:json-decode text))
    (:markdown text)
    (:binary
     (error "Binary artifact payloads are not supported yet."))))

(defun resolve-artifact-from-cas (runtime artifact)
  "Fetch and deserialize ARTIFACT payload from CAS under RUNTIME."
  (let* ((cas-root (runtime-effective-cas-root runtime))
         (ref-root (runtime-effective-cas-ref-root runtime))
         (cas-hash (artifact-cas-hash artifact))
         (ref-name (artifact-cas-ref-name artifact))
         (type (artifact-cas-type artifact))
         (resolved-hash (or (and cas-root ref-name ref-root
                                 (resolve-cas-ref ref-root cas-root ref-name
                                                  :require-object-p t))
                            cas-hash)))
    (%ensure-valid-versioned-hash-or-error resolved-hash)
    (unless (and cas-root resolved-hash)
      (error "Artifact has no CAS hash or resolvable ref: ~S" artifact))
    (let ((text (cas-get cas-root resolved-hash)))
      (unless text
        (error "CAS object is missing for artifact hash: ~A" resolved-hash))
      (%deserialize-artifact-payload text type))))

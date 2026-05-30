;;;; lisp/storage/durable-memory-embeddings.lisp
;;;;
;;;; Phase 7 Task 3 — Embedding Generation Pipeline
;;;;
;;;; Hooks embedding generation into the durable memory ingestion/update pipeline.
;;;;
;;;; FIXES applied from critical review:
;;;;   BLOCKER 1: Package identity — now uses claw-lisp.storage.durable-memory-embeddings
;;;;   BLOCKER 2: Config NIL defaults — all functions default to *runtime-config*
;;;;   BLOCKER 3: Batch logic — uses hash table to map record-id → embedding
;;;;   HIGH 1: Record reconstruction — uses copy-durable-memory-record
;;;;   HIGH 2: refresh-missing-embeddings — now persists updated records
;;;;   HIGH 3: Config key fix — checks runtime-config-embedding-enabled-p directly
;;;;   HIGH 4: Subject ID discovery — scans filesystem instead of hardcoded default
;;;;   Additional: text length validation, removed redundant export call

(in-package :claw-lisp.storage.durable-memory-embeddings)

;;; ============================================================
;;; Configuration: Embedding Generation Policies
;;; ============================================================

(defparameter *durable-memory-embedding-kinds*
  '(:user :feedback :project :reference)
  "List of durable memory kinds that should have embeddings generated.
   Can be customized via configuration.")

(defparameter *durable-memory-embedding-refresh-enabled-p* nil
  "When T, enables background/opportunistic embedding refresh for records
   missing embeddings or with outdated embedding models.")

(defparameter *durable-memory-embedding-refresh-batch-size* 50
  "Number of records to process in a single embedding refresh batch.")

(defparameter *durable-memory-embedding-pending-mark* :embedding-pending
  "Marker used to indicate a record is waiting for embedding generation.")

(defparameter *durable-memory-embedding-max-text-length* 8192
  "Maximum text length (in characters) to send for embedding generation.
   Texts longer than this are truncated.")

;;; ============================================================
;;; Internal Helpers
;;; ============================================================

(defun %effective-config (config)
  "Return CONFIG if non-NIL, otherwise fall back to *runtime-config*."
  (or config claw-lisp.config:*runtime-config*))

(defun %truncate-text-for-embedding (text)
  "Truncate TEXT to *durable-memory-embedding-max-text-length* if needed.
   Returns the (possibly truncated) string, or NIL if TEXT is NIL or empty."
  (when (and text (plusp (length text)))
    (if (> (length text) *durable-memory-embedding-max-text-length*)
        (subseq text 0 *durable-memory-embedding-max-text-length*)
        text)))

(defun %copy-record-with-embedding (record embedding model)
  "Return a copy of RECORD with embedding fields updated.
   Uses the auto-generated copy-durable-memory-record from defstruct."
  (let ((copy (claw-lisp.storage.durable-memory:copy-durable-memory-record record)))
    (setf (claw-lisp.storage.durable-memory:durable-memory-record-embedding copy) embedding
          (claw-lisp.storage.durable-memory:durable-memory-record-embedding-model copy) model
          (claw-lisp.storage.durable-memory:durable-memory-record-embedding-version copy) "v1")
    copy))

(defun discover-subject-ids (kind)
  "Discover subject IDs by scanning the filesystem for a given KIND.
    Returns a list of subject-id strings found on disk.

    Note: durable memory records are persisted as .md files (see
    `durable-memory-file-path' in durable-memory.lisp), so we scan for
    that extension here."
  (let* ((storage-root claw-lisp.storage.durable-memory:*durable-memory-storage-root*)
         (kind-dir (when storage-root
                     (merge-pathnames
                      (make-pathname :directory (list :relative (string-downcase (symbol-name kind))))
                      storage-root)))
         (subject-ids '()))
    (when (and kind-dir (probe-file kind-dir))
      (let ((entries (directory (merge-pathnames
                                 (make-pathname :name :wild :type "md")
                                 kind-dir))))
        (dolist (entry entries)
          (let ((name (pathname-name entry)))
            (when name
              (pushnew name subject-ids :test #'string=))))))
    (nreverse subject-ids)))

;;; ============================================================
;;; Embedding Generation for Single Record
;;; ============================================================

(defun %generate-embedding-for-text (text &key model provider timeout-seconds)
  "Generate an embedding vector for TEXT using the configured provider.

INPUTS:
  - TEXT: string to embed.
  - MODEL: embedding model name (default: from runtime config).
  - PROVIDER: embedding provider keyword (default: from runtime config).
  - TIMEOUT-SECONDS: timeout hint (default: from runtime config).

OUTPUT:
  - list of single-float values (embedding vector), or NIL on error.

ERRORS:
  - Logs error and returns NIL if embedding generation fails."
  (let ((text (%truncate-text-for-embedding text)))
    (when (null text)
      (return-from %generate-embedding-for-text nil))

    (handler-case
        (let* ((embeddings (claw-lisp.providers:compute-embeddings
                            (list text)
                            :provider provider
                            :model model
                            :timeout-seconds timeout-seconds
                            :signal-errors-p nil)))
          (car embeddings))
      (error (e)
        (format *error-output* "~&[DURABLE-MEMORY-EMBEDDING] Failed to generate embedding: ~A~%" e)
        nil))))

(defun %should-generate-embedding-p (record config)
  "Determine whether RECORD should have an embedding generated.

Returns T if:
  - Embeddings are enabled in CONFIG.
  - Record's kind is in *DURABLE-MEMORY-EMBEDDING-KINDS*.
  - Record does not already have an embedding, or embedding is outdated.
  - Record has non-empty content.

Returns NIL otherwise."
  (let* ((effective-config (%effective-config config))
         (embedding-enabled-p (claw-lisp.config:runtime-config-embedding-enabled-p
                               effective-config))
         (kind (claw-lisp.storage.durable-memory:durable-memory-record-kind record))
         (content (claw-lisp.storage.durable-memory:durable-memory-record-content record))
         (existing-embedding (claw-lisp.storage.durable-memory:durable-memory-record-embedding record))
         (existing-model (claw-lisp.storage.durable-memory:durable-memory-record-embedding-model record))
         (configured-model (claw-lisp.config:runtime-config-embedding-model effective-config)))
    (and embedding-enabled-p
         (member kind *durable-memory-embedding-kinds* :test #'eq)
         content
         (plusp (length content))
         (or (null existing-embedding)
             (and configured-model
                  (string/= existing-model configured-model))))))

(defun generate-embedding-for-record (record &key config)
  "Generate and attach an embedding to RECORD.

INPUTS:
  - RECORD: durable-memory-record to embed.
  - CONFIG: runtime config (optional, defaults to *runtime-config*).

OUTPUT:
  - Updated record with embedding fields populated, or original record
    if embedding generation was skipped/failed.

SIDE EFFECTS:
  - Updates *durable-memory-embedding-index* with the new embedding."
  (unless (%should-generate-embedding-p record config)
    (return-from generate-embedding-for-record record))

  (let* ((effective-config (%effective-config config))
         (text (claw-lisp.storage.durable-memory:durable-memory-record-content record))
         (model (claw-lisp.config:runtime-config-embedding-model effective-config))
         (provider (claw-lisp.config:runtime-config-embedding-provider effective-config))
         (timeout (claw-lisp.config:runtime-config-embedding-timeout-seconds effective-config))
         (embedding (%generate-embedding-for-text text
                                                   :model model
                                                   :provider provider
                                                   :timeout-seconds timeout)))
    (if embedding
        (let ((updated (%copy-record-with-embedding record embedding model)))
          ;; Update index
          (claw-lisp.storage.durable-memory:update-embedding-index
           (claw-lisp.storage.durable-memory:durable-memory-record-kind updated)
           (claw-lisp.storage.durable-memory:durable-memory-record-id updated)
           embedding)
          updated)
        record)))

;;; ============================================================
;;; Batch Embedding Generation
;;; ============================================================

(defun generate-embeddings-for-records (records &key config)
  "Generate embeddings for a list of RECORDS in batch.

INPUTS:
  - RECORDS: list of durable-memory-record.
  - CONFIG: runtime config (optional, defaults to *runtime-config*).

OUTPUT:
  - List of ALL records (same order as input), with embeddings attached
    to those that needed and successfully received them.

NOTES:
  - Filters out records that already have embeddings or don't need them.
  - Uses batch API for efficiency when possible.
  - Returns all input records, not just the ones that were embedded."
  (let* ((to-embed (remove-if-not
                    (lambda (r) (%should-generate-embedding-p r config))
                    records))
         (texts (mapcar (lambda (r)
                          (%truncate-text-for-embedding
                           (claw-lisp.storage.durable-memory:durable-memory-record-content r)))
                        to-embed)))
    (if (null texts)
        records
        (let* ((effective-config (%effective-config config))
               (model (claw-lisp.config:runtime-config-embedding-model effective-config))
               (provider (claw-lisp.config:runtime-config-embedding-provider effective-config))
               (timeout (claw-lisp.config:runtime-config-embedding-timeout-seconds effective-config))
               (embeddings (handler-case
                               (claw-lisp.providers:compute-embeddings
                                texts
                                :provider provider
                                :model model
                                :timeout-seconds timeout
                                :signal-errors-p nil)
                             (error (e)
                               (format *error-output*
                                       "~&[DURABLE-MEMORY-EMBEDDING] Batch embedding failed: ~A~%" e)
                               nil))))
          (if (null embeddings)
              records
              ;; Build a hash table mapping record-id → new embedding
              (let ((embedding-map (make-hash-table :test #'equal)))
                ;; Pair up to-embed records with their embeddings.
                ;; If compute-embeddings returned fewer results than to-embed,
                ;; only the first N get embeddings (rest are left without).
                (loop for rec in to-embed
                      for emb in embeddings
                      when emb
                      do (setf (gethash
                                (claw-lisp.storage.durable-memory:durable-memory-record-id rec)
                                embedding-map)
                               emb))
                ;; Map over ALL records, updating those that got embeddings
                (mapcar (lambda (record)
                          (let ((new-embedding
                                  (gethash
                                   (claw-lisp.storage.durable-memory:durable-memory-record-id record)
                                   embedding-map)))
                            (if new-embedding
                                (let ((updated (%copy-record-with-embedding
                                                record new-embedding model)))
                                  (claw-lisp.storage.durable-memory:update-embedding-index
                                   (claw-lisp.storage.durable-memory:durable-memory-record-kind updated)
                                   (claw-lisp.storage.durable-memory:durable-memory-record-id updated)
                                   new-embedding)
                                  updated)
                                record)))
                        records)))))))

;;; ============================================================
;;; Background/Opportunistic Embedding Refresh
;;; ============================================================

(defun find-records-missing-embeddings (&key kinds limit config)
  "Find durable memory records that are missing embeddings.

INPUTS:
  - KINDS: list of kinds to scan (default: *DURABLE-MEMORY-EMBEDDING-KINDS*).
  - LIMIT: maximum number of records to return.
  - CONFIG: runtime config (optional, defaults to *runtime-config*).

OUTPUT:
  - List of durable-memory-record missing embeddings."
  (let* ((kinds (or kinds *durable-memory-embedding-kinds*))
         (missing '()))
    (dolist (kind kinds)
       (let ((subject-ids (discover-subject-ids kind)))
        (dolist (subject-id subject-ids)
          (let ((records (claw-lisp.storage.durable-memory:load-durable-memories
                          kind subject-id)))
            (dolist (record records)
              (when (and (null (claw-lisp.storage.durable-memory:durable-memory-record-embedding record))
                         (member kind *durable-memory-embedding-kinds* :test #'eq)
                         (let ((content (claw-lisp.storage.durable-memory:durable-memory-record-content record)))
                           (and content (plusp (length content)))))
                (push record missing)
                (when (and limit (>= (length missing) limit))
                  (return-from find-records-missing-embeddings (nreverse missing)))))))))
    (nreverse missing)))

(defun refresh-missing-embeddings (&key kinds limit batch-size config)
  "Generate embeddings for records missing them.

INPUTS:
  - KINDS: list of kinds to process.
  - LIMIT: maximum total records to process.
  - BATCH-SIZE: records per batch (default: *DURABLE-MEMORY-EMBEDDING-REFRESH-BATCH-SIZE*).
  - CONFIG: runtime config (optional, defaults to *runtime-config*).

OUTPUT:
  - Number of embeddings successfully generated and persisted.

SIDE EFFECTS:
  - Updates records on disk and in index."
  (let* ((batch-size (or batch-size *durable-memory-embedding-refresh-batch-size*))
         (missing (find-records-missing-embeddings :kinds kinds :limit limit :config config))
         (generated-count 0))
    (when (null missing)
      (return-from refresh-missing-embeddings 0))

    ;; Process in batches
    (loop
      with batch = '()
      for record in missing
      do (push record batch)
      when (>= (length batch) batch-size)
      do (let ((updated (generate-embeddings-for-records (nreverse batch) :config config)))
           ;; Persist updated records that received embeddings
           (dolist (rec updated)
             (when (claw-lisp.storage.durable-memory:durable-memory-record-embedding rec)
               (claw-lisp.storage.durable-memory:save-durable-memory-record rec)
               (incf generated-count)))
           (setf batch '()))
      finally (when batch
                (let ((updated (generate-embeddings-for-records (nreverse batch) :config config)))
                  (dolist (rec updated)
                    (when (claw-lisp.storage.durable-memory:durable-memory-record-embedding rec)
                      (claw-lisp.storage.durable-memory:save-durable-memory-record rec)
                      (incf generated-count))))))
    generated-count))

;;; ============================================================
;;; Integration: Hook into Ingestion Pipeline
;;; ============================================================

(defun ingest-durable-memory-with-embedding (conversation session-memory subject-id
                                             &key (force-p nil) config)
  "Ingest durable memories with embedding generation.

This is a wrapper around INGEST-DURABLE-MEMORY-FROM-SESSION that:
  1. Calls the base ingestion function.
  2. Generates embeddings for newly saved records.
  3. Persists the updated records with embeddings.
  4. Returns the list of saved records (with embeddings).

See INGEST-DURABLE-MEMORY-FROM-SESSION for parameter details."
  (let ((saved (claw-lisp.storage.durable-memory:ingest-durable-memory-from-session
                conversation session-memory subject-id
                :force-p force-p :config config)))
    (if (null saved)
        saved
        ;; Generate embeddings for newly saved records
        (let ((with-embeddings (generate-embeddings-for-records saved :config config)))
          ;; Persist only records that actually received new embeddings
          ;; (avoid double-saving records that didn't get embeddings)
          (dolist (record with-embeddings)
            (when (claw-lisp.storage.durable-memory:durable-memory-record-embedding record)
              ;; Re-save to persist embedding to disk
              (claw-lisp.storage.durable-memory:save-durable-memory-record record)))
          with-embeddings))))

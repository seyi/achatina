
;;;; lisp/storage/durable-memory-search.lisp
;;;;
;;;; Phase 7 Task 4 — Semantic Similarity Search API
;;;;
;;;; Provides semantic similarity search over durable memory records.
;;;; Given a natural-language query, computes its embedding, scans the
;;;; in-memory embedding index for similar records, ranks them by cosine
;;;; similarity (optionally blended with importance/staleness scores),
;;;; and returns a scored, filtered result set.
;;;;
;;;; Depends on:
;;;;   - claw-lisp.storage.durable-memory (domain model, embedding index)
;;;;   - claw-lisp.providers (compute-embeddings for query embedding)
;;;;   - claw-lisp.config (runtime configuration)

(in-package :claw-lisp.storage.durable-memory-search)

;;; ============================================================
;;; Parameters
;;; ============================================================

(defparameter *semantic-search-default-hybrid-weight* 0.7s0
  "Default alpha for hybrid scoring when not specified in config.
    0.7 = 70% semantic, 30% importance.")

(defparameter *semantic-search-kind-hybrid-weights*
  '((:user . 0.6s0)
    (:feedback . 0.5s0)
    (:project . 0.7s0)
    (:reference . 0.8s0))
  "Alist mapping durable memory kind → hybrid weight (alpha).
   Kinds not listed here fall back to the global default.

   Rationale for defaults:
     :user      — 0.6: user preferences benefit from importance weighting
     :feedback  — 0.5: balanced; feedback relevance is both semantic and temporal
     :project   — 0.7: project facts are primarily semantic
     :reference — 0.8: reference material is almost purely semantic")

(defparameter *default-search-kinds*
  '(:user :feedback :project :reference)
  "Default kinds to search when none specified.")

;;; ============================================================
;;; Helper Functions
;;; ============================================================

(defun %effective-config (config)
  "Return CONFIG if non-NIL, otherwise fall back to *runtime-config*."
  (or config claw-lisp.config:*runtime-config*))

;;; ============================================================
;;; Core Similarity Functions
;;; ============================================================

(defun coerce-to-float-array (embedding)
  "Coerce EMBEDDING (a sequence of numbers) to (simple-array single-float (*)).

INPUTS:
  EMBEDDING — a sequence of numbers (typically single-float).

OUTPUT:
  A (simple-array single-float (*)) of the same length.
  Returns NIL if EMBEDDING is NIL or empty."
  (when (and embedding (plusp (length embedding)))
    (let* ((len (length embedding))
           (arr (make-array len :element-type 'single-float)))
      (dotimes (i len arr)
        (setf (aref arr i) (coerce (elt embedding i) 'single-float))))))

(defun compute-cosine-similarity-arrays (arr1 arr2)
  "Compute cosine similarity between two simple float arrays.

INPUTS:
  ARR1, ARR2 — each is a (simple-array single-float (*)).
               Must be the same length.

OUTPUT:
  A single-float in [-1.0, 1.0].
  Returns 0.0 if either array has zero magnitude.

PERFORMANCE:
  Single pass over both arrays computing dot product and magnitudes
  simultaneously. Declared with appropriate optimize qualities for SBCL.

PRECONDITIONS:
  - Both arrays must be (simple-array single-float (*)).
  - Both arrays must have the same length.
  - Caller is responsible for validation; this function does NOT check."
  (declare (optimize (speed 3) (safety 1) (debug 0))
           (type (simple-array single-float (*)) arr1 arr2))
  (let ((dot 0.0s0)
        (mag1-sq 0.0s0)
        (mag2-sq 0.0s0)
        (len (length arr1)))
    (declare (type single-float dot mag1-sq mag2-sq)
             (type fixnum len))
    (loop for i fixnum from 0 below len
          do (let ((a (aref arr1 i))
                   (b (aref arr2 i)))
               (declare (type single-float a b))
               (incf dot (* a b))
               (incf mag1-sq (* a a))
               (incf mag2-sq (* b b))))
    (let ((denom (* (sqrt mag1-sq) (sqrt mag2-sq))))
      (if (zerop denom)
          0.0s0
          (/ dot denom)))))

(defun compute-cosine-similarity (vec1 vec2)
  "Compute cosine similarity between two embedding vectors.

INPUTS:
  VEC1, VEC2 — each is a list of single-float values (embedding vectors).
               Must be the same length. NIL inputs return 0.0.

OUTPUT:
  A single-float in the range [-1.0, 1.0].
  Returns 0.0 if either vector is NIL, empty, or has zero magnitude.

NOTES:
  This is the convenience interface that accepts lists. For performance-critical
  inner loops, use COMPUTE-COSINE-SIMILARITY-ARRAYS with pre-coerced arrays.

COMPLEXITY: O(n) where n is the vector dimension."
  (when (or (null vec1) (null vec2))
    (return-from compute-cosine-similarity 0.0s0))
  (let ((len1 (length vec1))
        (len2 (length vec2)))
    (when (or (zerop len1) (zerop len2) (/= len1 len2))
      (return-from compute-cosine-similarity 0.0s0)))
  (let ((arr1 (coerce-to-float-array vec1))
        (arr2 (coerce-to-float-array vec2)))
    (if (and arr1 arr2)
        (compute-cosine-similarity-arrays arr1 arr2)
        0.0s0)))

;;; ============================================================
;;; Search Result Struct
;;; ============================================================

(defstruct (search-result
            (:constructor make-search-result
                (&key record semantic-score importance-score final-score rank)))
  "A single result from semantic search over durable memory.

FIELDS:
  RECORD           — the durable-memory-record that matched.
  SEMANTIC-SCORE   — cosine similarity between query and record embedding (0.0–1.0 typical).
  IMPORTANCE-SCORE — the record's importance component (0.0–1.0).
  FINAL-SCORE      — the hybrid score used for ranking.
  RANK             — 1-based rank in the result set."
  (record nil :type (or null claw-lisp.storage.durable-memory:durable-memory-record))
  (semantic-score 0.0s0 :type single-float)
  (importance-score 0.0s0 :type single-float)
  (final-score 0.0s0 :type single-float)
  (rank 0 :type (integer 0)))

;;; ============================================================
;;; Scoring Functions
;;; ============================================================

(defun compute-record-importance-component (record &key (staleness-penalty-p t))
  "Compute the importance component for RECORD used in hybrid scoring.

INPUTS:
  RECORD             — a durable-memory-record.
  STALENESS-PENALTY-P — when T (default), penalize importance by staleness:
                        result = importance-score × (1 − staleness-score).
                        When NIL, return raw importance-score.

OUTPUT:
  A single-float in [0.0, 1.0].

NOTES:
  This extracts and normalizes the 'lexical/importance' component from
  existing record metadata. The staleness penalty ensures that old,
  unaccessed records score lower even if their base importance is high."
  (let* ((importance (coerce
                      (or (claw-lisp.storage.durable-memory:durable-memory-record-importance-score record)
                          0.0)
                      'single-float))
         (staleness (coerce
                     (or (claw-lisp.storage.durable-memory:durable-memory-record-staleness-score record)
                         0.0)
                     'single-float)))
    (let ((raw (if staleness-penalty-p
                   (* importance (- 1.0s0 (max 0.0s0 (min 1.0s0 staleness))))
                   importance)))
      (max 0.0s0 (min 1.0s0 raw)))))

(defun compute-hybrid-score (semantic-score importance-score
                             &key (alpha 0.7))
  "Compute hybrid score blending semantic similarity with importance.

FORMULA:
  final-score = alpha × semantic-score + (1 − alpha) × importance-score

INPUTS:
  SEMANTIC-SCORE   — cosine similarity (single-float, typically 0.0–1.0).
  IMPORTANCE-SCORE — importance component (single-float, 0.0–1.0).
  ALPHA            — weight for semantic component (single-float, 0.0–1.0).
                     Default: 0.7 (semantic-dominant).

OUTPUT:
  A single-float representing the blended score.

NOTES:
  Alpha = 1.0 means pure semantic search.
  Alpha = 0.0 means pure importance-based ranking.
  Values outside [0.0, 1.0] are clamped."
  (let ((a (max 0.0s0 (min 1.0s0 (coerce alpha 'single-float))))
        (sem (coerce semantic-score 'single-float))
        (imp (coerce importance-score 'single-float)))
    (+ (* a sem) (* (- 1.0s0 a) imp))))

(defun resolve-hybrid-weight (kind config)
  "Resolve the hybrid weight (alpha) for KIND.

INPUTS:
  KIND   — a durable memory kind keyword, or NIL for global default.
  CONFIG — runtime-config.

OUTPUT:
  A single-float alpha value in [0.0, 1.0].

RESOLUTION ORDER:
  1. Per-kind override in *SEMANTIC-SEARCH-KIND-HYBRID-WEIGHTS*.
  2. Config slot: runtime-config-semantic-search-hybrid-weight.
  3. Hardcoded default: 0.7."
  (or (when kind
        (let ((entry (assoc kind *semantic-search-kind-hybrid-weights* :test #'eq)))
          (when entry
            (coerce (cdr entry) 'single-float))))
      (when config
        (handler-case
            (coerce (claw-lisp.config:runtime-config-semantic-search-hybrid-weight config)
                    'single-float)
          (error () nil)))
      *semantic-search-default-hybrid-weight*))

;;; ============================================================
;;; Query Embedding
;;; ============================================================

(defun embed-query-text (query-text config)
  "Embed QUERY-TEXT using the configured embedding provider.

INPUTS:
  QUERY-TEXT — string to embed (already validated as non-empty).
  CONFIG     — runtime-config.

OUTPUT:
  Embedding as a list of single-float, or NIL on failure.

NOTES:
  Calls compute-embeddings directly from the providers package.
  Logs errors but does not signal them."
  (handler-case
      (let* ((provider (claw-lisp.config:runtime-config-embedding-provider config))
             (model (claw-lisp.config:runtime-config-embedding-model config))
             (timeout (claw-lisp.config:runtime-config-embedding-timeout-seconds config))
             (embeddings (claw-lisp.providers:compute-embeddings
                          (list query-text)
                          :provider provider
                          :model model
                          :timeout-seconds timeout
                          :signal-errors-p nil)))
        (car embeddings))
    (error (e)
      (format *error-output*
              "~&[DURABLE-MEMORY-SEARCH] Failed to embed query text: ~A~%" e)
      nil)))

;;; ============================================================
;;; Candidate Gathering
;;; ============================================================

(defun %record-matches-filters-p (record
                                  &key min-created-universal-time
                                       max-created-universal-time
                                       subject-id
                                       tags-any)
  "Return T if RECORD passes all specified filters."
  (and
   ;; Time window: created-at >= min
   (or (null min-created-universal-time)
       (let ((created (claw-lisp.storage.durable-memory:durable-memory-record-created-universal-time record)))
         (and created (>= created min-created-universal-time))))
   ;; Time window: created-at <= max
   (or (null max-created-universal-time)
       (let ((created (claw-lisp.storage.durable-memory:durable-memory-record-created-universal-time record)))
         (and created (<= created max-created-universal-time))))
   ;; Subject ID match
   (or (null subject-id)
       (equal subject-id
              (claw-lisp.storage.durable-memory:durable-memory-record-subject-id record)))
   ;; Tags: at least one matching tag
   (or (null tags-any)
       (let ((record-tags (claw-lisp.storage.durable-memory:durable-memory-record-tags record)))
         (some (lambda (tag) (member tag record-tags :test #'equal))
               tags-any)))))

(defun gather-candidate-embeddings (&key kinds
                                         max-candidates
                                         min-created-universal-time
                                         max-created-universal-time
                                         subject-id
                                         tags-any
                                         config)
  "Gather candidate (record . embedding-array) pairs from the embedding index,
 filtered by the given criteria.

INPUTS:
  KINDS                      — list of kind keywords to search (default: all indexed kinds).
  MAX-CANDIDATES             — maximum number of candidates to return (default: 500).
  MIN-CREATED-UNIVERSAL-TIME — only records created at or after this time.
  MAX-CREATED-UNIVERSAL-TIME — only records created at or before this time.
  SUBJECT-ID                 — if non-NIL, only records with this subject-id.
  TAGS-ANY                   — if non-NIL, only records with at least one matching tag.
  CONFIG                     — runtime config (default: *runtime-config*).

OUTPUT:
  A list of (RECORD . EMBEDDING-ARRAY) conses, where:
    RECORD is a durable-memory-record.
    EMBEDDING-ARRAY is a (simple-array single-float (*)).

NOTES:
  - Records without embeddings in the index are skipped.
  - The MAX-CANDIDATES cap is applied after all other filters.
  - PERFORMANCE: Records are loaded from disk for each kind on first access within
    a search call (cached for subsequent candidates of the same kind). This ensures
    that importance/staleness scores are available for hybrid scoring. For stores
    with many records, this disk I/O may be a bottleneck; a future optimization
    is to store lightweight metadata in the embedding index."
  (let* ((effective-config (%effective-config config))
         (effective-kinds (or kinds *default-search-kinds*))
         (effective-max (or max-candidates
                            (handler-case
                                (claw-lisp.config:runtime-config-semantic-search-max-candidates
                                 effective-config)
                              (error () 500))))
         (needs-record-filter-p (or min-created-universal-time
                                    max-created-universal-time
                                    subject-id
                                    tags-any))
         ;; Cache: (kind . subject-id-or-nil) → list of records
         ;; Avoids redundant loads within a single search call.
         (record-cache (make-hash-table :test #'equal))
         (candidates '())
         (count 0))
    (dolist (kind effective-kinds)
      (when (>= count effective-max)
        (return))
      (let ((alist (gethash kind claw-lisp.storage.durable-memory:*durable-memory-embedding-index*)))
        (dolist (entry alist)
          (when (>= count effective-max)
            (return))
          (let* ((record-id (car entry))
                 (embedding-list (cdr entry)))
            (when (and record-id embedding-list)
              ;; Coerce embedding to array
              (let ((emb-array (coerce-to-float-array embedding-list)))
                (when emb-array
                  ;; Always load and cache records for this kind (needed for scoring).
                  ;; Cache stores a hash table mapping record-id → record for O(1) lookup.
                  (let* ((cache-key kind)
                         (cached (gethash cache-key record-cache :not-found)))
                    (when (eq cached :not-found)
                      (let ((all-records '())
                            (id-table (make-hash-table :test #'equal)))
                        (handler-case
                            (let ((subject-ids
                                    (claw-lisp.storage.durable-memory-embeddings:discover-subject-ids kind)))
                              (dolist (sid subject-ids)
                                (let ((records (claw-lisp.storage.durable-memory:load-durable-memories kind sid)))
                                  (dolist (r records)
                                    (push r all-records)))))
                          (error (e)
                            (format *error-output*
                                    "~&[DURABLE-MEMORY-SEARCH] Error loading records for kind ~A: ~A~%" kind e)))
                        ;; Build id→record hash table
                        (dolist (r all-records)
                          (setf (gethash (claw-lisp.storage.durable-memory:durable-memory-record-id r) id-table) r))
                        (setf (gethash cache-key record-cache) id-table)
                        (setf cached id-table)))
                    ;; Look up record by ID
                    (let ((record (gethash record-id cached)))
                      (when record
                        (when (or (not needs-record-filter-p)
                                  (%record-matches-filters-p record
                                                              :min-created-universal-time min-created-universal-time
                                                              :max-created-universal-time max-created-universal-time
                                                              :subject-id subject-id
                                                              :tags-any tags-any))
                          (push (cons record emb-array) candidates)
                          (incf count))))))))))))
    (nreverse candidates)))

;;; ============================================================
;;; Internal Scoring Loop
;;; ============================================================

(defun score-candidates (candidates query-array
                          &key hybrid-weight staleness-penalty-p config)
  "Score a list of (RECORD . EMBEDDING-ARRAY) candidates against QUERY-ARRAY.

INPUTS:
  CANDIDATES      — list of (record . embedding-array) conses.
  QUERY-ARRAY     — (simple-array single-float (*)) for the query.
  HYBRID-WEIGHT   — alpha for hybrid scoring (may be overridden per-kind).
  STALENESS-PENALTY-P — whether to apply staleness penalty.
  CONFIG          — runtime-config.

OUTPUT:
  List of SEARCH-RESULT structs (unsorted, unranked).

NOTES:
  - Candidates with embedding dimensions mismatching the query are skipped
    and a warning is logged to *ERROR-OUTPUT*.
  - Semantic scores are clamped to [0.0, 1.0] (negative similarities treated as 0)."
  (let ((results '())
        (query-len (length query-array)))
    (dolist (pair candidates)
      (let* ((record (car pair))
             (emb-array (cdr pair))
             (emb-len (length emb-array)))
        ;; Only compute similarity if dimensions match
        (if (and (> query-len 0) (= query-len emb-len))
            (let* ((semantic (compute-cosine-similarity-arrays query-array emb-array))
                   ;; Clamp semantic to [0, 1] for scoring purposes
                   ;; (cosine similarity can be negative, but we treat negative as 0 for ranking)
                   (semantic-clamped (max 0.0s0 semantic))
                   (importance (compute-record-importance-component
                                record
                                :staleness-penalty-p staleness-penalty-p))
                   ;; Resolve per-kind alpha if no explicit override
                   (kind (claw-lisp.storage.durable-memory:durable-memory-record-kind record))
                   (alpha (or hybrid-weight
                              (resolve-hybrid-weight kind config)))
                   (final (compute-hybrid-score semantic-clamped importance
                                                :alpha alpha)))
              (push (make-search-result
                     :record record
                     :semantic-score semantic-clamped
                     :importance-score importance
                     :final-score final
                     :rank 0)
                    results))
            (when (/= query-len emb-len)
              (format *error-output*
                      "~&[DURABLE-MEMORY-SEARCH] Dimension mismatch: query=~D record=~D (record-id=~A, kind=~A)~%"
                      query-len emb-len
                      (claw-lisp.storage.durable-memory:durable-memory-record-id record)
                      (claw-lisp.storage.durable-memory:durable-memory-record-kind record))))))
    (nreverse results)))

;;; ============================================================
;;; Main Search API
;;; ============================================================

(defun semantic-search-durable-memory (query-text
                                       &key
                                       kinds
                                       limit
                                       min-score
                                       hybrid-weight
                                       (staleness-penalty-p t)
                                       min-created-universal-time
                                       max-created-universal-time
                                       subject-id
                                       tags-any
                                       max-candidates
                                       config)
  "Perform semantic similarity search over durable memory.

INPUTS:
  QUERY-TEXT                  — the natural-language query string (required).
  KINDS                      — list of durable memory kinds to search
                               (default: all embedding-enabled kinds).
  LIMIT                      — maximum number of results to return (default: 10,
                               or from config).
  MIN-SCORE                  — minimum final (hybrid) score threshold (default: 0.0).
                               Results below this score are excluded.
  HYBRID-WEIGHT              — alpha for hybrid scoring (default: from config).
                               When NIL, uses per-kind weights if configured,
                               else global default.
  STALENESS-PENALTY-P        — when T, penalize importance by staleness (default: T).
  MIN-CREATED-UNIVERSAL-TIME — only search records created at or after this time.
  MAX-CREATED-UNIVERSAL-TIME — only search records created at or before this time.
  SUBJECT-ID                 — restrict search to this subject-id.
  TAGS-ANY                   — restrict to records with at least one matching tag.
  MAX-CANDIDATES             — max candidates to scan (default: from config).
  CONFIG                     — runtime config (default: *runtime-config*).

OUTPUT:
  A list of SEARCH-RESULT structs, sorted by FINAL-SCORE descending,
  with RANK fields set (1-based).

  Returns NIL if:
    - QUERY-TEXT is NIL or empty.
    - Embeddings are disabled.
    - No candidates found.
    - Embedding generation for query fails.

ERRORS:
  - Does NOT signal errors for embedding failures; logs and returns NIL.
  - Does NOT signal errors for empty results.

SIDE EFFECTS:
  - Calls the embedding provider to embed QUERY-TEXT (network I/O).
  - May load durable memory records from disk (file I/O).

QUERY FLOW:
  1. Validate inputs; return NIL if query is empty or embeddings disabled.
  2. Compute embedding for QUERY-TEXT via the embedding pipeline.
  3. Gather candidate records with embeddings (filtered by kind, time, etc.).
  4. For each candidate, compute cosine similarity with query embedding.
  5. Compute importance component for each candidate.
  6. Compute hybrid score: alpha × semantic + (1-alpha) × importance.
  7. Filter by MIN-SCORE.
  8. Sort by final score descending.
  9. Truncate to LIMIT.
  10. Assign ranks and return."
  (let ((effective-config (%effective-config config)))
    ;; Step 1: Validate query text
    (let ((trimmed (when query-text
                     (string-trim '(#\Space #\Tab #\Newline #\Return) query-text))))
      (when (or (null trimmed) (zerop (length trimmed)))
        (return-from semantic-search-durable-memory nil))

      ;; Check that embeddings are enabled
      (unless (handler-case
                  (claw-lisp.config:runtime-config-embedding-enabled-p effective-config)
                (error () nil))
        (return-from semantic-search-durable-memory nil))

       ;; Check that semantic search is enabled
       (unless (handler-case
                   (claw-lisp.config:runtime-config-semantic-search-enabled-p effective-config)
                 (error () nil))  ; default to disabled if slot missing
         (return-from semantic-search-durable-memory nil))

      ;; Step 2: Embed query
      (let ((query-embedding (embed-query-text trimmed effective-config)))
        (when (null query-embedding)
          (return-from semantic-search-durable-memory nil))

        (let ((query-array (coerce-to-float-array query-embedding)))
          (when (null query-array)
            (return-from semantic-search-durable-memory nil))

          ;; Step 3: Gather candidates
          (let ((candidates (gather-candidate-embeddings
                             :kinds (or kinds *default-search-kinds*)
                             :max-candidates max-candidates
                             :min-created-universal-time min-created-universal-time
                             :max-created-universal-time max-created-universal-time
                             :subject-id subject-id
                             :tags-any tags-any
                             :config effective-config)))
            (when (null candidates)
              (return-from semantic-search-durable-memory nil))

            ;; Steps 4-6: Score each candidate
            (let* ((scored (score-candidates candidates query-array
                                             :hybrid-weight hybrid-weight
                                             :staleness-penalty-p staleness-penalty-p
                                             :config effective-config))
                   ;; Step 7: Filter by min-score
                   (effective-min-score (or min-score 0.0s0))
                   (filtered (remove-if (lambda (sr)
                                          (< (search-result-final-score sr)
                                             effective-min-score))
                                        scored))
                   ;; Step 8: Sort by final score descending
                   (sorted (sort filtered #'> :key #'search-result-final-score))
                   ;; Step 9: Truncate to limit
                   (effective-limit (or limit
                                       (handler-case
                                           (claw-lisp.config:runtime-config-semantic-search-default-limit
                                            effective-config)
                                         (error () 10))))
                   (limited (if (<= (length sorted) effective-limit)
                                sorted
                                (subseq sorted 0 effective-limit))))
              ;; Step 10: Assign ranks
              (loop for result in limited
                    for rank from 1
                    do (setf (search-result-rank result) rank))
              limited)))))))

 ;;; ============================================================
 ;;; Public API Exports
 ;;; ============================================================

 ;; Exports are defined in packages.lisp; no additional export needed.
 ;; Previously had a redundant (export ...) form here.
 

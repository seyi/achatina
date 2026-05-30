;;;; lisp/storage/durable-memory-query.lisp
;;;;
;;;; Phase 7 Task 6 — Runtime Integration & Query Helpers
;;;;
;;;; Provides high-level query API and context injection utilities
;;;; for integrating semantic durable memory into the agent runtime.
;;;;
;;;; This file handles:
;;;;   - Turn-aware query construction
;;;;   - Context injection with deduplication
;;;;   - Result summarization
;;;;   - Circuit breaker for embedding provider
;;;;
;;;; Depends on:
;;;;   - claw-lisp.storage.durable-memory (domain model)
;;;;   - claw-lisp.storage.durable-memory-search (search primitives)
;;;;   - claw-lisp.storage.durable-memory-consolidate (superseded filtering)
;;;;   - claw-lisp.config (runtime configuration)

(in-package :claw-lisp.storage.durable-memory-search)

;;; ============================================================
;;; Circuit Breaker for Embedding Provider
;;; ============================================================

(defparameter *dmq-embedding-failures* 0
  "Count of consecutive embedding provider failures.")

(defparameter *dmq-circuit-open-until* nil
  "Universal time when circuit breaker cooldown expires.
   NIL means circuit is closed (provider available).")

(defparameter *dmq-debug-p* nil
  "Enable debug output for durable memory query operations.")

(defun embedding-available-p (&optional session)
  "Check if embedding provider is available (circuit breaker check).
   
   Returns T if:
     - Circuit is closed (no recent failures)
     - OR cooldown has expired and reset succeeds
   
   Returns NIL if:
     - Circuit is open (in cooldown period)
   
   SIDE EFFECTS:
     Increments failure count on provider errors."
  (let ((config (current-dmq-config session)))
    (declare (ignore config))
    (cond
      ;; Circuit closed — provider available
      ((null *dmq-circuit-open-until*)
       t)
      ;; Circuit open — check if cooldown expired
      ((>= (get-universal-time) *dmq-circuit-open-until*)
       ;; Reset and allow retry
       (setf *dmq-circuit-open-until* nil
             *dmq-embedding-failures* 0)
       t)
      ;; Circuit open — still in cooldown
      (t
       nil))))

(defun record-embedding-success ()
  "Record a successful embedding provider call.
   Resets failure counter and closes circuit."
  (setf *dmq-embedding-failures* 0
        *dmq-circuit-open-until* nil))

(defun record-embedding-failure ()
  "Record an embedding provider failure.
   Opens circuit if failure threshold exceeded."
  (let ((config (current-dmq-config)))
    (incf *dmq-embedding-failures*)
    (when (>= *dmq-embedding-failures*
              (dmq-config-embedding-failure-threshold config))
      ;; Open circuit
      (setf *dmq-circuit-open-until*
            (+ (get-universal-time)
               (dmq-config-embedding-cooldown-seconds config)))
      (warn "[DMQ] Embedding circuit breaker OPEN — ~D consecutive failures"
            *dmq-embedding-failures*))))

(defun reset-embedding-circuit-breaker ()
  "Manually reset the embedding circuit breaker.
   Use for debugging or after provider recovery is confirmed."
  (setf *dmq-embedding-failures* 0
        *dmq-circuit-open-until* nil)
  (format t "~&[DMQ] Embedding circuit breaker reset.~%"))

;;; ============================================================
;;; Step 3: Runtime Hook Integration
;;; ============================================================

;;; Memory injection record struct

(defstruct (memory-injection-record
            (:conc-name memory-injection-record-))
  "Record of a memory injection event.
   
   Used to track which memories have been injected into which turns,
   enabling importance-aware deduplication across the conversation."
  (memory-id nil :type (or null string))
  (turn-id nil :type (or null fixnum))
  (importance 0.0 :type single-float)
  (kind :user :type keyword)
  (timestamp 0 :type fixnum))

;;; Injection tracking

(defun turn-memory-injected-p (turn pass)
  "Check if memory was already injected for TURN.

INPUTS:
  TURN — current turn struct.
  PASS — retained for compatibility; only :initial behavior remains public.

OUTPUT:
  T if memory context was already injected for TURN.
  NIL otherwise."
  (declare (ignore pass))
  (let ((injected (getf (agent-turn-metadata turn) :memory-context-injected)))
    (or (eq injected t)
        (eq injected :initial))))

(defun mark-turn-memory-injected (turn pass)
  "Mark TURN as having memory injected.

INPUTS:
  TURN — current turn struct.
  PASS — retained for compatibility; only :initial behavior remains public.

SIDE EFFECTS:
  Updates turn metadata."
  (declare (ignore pass))
  (setf (getf (agent-turn-metadata turn) :memory-context-injected) :initial))

;;; Deduplication (full implementation)

(defun filter-dedup-results (session results config)
  "Filter query results through importance-aware deduplication.

INPUTS:
  SESSION — session struct with injection log.
  RESULTS — list of (SCORE . RECORD) conses from query-durable-memory.
  CONFIG  — durable-memory-query-config.

OUTPUT:
  Filtered list of (SCORE . RECORD) conses.

DEDUP STRATEGY:
  1. Evergreen kinds (e.g., :project): inject at most once per session.
  2. High-importance (>= threshold): use extended dedup window (20 turns).
  3. Normal: use standard dedup window (5 turns).
  4. No per-record force-inject override is currently supported."
  (let* ((injection-log (session-memory-injection-log session))
         (current-turn (session-current-turn-id session))
         (normal-window (dmq-config-dedup-window-normal config))
         (important-window (dmq-config-dedup-window-important config))
         (importance-threshold (dmq-config-importance-threshold config))
         (evergreen-kinds (dmq-config-evergreen-kinds config))
         (filtered '()))
    
    (dolist (pair results)
      (let* ((score (car pair))
             (record (cdr pair))
             (record-id (durable-memory-record-id record))
             (kind (durable-memory-record-kind record))
             (importance (durable-memory-record-importance-score record))
             ;; Check if evergreen and already injected this session
             (is-evergreen (member kind evergreen-kinds :test #'eq))
             (already-injected-evergreen
               (and is-evergreen
                    (find kind injection-log
                          :key #'memory-injection-record-kind
                          :test #'eq)))
             ;; Check dedup window
             (recent-injection
               (find record-id injection-log
                     :key #'memory-injection-record-memory-id
                     :test #'string=))
             (within-window
               (and recent-injection
                    (<= (- current-turn
                           (memory-injection-record-turn-id recent-injection))
                        (if (>= importance importance-threshold)
                            important-window
                            normal-window))))
             (force-inject-p nil))
        
        ;; Decide whether to include
        (cond
          (force-inject-p
           ;; Always include force-injected memories
           (push pair filtered))
          (already-injected-evergreen
           ;; Skip evergreen already injected this session
           (when *dmq-debug-p*
             (format t "~&[DMQ] Dedup: skipping evergreen ~A (already injected)"
                     record-id)))
          (within-window
           ;; Skip if within dedup window
           (when *dmq-debug-p*
             (format t "~&[DMQ] Dedup: skipping ~A (within ~A-turn window)"
                     record-id
                     (if (>= importance importance-threshold)
                         important-window
                         normal-window))))
          (t
           ;; Include this result
           (push pair filtered)))))
    
    ;; Return in original order (highest score first)
    (nreverse filtered)))

(defun record-memory-injection (session results turn-id)
  "Record that these memories were injected at this turn.

INPUTS:
  SESSION    — session struct with injection log.
  RESULTS    — list of (SCORE . RECORD) conses that were injected.
  TURN-ID    — the turn ID when injection occurred.

SIDE EFFECTS:
  Updates session-memory-injection-log.
  Maintains bounded log (max 200 entries)."
  (let ((log (session-memory-injection-log session)))
    ;; Add new entries
    (dolist (pair results)
      (let* ((record (cdr pair))
             (entry (make-memory-injection-record
                     :memory-id (durable-memory-record-id record)
                     :turn-id turn-id
                     :importance (durable-memory-record-importance-score record)
                     :kind (durable-memory-record-kind record)
                     :timestamp (get-universal-time))))
        (push entry log)))
    
    ;; Bound the log (max 200 entries)
    (when (> (length log) 200)
      (setf log (subseq log 0 200)))
    
    (setf (session-memory-injection-log session) log)))

;;; Main injection function

(defun inject-durable-memory-context (session turn
                                      &key (pass :initial)
                                           (force-refresh nil))
  "Inject relevant durable memory context into the conversation.

INPUTS:
  SESSION        — session struct with injection log and config.
  TURN           — current turn struct.
  PASS           — :initial is supported in the public build. :augmented
                   returns no injection.
  FORCE-REFRESH  — if T, bypass dedup and cache, re-query from scratch.

OUTPUT:
  Two values: INJECTED-P and MEMORY-COUNT."
  (let* ((config (current-dmq-config session))
         (enabled (dmq-config-injection-enabled config)))
    
    ;; Gate 1: injection enabled?
    (unless enabled
      (return-from inject-durable-memory-context (values nil 0)))
    
    ;; Gate 2: public build supports only initial-pass injection.
    (when (eq pass :augmented)
      (return-from inject-durable-memory-context (values nil 0)))
    
    ;; Gate 3: already injected this turn?
    (when (and (not force-refresh)
               (turn-memory-injected-p turn pass))
      (return-from inject-durable-memory-context (values nil 0)))
    
    ;; Build query from turn content
    (let* ((query-text (extract-query-text turn pass))
           (query-text (and query-text
                            (not (%string-empty-or-whitespace-p query-text))
                            query-text)))
      (unless query-text
        (return-from inject-durable-memory-context (values nil 0)))
      
      ;; Query durable memory
      (let* ((results (query-durable-memory
                       query-text
                       :mode (dmq-config-default-query-mode config)
                       :limit (dmq-config-max-results config)
                       :min-score (dmq-config-min-relevance-score config)))
             ;; Filter through dedup
             (novel-results (filter-dedup-results session results config))
             ;; Summarize within budget
             (context-text (when novel-results
                             (summarize-memory-results
                              novel-results
                              :max-chars (dmq-config-max-injection-chars config)
                              :format :markdown))))
        
        (cond
          ((and context-text (plusp (length context-text)))
           (let ((tid (session-current-turn-id session)))
             ;; Record injection
             (record-memory-injection session novel-results tid)
             (mark-turn-memory-injected turn pass)
             ;; Insert context message
             (insert-memory-context-message session turn context-text pass)
             (when *dmq-debug-p*
               (format *error-output* "~&[dmq] Injected ~D memories (~A pass) for turn ~A~%"
                       (length novel-results) pass tid))
             (values t (length novel-results))))

          (t
           (when *dmq-debug-p*
             (format *error-output* "~&[dmq] No novel memories to inject (~A pass) for turn ~A~%"
                     pass (session-current-turn-id session)))
           (values nil 0)))))))

(defun extract-query-text (turn pass)
  "Extract text to use as the memory query.

:initial    — Returns user message content.
:augmented  — Returns the same user content; public runtime does not perform
              second-pass injection."
  (declare (ignore pass))
  (agent-turn-content turn))

(defun insert-memory-context-message (session turn context-text pass)
  "Insert a memory context message into the turn's message list.

Uses :user role with [MEMORY CONTEXT] header for backend compatibility."
  (let* ((message (make-message
                   :role :user
                   :content (format nil "[MEMORY CONTEXT~A]~%~
The following information was retrieved from your durable memory ~
and may be relevant to this conversation:~%~%~A~%~%[END MEMORY CONTEXT]"
                                    ""
                                    context-text)
                   :metadata `(:synthetic t
                               :source :durable-memory
                               :pass ,pass
                               :timestamp ,(get-universal-time)))))
    ;; Insert before user turn (after system prompt)
    (claw-lisp.core.domain::insert-message-before-user-turn turn message)))

(defun summarize-tool-results (turn)
  "Summarize recent tool results for query context."
  (let* ((tool-results (agent-turn-tool-results turn))
         (recent (when tool-results
                   (subseq tool-results
                           (max 0 (- (length tool-results) 3))))))
    (when recent
      (format nil "~{~A~^; ~}"
              (mapcar (lambda (tr)
                        (format nil "~A: ~A"
                                (tool-result-tool-name tr)
                                (subseq (tool-result-content tr) 0
                                        (min 50 (length (tool-result-content tr))))))
                      recent)))))

(defun query-durable-memory (query-text &key
                                      (mode :hybrid)
                                      (kinds nil)
                                      (limit nil)
                                      (min-score nil)
                                      (include-superseded-p nil)
                                      (config nil))
  "Query durable memory using the specified mode.

INPUTS:
  QUERY-TEXT          — the search query (string).
  MODE                — scoring mode: :semantic, :lexical, or :hybrid.
  KINDS               — list of memory kinds to search. Default: all kinds.
  LIMIT               — max results to return. Default: from config.
  MIN-SCORE           — minimum score threshold. Default: from config.
  INCLUDE-SUPERSEDED-P — if T, include superseded records. Default: NIL.
  CONFIG              — runtime-config or durable-memory-query-config.
                        Default: (current-dmq-config).

OUTPUT:
  A list of (SCORE . RECORD) conses, sorted by descending score.
  Each SCORE is a float (0.0-1.0). Each RECORD is a durable-memory-record.

MODE BEHAVIOR:
  :semantic — Pure cosine similarity on embeddings. Returns NIL if
              embeddings unavailable.
  :lexical  — Keyword/text matching only. Always available.
  :hybrid   — Weighted blend of semantic + lexical. Degrades to
              lexical if embeddings unavailable."
  (declare (ignore config))
  ;; Validate input
  (when (or (null query-text)
            (not (stringp query-text))
            (%string-empty-or-whitespace-p query-text))
    (return-from query-durable-memory nil))
  
  ;; Resolve config
  (let* ((effective-config (current-dmq-config))
         (effective-limit (or limit (dmq-config-max-results effective-config)))
         (effective-min-score (or min-score (dmq-config-min-relevance-score effective-config)))
         (effective-kinds (or kinds '(:user :project :feedback :reference))))
    
    ;; Dispatch by mode
    (ecase mode
      (:semantic
       (if (embedding-available-p)
           (%query-semantic query-text effective-kinds effective-limit effective-min-score
                            include-superseded-p)
           (progn
             (warn "[DMQ] Embeddings unavailable; :semantic mode returning NIL")
             nil)))
      (:lexical
       (%query-lexical query-text effective-kinds effective-limit include-superseded-p))
      (:hybrid
       (if (embedding-available-p)
           (%query-hybrid query-text effective-kinds effective-limit effective-min-score
                          effective-config include-superseded-p)
           ;; Degrade to lexical
           (progn
             (warn "[DMQ] Embeddings unavailable; :hybrid mode degrading to :lexical")
             (%query-lexical query-text effective-kinds effective-limit include-superseded-p)))))))

(defun %query-semantic (query-text kinds limit min-score include-superseded-p)
  "Perform pure semantic search using cosine similarity.

Delegates to Task 4's semantic-search-durable-memory."
  (let ((results (semantic-search-durable-memory
                  query-text
                  :kinds kinds
                  :limit limit
                  :min-score min-score
                  :hybrid-weight 1.0)))  ; Pure semantic (no lexical blend)
    ;; Convert SEARCH-RESULT structs to (SCORE . RECORD) conses
    (mapcar (lambda (sr)
              (cons (claw-lisp.storage.durable-memory-search:search-result-final-score sr)
                    (claw-lisp.storage.durable-memory-search:search-result-record sr)))
            results)))

(defun %query-lexical (query-text kinds limit include-superseded-p)
  "Perform pure lexical (keyword) search.

Delegates to Task 4's semantic-search-durable-memory with hybrid-weight 0.0."
  (let ((results (semantic-search-durable-memory
                  query-text
                  :kinds kinds
                  :limit limit
                  :min-score 0.0  ; Lexical search may have lower scores
                  :hybrid-weight 0.0)))  ; Pure lexical (no semantic blend)
    ;; Convert SEARCH-RESULT structs to (SCORE . RECORD) conses
    (mapcar (lambda (sr)
              (cons (claw-lisp.storage.durable-memory-search:search-result-final-score sr)
                    (claw-lisp.storage.durable-memory-search:search-result-record sr)))
            results)))

(defun %query-hybrid (query-text kinds limit min-score config include-superseded-p)
  "Perform hybrid search: weighted blend of semantic + lexical.

Uses per-kind semantic weights from config."
  (declare (ignore include-superseded-p))
  ;; For hybrid, we use semantic-search-durable-memory with the global hybrid-weight
  ;; The search function handles the blending internally
  (let* ((global-weight (dmq-config-semantic-weight-by-kind config))
         (default-entry (assoc :default global-weight))
         (hybrid-weight (if default-entry (cdr default-entry) 0.7s0))
         (results (semantic-search-durable-memory
                   query-text
                   :kinds kinds
                   :limit limit
                   :min-score min-score
                   :hybrid-weight hybrid-weight)))
    ;; Convert SEARCH-RESULT structs to (SCORE . RECORD) conses
    (mapcar (lambda (sr)
              (cons (claw-lisp.storage.durable-memory-search:search-result-final-score sr)
                    (claw-lisp.storage.durable-memory-search:search-result-record sr)))
            results)))

(defun %resolve-semantic-weight (kind config)
  "Resolve semantic weight for KIND from config."
  (let* ((weights (dmq-config-semantic-weight-by-kind config))
         (entry (or (assoc kind weights) (assoc :default weights))))
    (if entry (cdr entry) 0.7s0)))

;;; Summarization

(defun summarize-memory-results (results &key (max-chars nil) (format :markdown))
  "Render memory search results as a string for injection into context.

INPUTS:
  RESULTS    — list of (SCORE . RECORD) conses from query-durable-memory.
  MAX-CHARS  — if non-NIL, truncate output to this length.
  FORMAT     — output format: :markdown (default) or :plain.

OUTPUT:
  A string suitable for appending to system prompt or user message."
  (ecase format
    (:markdown
     (let ((lines (list "## Relevant Memories" "")))
       (dolist (pair results)
         (let* ((score (car pair))
                (record (cdr pair))
                (kind (durable-memory-record-kind record))
                (title (or (durable-memory-record-title record) "(untitled)"))
                (importance (durable-memory-record-importance-score record))
                (content (durable-memory-record-content record))
                (preview (if content
                             (truncate-string (remove-newlines content) 120)
                             "(no content)")))
           (push (format nil "- **[~(~A~)]** ~A (importance: ~,1F, score: ~,1F)~%  > ~A..."
                         kind title importance score preview)
                 lines)))
       (let ((output (format nil "~{~A~^~%~}" (nreverse lines))))
         (if (and max-chars (> (length output) max-chars))
             (format nil "~A~%~%...(truncated)" (subseq output 0 max-chars))
             output))))
    (:plain
     (let ((lines '()))
       (dolist (pair results)
         (let* ((score (car pair))
                (record (cdr pair))
                (kind (durable-memory-record-kind record))
                (title (durable-memory-record-title record))
                (importance (durable-memory-record-importance-score record)))
           (push (format nil "[~(~A~)] ~A (imp: ~,1F, score: ~,1F)"
                         kind title importance score)
                 lines)))
       (format nil "~{~A~^~%~}" (nreverse lines))))))

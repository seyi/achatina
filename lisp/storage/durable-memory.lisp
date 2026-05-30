;;;; lisp/storage/durable-memory.lisp
;;;;
;;;; Durable Memory Domain Model & Types
;;;;
;;;; Defines the core domain structures for durable memory.
;;;; Follows patterns from domain.lisp and session-memory.lisp.

(in-package #:claw-lisp.storage.durable-memory)

;;; ============================================================
;;; Durable Memory Kinds
;;; ============================================================

(defparameter *durable-memory-kinds*
  '(:user :feedback :project :reference)
  "List of valid durable memory kinds.")

(defun durable-memory-kind-p (kind)
  "Return T if KIND is a valid durable memory kind."
  (member kind *durable-memory-kinds* :test #'eq))

;;; ============================================================
;;; Durable Memory Record Struct
;;; ============================================================

(defstruct (durable-memory-record
            (:constructor make-durable-memory-record
                (&key
                 id
                 kind
                 subject-id
                 title
                 content
                 source
                 created-universal-time
                 updated-universal-time
                 importance-score
                 staleness-score
                 last-accessed-universal-time
                 tags
                 version
                 supersedes-id
                 superseded-by-id
                 embedding
                 embedding-model
                 embedding-version)))
  "A record representing a durable memory entry.

Fields:
  - id: Unique identifier (string/uuid)
  - kind: Memory kind (:user, :feedback, :project, :reference)
  - subject-id: User/project/etc. identifier (string)
  - title: Short label for memory
  - content: String or list of content blocks
  - source: Provenance (:conversation, :tool, :manual)
  - created-universal-time: Creation timestamp (integer)
  - updated-universal-time: Last update timestamp (integer)
  - importance-score: Importance (0.0–1.0)
  - staleness-score: Staleness (0.0–1.0)
  - last-accessed-universal-time: Last accessed timestamp (integer)
  - tags: List of keywords/strings
  - version: Version integer
  - supersedes-id: ID of previous record (string/uuid)
  - superseded-by-id: ID of record that superseded this one (string/uuid) or NIL
  - embedding: Embedding vector (list of single-float) or NIL
  - embedding-model: Model name used for embedding (string) or NIL
  - embedding-version: Embedding version/checksum (string) or NIL"
  (id nil :type (or null string))
  (kind :user :type keyword)
  (subject-id "" :type string)
  (title "" :type string)
  (content nil :type (or null string))
  (source :conversation :type keyword)
  (created-universal-time nil :type (or null integer))
  (updated-universal-time nil :type (or null integer))
  (importance-score 0.0 :type float)
  (staleness-score 0.0 :type float)
  (last-accessed-universal-time nil :type (or null integer))
  (tags nil :type list)
  (version 1 :type integer)
  (supersedes-id nil :type (or null string))
  (superseded-by-id nil :type (or null string))
  ;; Phase 7: Embedding fields
  (embedding nil :type (or null list))
  (embedding-model nil :type (or null string))
  (embedding-version nil :type (or null string)))

;;; ============================================================
;;; Helper Constructors
;;; ============================================================

(defun make-user-memory (&key id subject-id title content source
                             created-universal-time updated-universal-time
                             importance-score staleness-score
                             last-accessed-universal-time tags
                             version supersedes-id superseded-by-id)
  "Construct a durable memory record of kind :user."
  (make-durable-memory-record
   :id id
   :kind :user
   :subject-id subject-id
   :title title
   :content content
   :source (or source :conversation)
   :created-universal-time created-universal-time
   :updated-universal-time updated-universal-time
   :importance-score (or importance-score 0.5)
   :staleness-score (or staleness-score 0.0)
   :last-accessed-universal-time last-accessed-universal-time
   :tags tags
   :version (or version 1)
   :supersedes-id supersedes-id
   :superseded-by-id superseded-by-id))

(defun make-feedback-memory (&key id subject-id title content source
                                 created-universal-time updated-universal-time
                                 importance-score staleness-score
                                 last-accessed-universal-time tags
                                 version supersedes-id superseded-by-id)
  "Construct a durable memory record of kind :feedback."
  (make-durable-memory-record
   :id id
   :kind :feedback
   :subject-id subject-id
   :title title
   :content content
   :source (or source :conversation)
   :created-universal-time created-universal-time
   :updated-universal-time updated-universal-time
   :importance-score (or importance-score 0.5)
   :staleness-score (or staleness-score 0.0)
   :last-accessed-universal-time last-accessed-universal-time
   :tags tags
   :version (or version 1)
   :supersedes-id supersedes-id
   :superseded-by-id superseded-by-id))

(defun make-project-memory (&key id subject-id title content source
                                created-universal-time updated-universal-time
                                importance-score staleness-score
                                last-accessed-universal-time tags
                                version supersedes-id superseded-by-id)
  "Construct a durable memory record of kind :project."
  (make-durable-memory-record
   :id id
   :kind :project
   :subject-id subject-id
   :title title
   :content content
   :source (or source :manual)
   :created-universal-time created-universal-time
   :updated-universal-time updated-universal-time
   :importance-score (or importance-score 0.5)
   :staleness-score (or staleness-score 0.0)
   :last-accessed-universal-time last-accessed-universal-time
   :tags tags
   :version (or version 1)
   :supersedes-id supersedes-id
   :superseded-by-id superseded-by-id))

(defun make-reference-memory (&key id subject-id title content source
                                  created-universal-time updated-universal-time
                                  importance-score staleness-score
                                  last-accessed-universal-time tags
                                  version supersedes-id superseded-by-id)
  "Construct a durable memory record of kind :reference."
  (make-durable-memory-record
   :id id
   :kind :reference
   :subject-id subject-id
   :title title
   :content content
   :source (or source :manual)
   :created-universal-time created-universal-time
   :updated-universal-time updated-universal-time
   :importance-score (or importance-score 0.5)
   :staleness-score (or staleness-score 0.0)
   :last-accessed-universal-time last-accessed-universal-time
   :tags tags
   :version (or version 1)
   :supersedes-id supersedes-id
   :superseded-by-id superseded-by-id))

;;; ============================================================
;;; Validation Utilities
;;; ============================================================

(defun validate-durable-memory-record (record)
  "Validate that RECORD is a durable-memory-record with a valid kind.
   Returns T if valid, NIL otherwise."
  (and (typep record 'durable-memory-record)
       (durable-memory-kind-p (durable-memory-record-kind record))))

;;; ============================================================
;;; Serialization / Deserialization
;;; ============================================================

(defun durable-memory-record-to-plist (record)
  "Serialize a durable-memory-record to a plist for storage."
  (list
   :id (durable-memory-record-id record)
   :kind (durable-memory-record-kind record)
   :subject-id (durable-memory-record-subject-id record)
   :title (durable-memory-record-title record)
   :content (durable-memory-record-content record)
   :source (durable-memory-record-source record)
   :created-universal-time (durable-memory-record-created-universal-time record)
   :updated-universal-time (durable-memory-record-updated-universal-time record)
   :importance-score (durable-memory-record-importance-score record)
   :staleness-score (durable-memory-record-staleness-score record)
   :last-accessed-universal-time (durable-memory-record-last-accessed-universal-time record)
   :tags (durable-memory-record-tags record)
   :version (durable-memory-record-version record)
   :supersedes-id (durable-memory-record-supersedes-id record)
   :superseded-by-id (durable-memory-record-superseded-by-id record)
   ;; Phase 7: Embedding fields
   :embedding (durable-memory-record-embedding record)
   :embedding-model (durable-memory-record-embedding-model record)
   :embedding-version (durable-memory-record-embedding-version record)))

(defun plist-to-durable-memory-record (plist)
  "Deserialize a plist into a durable-memory-record.
   Returns a durable-memory-record struct."
  (make-durable-memory-record
   :id (getf plist :id "")
   :kind (getf plist :kind :user)
   :subject-id (getf plist :subject-id "")
   :title (getf plist :title "")
   :content (getf plist :content nil)
   :source (getf plist :source :conversation)
   :created-universal-time (getf plist :created-universal-time 0)
   :updated-universal-time (getf plist :updated-universal-time 0)
   :importance-score (getf plist :importance-score 0.0)
   :staleness-score (getf plist :staleness-score 0.0)
   :last-accessed-universal-time (getf plist :last-accessed-universal-time 0)
   :tags (getf plist :tags nil)
   :version (getf plist :version 1)
   :supersedes-id (getf plist :supersedes-id nil)
   :superseded-by-id (getf plist :superseded-by-id nil)
   ;; Phase 7: Embedding fields
   :embedding (getf plist :embedding)
   :embedding-model (getf plist :embedding-model)
   :embedding-version (getf plist :embedding-version)))

;;; ============================================================
;;; Simple Test Functions
;;; ============================================================

(defun test-durable-memory-constructors ()
  "Simple test function to construct each durable memory kind."
  (list
   (make-user-memory :id "uuid-u1" :subject-id "user-123" :title "Prefers concise" :content "User prefers concise responses." :tags '(:preference :communication))
   (make-feedback-memory :id "uuid-f1" :subject-id "user-123" :title "Feedback: Too verbose" :content "Your last answer was too verbose." :tags '(:feedback))
   (make-project-memory :id "uuid-p1" :subject-id "proj-456" :title "Project uses library Z" :content "We decided to use library Z." :tags '(:decision :library))
   (make-reference-memory :id "uuid-r1" :subject-id "proj-456" :title "Reference: Docs" :content "https://docs.example.com" :tags '(:reference :link))))

(defun test-durable-memory-serialization ()
  "Simple test function for serialization/deserialization round-trip."
  (let* ((record (make-user-memory :id "uuid-u2" :subject-id "user-789" :title "Prefers markdown" :content "User prefers markdown output." :tags '(:preference :format)))
         (plist (durable-memory-record-to-plist record))
         (record2 (plist-to-durable-memory-record plist)))
    (and (equal (durable-memory-record-id record) (durable-memory-record-id record2))
         (equal (durable-memory-record-title record) (durable-memory-record-title record2))
         (equal (durable-memory-record-tags record) (durable-memory-record-tags record2)))))

;;; ============================================================
;;; Phase 7: Embedding Index Structure
;;; ============================================================

(defparameter *durable-memory-embedding-index* (make-hash-table :test 'eq)
  "Hash table mapping KIND keyword → alist of (RECORD-ID . EMBEDDING).
Each alist entry is (record-id . embedding), where embedding is a list of single-float.")

(defun update-embedding-index (kind record-id embedding)
  "Add or update embedding in index for KIND and RECORD-ID.
If EMBEDDING is NIL, removes the entry."
  (let* ((alist (gethash kind *durable-memory-embedding-index*)))
    (if embedding
        ;; Add or update
        (let ((existing (assoc record-id alist :test #'equal)))
          (if existing
              (setf (cdr existing) embedding)
              (setf (gethash kind *durable-memory-embedding-index*)
                    (acons record-id embedding alist))))
        ;; Remove entry if embedding is NIL
        (setf (gethash kind *durable-memory-embedding-index*)
              (remove record-id alist :test #'equal :key #'car)))))

(defun get-embedding-from-index (kind record-id)
  "Retrieve embedding from index for KIND and RECORD-ID, or NIL if not found."
  (let ((alist (gethash kind *durable-memory-embedding-index*)))
    (cdr (assoc record-id alist :test #'equal))))

(defun preload-all-embeddings (records)
  "Preload embeddings from RECORDS into *durable-memory-embedding-index*.
RECORDS is a list of durable-memory-record."
  (clrhash *durable-memory-embedding-index*)
  (dolist (record records)
    (let ((embedding (durable-memory-record-embedding record)))
      (when embedding
        (update-embedding-index
         (durable-memory-record-kind record)
         (durable-memory-record-id record)
         embedding)))))

(defun retrieve-record-embedding (record)
  "Retrieve embedding for RECORD, using index if available.
Returns embedding (list of single-float) or NIL."
  (or (durable-memory-record-embedding record)
      (get-embedding-from-index
       (durable-memory-record-kind record)
       (durable-memory-record-id record))))

;;; ============================================================
;;; Backward Compatibility Functions (for consolidate.lisp)
;;; ============================================================

(defun durable-memory-root (config)
  "Return the durable-memory directory for CONFIG."
  (merge-pathnames
   (make-pathname :directory '(:relative "durable"))
   (uiop:ensure-directory-pathname
    (claw-lisp.config:runtime-config-memory-root config))))

(defun durable-memory-index-path (config)
  "Return the durable-memory index pathname for CONFIG."
  (merge-pathnames "MEMORY.md" (durable-memory-root config)))

(defun durable-memory-note-path (config session-id)
  "Return the durable-memory note pathname for SESSION-ID."
  (merge-pathnames
   (make-pathname :name session-id :type "md")
   (durable-memory-root config)))

(defun %durable-note-files (config)
  "Return the per-session durable-memory note files under CONFIG's memory root,
excluding the MEMORY.md index itself."
  (let* ((root (durable-memory-root config))
         (pattern (merge-pathnames "*.md" root)))
    (remove-if (lambda (p) (string= "MEMORY" (or (pathname-name p) "")))
               (ignore-errors (directory pattern)))))

(defun update-durable-memory-index (config)
  "Rewrite the durable-memory MEMORY.md index for CONFIG.
Lists each per-session note file. Writes '- none' only when there are none."
  (let* ((path (durable-memory-index-path config))
         (notes (%durable-note-files config)))
    (ensure-directories-exist path)
    (with-open-file (stream path
                            :direction :output
                            :if-exists :supersede
                            :if-does-not-exist :create)
      (format stream "# Durable Memory Index~%~%")
      (if (null notes)
          (format stream "- none~%")
          (dolist (note notes)
            (let ((name (pathname-name note)))
              (format stream "- [~A](~A.md)~%" name name)))))
    path))

(defun %conversation-struct-to-plist (conversation)
  "Adapt a CLAW-LISP.CORE.DOMAIN:CONVERSATION struct to the plist shape
expected by the Phase 6 Task 5 extraction pipeline."
  (if (typep conversation 'claw-lisp.core.domain:conversation)
      (list :messages
            (mapcar
             (lambda (msg)
               (list :role (claw-lisp.core.domain:message-role msg)
                     :content
                     (let ((content (claw-lisp.core.domain:message-content msg)))
                       (if (stringp content)
                           content
                           ;; Best-effort flatten: use the text accessor on
                           ;; block-list content so heuristics see a string.
                           (or (ignore-errors
                                 (claw-lisp.core.domain:message-content-text msg))
                               "")))))
             (claw-lisp.core.domain:conversation-messages conversation)))
      conversation))

(defun extract-durable-memory (config session)
  "Extract durable memory from SESSION and persist it via the Phase 6 Task 5 pipeline.

CONFIG is the runtime-config (not a runtime). SESSION is an agent-session.

Delegates to INGEST-DURABLE-MEMORY-FROM-SESSION. Returns the list of saved
DURABLE-MEMORY-RECORDs (possibly empty). Returns NIL when durable memory is
disabled or the session has no conversation state to extract from.

Also writes a per-session snapshot file (<session-id>.md) for legacy test compatibility."
  (let ((conversation
          (ignore-errors
            (claw-lisp.core.domain:agent-session-conversation session)))
        (subject-id
          (ignore-errors
            (claw-lisp.core.domain:agent-session-id session))))
    (when (and conversation subject-id)
      (let ((saved (ingest-durable-memory-from-session
                    (%conversation-struct-to-plist conversation)
                    nil ; session-memory is currently unused by the pipeline
                    subject-id
                    :config config)))
        (when saved
          ;; Keep the MEMORY.md index in sync with the new records.
          (ignore-errors (update-durable-memory-index config)))
        ;; Write per-session snapshot for legacy test compatibility
        (ignore-errors
          (let ((note-path (durable-memory-note-path config subject-id)))
            (ensure-directories-exist note-path)
            (with-open-file (stream note-path
                                    :direction :output
                                    :if-exists :supersede
                                    :if-does-not-exist :create)
              (format stream "---~%")
              (format stream "session_id: ~A~%" subject-id)
              (format stream "type: project~%")
              (format stream "---~%~%")
              (format stream "# Durable Memory ~A~%~%" subject-id)
              (format stream "## Last User Request~%")
              (let ((last-user-msg
                      (car (last (claw-lisp.core.domain:conversation-messages conversation)))))
                (if last-user-msg
                    (let ((content (claw-lisp.core.domain:message-content last-user-msg)))
                      (format stream "- ~A~%~%" (if (stringp content) content "")))
                    (format stream "- none~%~%")))
              (format stream "## Recent Tool Results~%")
              (let ((tool-results (claw-lisp.core.domain:conversation-tool-results conversation)))
                (if tool-results
                    (dolist (result (last tool-results (min 3 (length tool-results))))
                      (format stream "- ~A: ~A~@[ (full: ~A)~]~%"
                              (claw-lisp.core.domain:tool-result-tool-name result)
                              (claw-lisp.core.domain:tool-result-content result)
                              (claw-lisp.core.domain:tool-result-persisted-path result)))
                    (format stream "- none~%"))))))
        saved))))

;;; ============================================================
;;; Phase 6 Task 2: Durable Memory Config & Registry
;;; ============================================================

(defparameter *durable-memory-policies*
  '((:user . (:importance-threshold 0.7
             :max-age-days 365
             :deduplicate-p nil))
    (:feedback . (:importance-threshold 0.7
                 :max-age-days 180
                 :deduplicate-p nil))
    (:project . (:importance-threshold 0.5
                :max-age-days 365
                :deduplicate-p nil))
    (:reference . (:importance-threshold 0.5
                  :max-age-days 365
                  :deduplicate-p t)))
  "Alist mapping durable memory kind keywords to their default policies.

Each policy is a plist with keys:
  - :importance-threshold (float 0.0–1.0)
  - :max-age-days (integer)
  - :deduplicate-p (boolean)")

(defun get-durable-memory-config (config)
  "Return a plist of durable memory related config values from CONFIG struct."
  (list
   :enabled (claw-lisp.config:runtime-config-durable-memory-enabled-p config)
   :user-budget (claw-lisp.config:runtime-config-durable-user-memory-budget-chars config)
   :feedback-budget (claw-lisp.config:runtime-config-durable-feedback-memory-budget-chars config)
   :project-budget (claw-lisp.config:runtime-config-durable-project-memory-budget-chars config)
   :reference-budget (claw-lisp.config:runtime-config-durable-reference-memory-budget-chars config)
   :max-records-per-kind (claw-lisp.config:runtime-config-durable-memory-max-records-per-kind config)
   :max-record-age-days (claw-lisp.config:runtime-config-durable-memory-max-record-age-days config)))

(defun durable-memory-kind-policy (kind)
  "Return the default policy plist for durable memory KIND keyword.
   Returns NIL if KIND is not recognized."
  (cdr (assoc kind *durable-memory-policies* :test #'eq)))

;;; ============================================================
;;; Phase 6 Task 3: Save/Ignore Criteria & Scoring Engine
;;; ============================================================

;;; ------------------------------------------------------------
;;; Durable Memory Candidate
;;; ------------------------------------------------------------

(defstruct (durable-memory-candidate
            (:constructor make-durable-memory-candidate
                (&key
                 kind
                 subject-id
                 content
                 source
                 context-window
                 explicit-user-request-p
                 tags
                 metadata)))
  "A candidate piece of information for durable memory.

Fields:
  - kind: Target durable memory kind (:user, :feedback, :project, :reference).
  - subject-id: User/project/etc. identifier (string).
  - content: Primary content string to consider for storage.
  - source: Provenance (:conversation, :tool, :manual, etc.).
  - context-window: Recent messages or context (list of plists/strings).
  - explicit-user-request-p: T if user explicitly asked to remember this.
  - tags: Optional list of tags (keywords/strings).
  - metadata: Optional plist with additional hints (e.g. :task-id, :file-path)."
  (kind :user :type keyword)
  (subject-id "" :type string)
  (content "" :type string)
  (source :conversation :type keyword)
  (context-window nil :type list)
  (explicit-user-request-p nil :type boolean)
  (tags nil :type list)
  (metadata nil :type list))

;;; ------------------------------------------------------------
;;; Internal Utilities
;;; ------------------------------------------------------------

(defun %string-empty-or-whitespace-p (s)
  "Return T if S is NIL or consists only of whitespace."
  (or (null s)
      (and (stringp s)
           (every (lambda (ch) (find ch " \t\n\r" :test #'char=)) s))))

(defun %normalize-content (content)
  "Return CONTENT as a trimmed string (or empty string if NIL)."
  (let ((s (if (stringp content) content (princ-to-string content))))
    (string-trim '(#\Space #\Tab #\Newline #\Return) s)))

(defun %count-words (s)
  "Rudimentary word count for S."
  (let* ((trimmed (%normalize-content s)))
    (if (%string-empty-or-whitespace-p trimmed)
        0
        (length (remove "" (split-sequence:split-sequence-if
                            (lambda (ch) (find ch " \t\n\r" :test #'char=))
                            trimmed)
                        :test #'string=)))))

(defun %contains-any-substring-p (s substrings)
  "Return T if S contains any of SUBSTRINGS (case-insensitive)."
  (let ((lower (string-downcase s)))
    (some (lambda (sub)
            (search (string-downcase sub) lower))
          substrings)))

(defun %split-lines (s)
  "Split S into lines."
  (when s
    (split-sequence:split-sequence #\Newline s)))

;;; ------------------------------------------------------------
;;; Explicit User Request Detection
;;; ------------------------------------------------------------

(defun detect-explicit-user-request-p (candidate)
  "Heuristically detect if the user explicitly asked to remember this.
   This is a fallback in case EXPLICIT-USER-REQUEST-P was not set by
   upstream logic. It scans the candidate CONTENT and CONTEXT-WINDOW
   for phrases like \"remember this\", \"please remember\", etc."
  (or (durable-memory-candidate-explicit-user-request-p candidate)
      (let* ((content (%normalize-content
                       (durable-memory-candidate-content candidate)))
             (context (durable-memory-candidate-context-window candidate))
             (phrases '("remember this"
                        "please remember"
                        "you should remember"
                        "keep this in mind"
                        "for future reference"
                        "in the future, remember"
                        "always do this"
                        "my preference is"
                        "i prefer that you")))
        (or (%contains-any-substring-p content phrases)
            (some (lambda (msg)
                    (when (stringp msg)
                      (%contains-any-substring-p msg phrases)))
                  context)))))

;;; ------------------------------------------------------------
;;; Anti-Criteria Pattern Heuristics
;;; ------------------------------------------------------------

(defparameter *stack-trace-line-patterns*
  '((( :prefix . "Traceback (most recent call last):"))
    ((:prefix . "  File \""))
    ((:prefix . "    at "))
    ((:prefix . "at "))
    ((:contains . "Exception:"))
    ((:contains . "Error:"))
    ((:contains . "ERROR "))
    ((:contains . "WARN "))
    ((:contains . "WARNING ")))
  "Patterns that indicate stack trace or error log lines.")

(defparameter *temporary-path-substrings*
  '("/tmp/" "\\tmp\\" ".tmp" ".log" "/var/log/" "node_modules/" "dist/")
  "Substrings indicating temporary or build paths.")

(defparameter *code-block-indicators*
  '("```" ";;;" "#include" "using System;" "public class " "def " "class "
    "function " "fn " "let " "const " "var " "=> {" "BEGIN {" "END;")
  "Substrings that suggest the content is primarily raw code.")

(defparameter *repo-path-indicators*
  '("src/" "lib/" "lisp/" "tests/" "spec/" "README.md" "package.json"
    "setup.py" "requirements.txt" ".gitignore")
  "Substrings that suggest code or files that likely exist in the repo.")

(defun %detect-stack-trace-or-error-log-p (content)
  "Return T if CONTENT looks like a stack trace or error log."
  (let ((lines (%split-lines content)))
    (and lines
         (>= (length lines) 2)
         (>= (count-if (lambda (line)
                         (%contains-any-substring-p line '("Traceback" "File \"" "  at " "Exception:" "Error:")))
                       lines)
             2))))

(defun %detect-temporary-paths-p (content)
  "Return T if CONTENT contains obvious temporary or log paths."
  (%contains-any-substring-p content *temporary-path-substrings*))

(defun %detect-raw-code-block-p (content)
  "Return T if CONTENT appears to be primarily raw code."
  (let* ((lines (%split-lines content))
         (line-count (length lines)))
    (when (and lines (> line-count 0))
      (let* ((indicator-hit (some (lambda (ind)
                                    (search ind content :test #'char-equal))
                                  *code-block-indicators*))
             (semicolon-lines (count-if (lambda (line)
                                          (and (> (length line) 0)
                                               (char= (char line (1- (length line))) #\;)))
                                        lines))
             (brace-lines (count-if (lambda (line)
                                      (or (search "{" line) (search "}" line)))
                                    lines)))
        (or indicator-hit (>= (+ semicolon-lines brace-lines) (floor line-count 2)))))))

(defun %detect-repo-derived-code-p (content)
  "Return T if CONTENT appears to be code or paths that likely exist in the repo."
  (or (%contains-any-substring-p content *repo-path-indicators*)
      (%detect-raw-code-block-p content)))

;;; ------------------------------------------------------------
;;; Importance Scoring
;;; ------------------------------------------------------------

(defun compute-durable-memory-importance-score (candidate)
  "Compute an importance score (0.0–1.0) for CANDIDATE.

Positive factors:
  - Explicit user request (strong boost).
  - Length and specificity (non-trivial number of words).
  - Stability: not tied to a single ephemeral task or timestamp.
  - Non-derivability: content that is not obviously code or repo-derived."
  (let* ((content (%normalize-content (durable-memory-candidate-content candidate)))
         (word-count (%count-words content))
         (score 0.0))
    ;; Explicit user request: strong baseline.
    (when (detect-explicit-user-request-p candidate)
      (incf score 0.5))
    ;; Content length / specificity: reward moderate length.
    (cond ((< word-count 5) (incf score 0.0))
          ((<= word-count 40) (incf score 0.25))
          ((<= word-count 200) (incf score 0.35))
          (t (incf score 0.2)))
    ;; Penalize if content looks like raw code or repo-derived.
    (when (%detect-repo-derived-code-p content)
      (decf score 0.2))
    ;; Clamp to [0.0, 1.0].
    (max 0.0 (min 1.0 score))))

;;; ------------------------------------------------------------
;;; Anti-Score (Ephemeral / Debug / Derivable)
;;; ------------------------------------------------------------

(defun compute-durable-memory-anti-score (candidate)
  "Compute an anti-score (0.0–1.0) for CANDIDATE.

Higher anti-score means the content is more likely to be ephemeral,
debugging residue, or derivable from other sources."
  (let* ((content (%normalize-content (durable-memory-candidate-content candidate)))
         (anti 0.0))
    ;; Stack traces / error logs: very strong anti-signal.
    (when (%detect-stack-trace-or-error-log-p content)
      (incf anti 0.7))
    ;; Temporary paths / logs.
    (when (%detect-temporary-paths-p content)
      (incf anti 0.3))
    ;; Raw code or repo-derived code.
    (when (%detect-repo-derived-code-p content)
      (incf anti 0.4))
    ;; Ephemeral task instructions and time-bound phrases.
    (when (%contains-any-substring-p content '("now run" "right now" "today" "temporary" "debug" "stack trace" "log output"))
      (incf anti 0.2))
    ;; Clamp to [0.0, 1.0].
    (max 0.0 (min 1.0 anti))))

;;; ------------------------------------------------------------
;;; Decision Logic
;;; ------------------------------------------------------------

(defun %durable-memory-kind-threshold (kind)
  "Return the default importance threshold for KIND."
  (let* ((policy (durable-memory-kind-policy kind))
         (threshold (and policy (getf policy :importance-threshold))))
    (or threshold (ecase kind (:user 0.55) (:feedback 0.6) (:project 0.5) (:reference 0.5)))))

(defun should-save-durable-memory-p (candidate)
  "Decide whether CANDIDATE should be persisted as durable memory.
   Returns (values SHOULD-SAVE-P REASON-CODE FINAL-SCORE IMPORTANCE ANTI-SCORE).

REASON-CODE is one of:
  - :anti-criteria            — Rejected due to strong anti-criteria.
  - :explicit-request         — Saved due to explicit user request.
  - :score-exceeded-threshold — Saved because final score >= threshold.
  - :score-below-threshold    — Rejected because final score < threshold.
  - :empty-content            — Rejected because content is empty."
  (let* ((kind (durable-memory-candidate-kind candidate))
         (content (%normalize-content (durable-memory-candidate-content candidate))))
    (if (%string-empty-or-whitespace-p content)
        (values nil :empty-content 0.0 0.0 0.0)
        (let* ((importance (compute-durable-memory-importance-score candidate))
               (anti-score (compute-durable-memory-anti-score candidate))
               (final-score (max 0.0 (min 1.0 (- importance anti-score))))
               (threshold (%durable-memory-kind-threshold kind))
               (explicit (detect-explicit-user-request-p candidate)))
          (cond ((>= anti-score 0.95) (values nil :anti-criteria final-score importance anti-score))
                ((and explicit (< anti-score 0.95)) (values t :explicit-request final-score importance anti-score))
                ((>= final-score threshold) (values t :score-exceeded-threshold final-score importance anti-score))
                (t (values nil :score-below-threshold final-score importance anti-score)))))))


;;; ============================================================
;;; Phase 6 Task 4: Durable Memory Storage Backend
;;; ============================================================


;;;; lisp/storage/durable-memory.lisp
;;;;
;;;; Durable Memory Storage Backend (File-Based)
;;;;
;;;; Implements file-based storage for durable memory records.
;;;; Follows patterns from session-memory.lisp but is independent.
;;;; Serialization is in plist format for each record.
;;;; Storage layout: .claw-lisp/memory/durable/<kind>/<subject-id>.md

(in-package #:claw-lisp.storage.durable-memory)

;;; ============================================================
;;; Storage Path Utilities
;;; ============================================================

(defparameter *durable-memory-storage-root*
  (merge-pathnames #P".claw-lisp/memory/durable/")
  "Root directory for durable memory storage.")

(defun durable-memory-kind-directory (kind)
  "Return the directory path for KIND (as a string or keyword)."
  (merge-pathnames
   (format nil "~(~A~)/" kind)
   *durable-memory-storage-root*))

(defun durable-memory-file-path (kind subject-id)
  "Return the full path for the durable memory file for KIND and SUBJECT-ID."
  (let ((dir (durable-memory-kind-directory kind)))
    (ensure-directories-exist dir)
    (merge-pathnames
     (format nil "~A.md" subject-id)
     dir)))

;;; ============================================================
;;; Metadata Header Utilities
;;; ============================================================

(defstruct (durable-memory-metadata
            (:constructor make-durable-memory-metadata
                          (&key kind subject-id record-count last-updated)))
  "Metadata for durable memory file."
  (kind :user :type keyword)
  (subject-id "" :type string)
  (record-count 0 :type integer)
  (last-updated 0 :type integer))

(defun render-durable-memory-header (metadata)
  "Render the metadata section as a markdown comment block."
  (format nil "<!-- Durable Memory Index~%kind: ~A~%subject-id: ~A~%record-count: ~A~%last-updated: ~A~%-->~%"
          (durable-memory-metadata-kind metadata)
          (durable-memory-metadata-subject-id metadata)
          (durable-memory-metadata-record-count metadata)
          (durable-memory-metadata-last-updated metadata)))

(defun parse-durable-memory-header (text)
  "Parse the durable memory metadata from the markdown comment header in TEXT.
   Returns a durable-memory-metadata struct or NIL if not found."
  (let ((start (search "<!--" text))
        (end (search "-->" text)))
    (when (and start end (> end start))
      (let* ((header (subseq text start (+ end 3)))
             (kind (let ((pos (search "kind:" header)))
                     (when pos
                       (let* ((start (+ pos 5))
                              (end (position #\Newline header :start start)))
                         (read-from-string (string-trim '(#\Space #\:) (subseq header start end)))))))
             (subject-id (let ((pos (search "subject-id:" header)))
                           (when pos
                             (let* ((start (+ pos 11))
                                    (end (position #\Newline header :start start)))
                               (string-trim '(#\Space #\:) (subseq header start end))))))
             (record-count (let ((pos (search "record-count:" header)))
                             (when pos
                               (parse-integer header :start (+ pos 13) :junk-allowed t))))
             (last-updated (let ((pos (search "last-updated:" header)))
                             (when pos
                               (parse-integer header :start (+ pos 13) :junk-allowed t)))))
        (make-durable-memory-metadata
         :kind (or kind :user)
         :subject-id (or subject-id "")
         :record-count (or record-count 0)
         :last-updated (or last-updated 0))))))

;;; ============================================================
;;; Serialization Utilities
;;; ============================================================

(defun serialize-durable-memory-record (record)
  "Serialize a durable-memory-record as a pretty-printed Lisp form."
  (with-output-to-string (s)
    (pprint (durable-memory-record-to-plist record) s)))

(defun deserialize-durable-memory-record (string)
  "Deserialize a durable-memory-record from a Lisp plist string.
   Returns NIL if parsing fails."
  (handler-case
      (let ((form (read-from-string string)))
        (when (and (listp form)
                   (evenp (length form)))
          (plist-to-durable-memory-record form)))
    (error (e)
      (format *error-output* "~&[durable-memory] Failed to parse record: ~A~%" e)
      nil)))

;;; ============================================================
;;; File Parsing and Writing
;;; ============================================================

(defun split-header-and-records (text)
  "Split the file TEXT into (header-string records-string).
   If no header, header-string is NIL."
  (let ((start (search "<!--" text))
        (end (search "-->" text)))
    (if (and start end (> end start))
        (values (subseq text start (+ end 3))
                (subseq text (+ end 3)))
        (values nil text))))

(defun parse-durable-memory-records-section (text)
  "Parse all durable memory records from TEXT (after header).
   Returns a list of durable-memory-record structs."
  (let ((records '()))
    (with-input-from-string (in text)
      (loop
        (handler-case
            (let ((form (read in nil :eof)))
              (when (eq form :eof)
                (return))
              (when (and (listp form) (evenp (length form)))
                (let ((record (plist-to-durable-memory-record form)))
                  (push record records))))
          (end-of-file ()
            (return))
          (error (e)
            (format *error-output* "[durable-memory] Skipping corrupted record form: ~A~%" e)))))
    (nreverse records)))

(defun write-durable-memory-file (file-path metadata records)
  "Write durable memory METADATA and RECORDS to FILE-PATH."
  (with-open-file (out file-path
                       :direction :output
                       :if-exists :supersede
                       :if-does-not-exist :create)
    (princ (render-durable-memory-header metadata) out)
    (format out "~%~%## Records~%~%")
    (dolist (record records)
      (princ (serialize-durable-memory-record record) out)
      (terpri out)
      (terpri out))))

;;; ============================================================
;;; ID Generation Utility
;;; ============================================================

(defun generate-durable-memory-id (kind)
  "Generate a simple unique id for a durable memory record."
  (format nil "~(~A~)-~A-~A"
          kind
          (get-universal-time)
          (random 1000000)))

;;; ============================================================
;;; Public API
;;; ============================================================

(defun load-durable-memories (kind subject-id)
  "Load all durable memory records for KIND and SUBJECT-ID.
   Returns a list of durable-memory-record structs.
   If file does not exist, returns NIL."
  (let ((file (durable-memory-file-path kind subject-id)))
    (if (probe-file file)
        (with-open-file (in file :direction :input)
          (let ((text (let ((contents (make-string (file-length in))))
                        (read-sequence contents in)
                        contents)))
            (multiple-value-bind (header records-section)
                (split-header-and-records text)
              (declare (ignore header))
              (parse-durable-memory-records-section records-section))))
        nil)))

(defun save-durable-memory-record (record)
  "Append RECORD to the durable memory file for its kind/subject-id.
   If RECORD has no id, assign one. Returns the (possibly updated) record."
  (let* ((kind (durable-memory-record-kind record))
         (subject-id (durable-memory-record-subject-id record))
         (file (durable-memory-file-path kind subject-id))
         (existing-records (or (load-durable-memories kind subject-id) '()))
         (now (get-universal-time))
         (id (or (durable-memory-record-id record)
                 (generate-durable-memory-id kind)))
         (record-with-id (make-durable-memory-record
                          :id id
                          :kind (durable-memory-record-kind record)
                          :subject-id (durable-memory-record-subject-id record)
                          :title (durable-memory-record-title record)
                          :content (durable-memory-record-content record)
                          :source (durable-memory-record-source record)
                          :created-universal-time (or (durable-memory-record-created-universal-time record) now)
                          :updated-universal-time now
                          :importance-score (durable-memory-record-importance-score record)
                          :staleness-score (durable-memory-record-staleness-score record)
                          :last-accessed-universal-time (durable-memory-record-last-accessed-universal-time record)
                          :tags (durable-memory-record-tags record)
                          :version (durable-memory-record-version record)
                          :supersedes-id (durable-memory-record-supersedes-id record))))
    ;; Remove any records superseded by this one (by supersedes-id)
    (let* ((supersedes-id (durable-memory-record-supersedes-id record-with-id))
           (filtered-records (if supersedes-id
                                 (remove supersedes-id existing-records
                                         :key #'durable-memory-record-id
                                         :test #'string=)
                                 existing-records))
           (new-records (append filtered-records (list record-with-id)))
           (metadata (make-durable-memory-metadata
                      :kind kind
                      :subject-id subject-id
                      :record-count (length new-records)
                      :last-updated now)))
      (write-durable-memory-file file metadata new-records)
      record-with-id)))

(defun update-durable-memory-record (record)
  "Update an existing durable memory record by id.
   Returns the updated record, or NIL if not found."
  (let* ((kind (durable-memory-record-kind record))
         (subject-id (durable-memory-record-subject-id record))
         (file (durable-memory-file-path kind subject-id))
         (existing-records (or (load-durable-memories kind subject-id) '()))
         (id (durable-memory-record-id record))
         (now (get-universal-time)))
    (if (and id (find id existing-records :key #'durable-memory-record-id :test #'string=))
        (let* ((updated-record (make-durable-memory-record
                                :id (durable-memory-record-id record)
                                :kind (durable-memory-record-kind record)
                                :subject-id (durable-memory-record-subject-id record)
                                :title (durable-memory-record-title record)
                                :content (durable-memory-record-content record)
                                :source (durable-memory-record-source record)
                                :created-universal-time (durable-memory-record-created-universal-time record)
                                :updated-universal-time now
                                :importance-score (durable-memory-record-importance-score record)
                                :staleness-score (durable-memory-record-staleness-score record)
                                :last-accessed-universal-time (durable-memory-record-last-accessed-universal-time record)
                                :tags (durable-memory-record-tags record)
                                :version (durable-memory-record-version record)
                                :supersedes-id (durable-memory-record-supersedes-id record)))
               (new-records (mapcar (lambda (r)
                                      (if (string= (durable-memory-record-id r) id)
                                          updated-record
                                          r))
                                    existing-records))
               (metadata (make-durable-memory-metadata
                          :kind kind
                          :subject-id subject-id
                          :record-count (length new-records)
                          :last-updated now)))
          (write-durable-memory-file file metadata new-records)
          updated-record)
        nil)))

(defun delete-durable-memory-record (kind subject-id id)
  "Delete a durable memory record by id (soft delete).
   Returns T if deleted, NIL if not found."
  (let* ((file (durable-memory-file-path kind subject-id))
         (existing-records (or (load-durable-memories kind subject-id) '()))
         (filtered-records (remove id existing-records
                                   :key #'durable-memory-record-id
                                   :test #'string=))
         (now (get-universal-time)))
    (if (< (length filtered-records) (length existing-records))
        (let ((metadata (make-durable-memory-metadata
                         :kind kind
                         :subject-id subject-id
                         :record-count (length filtered-records)
                         :last-updated now)))
          (write-durable-memory-file file metadata filtered-records)
          t)
        nil)))

;;; ============================================================
;;; End of Durable Memory Storage Backend
;;; ============================================================


;;; ============================================================
;;; Phase 6 Task 5: Ingestion Pipeline
;;; ============================================================


(in-package #:claw-lisp.storage.durable-memory)

;;; ============================================================
;;; Phase 6 Task 5: Ingestion Pipeline from Session to Durable Memory
;;; ============================================================

;;; This section wires the durable memory scoring/criteria and storage
;;; backend into a simple ingestion pipeline that can be called from
;;; orchestration code after turns, at end-of-session, or on explicit
;;; triggers.
;;;
;;; Design goals:
;;; - Lightweight, mostly pure heuristics for candidate extraction.
;;; - Clear separation between:
;;;     * extraction (conversation → candidates),
;;;     * decision (scoring engine),
;;;     * persistence (storage backend).
;;; - Conservative defaults: only save high-signal, low-ephemeral content.
;;; - Easy to test in isolation with synthetic conversation/session data.

;;; ------------------------------------------------------------
;;; Utility: Safe string helpers
;;; ------------------------------------------------------------

(defun %safe-string-downcase (thing)
  "Return a downcased string representation of THING, or \"\" if NIL."
  (declare (inline %safe-string-downcase))
  (cond
    ((stringp thing) (string-downcase thing))
    ((null thing) "")
    (t (string-downcase (princ-to-string thing)))))

(defun %string-contains-any-p (string substrings)
  "Return T if STRING contains any of SUBSTRINGS (case-insensitive)."
  (let ((s (%safe-string-downcase string)))
    (some (lambda (sub)
            (and sub
                 (not (null (search (%safe-string-downcase sub) s)))))
          substrings)))
(defun %string-matches-regex-p (string regex)
  "Return T if STRING matches REGEX (if CL-PPCRE is available).
If CL-PPCRE is not available, fall back to simple SEARCH heuristics
for a few common patterns."
  ;; We avoid a hard dependency here; if CL-PPCRE is present in the
  ;; environment, callers can rebind or extend this.
  (declare (ignore regex))
  (declare (inline %string-matches-regex-p))
  ;; For now, just return NIL; higher-level heuristics use SEARCH.
  nil)

;;; ------------------------------------------------------------
;;; Conversation / session access helpers
;;; ------------------------------------------------------------

;; We intentionally keep the shape of CONVERSATION and SESSION-MEMORY
;; generic (plists/alists) so this module does not depend tightly on
;; orchestration internals. The orchestration layer is expected to pass
;; in a structure with at least:
;;   - a list of messages (most recent last),
;;   - each message having :role and :content keys.
;;
;; Example message plist:
;;   (:role :user :content \"I prefer concise answers.\")

(defun %conversation-messages (conversation)
  "Extract the list of messages from CONVERSATION.
This is a small adapter to avoid hard-coding the exact key name."
  (or (getf conversation :messages)
      (getf conversation :conversation-messages)
      ;; Fallback: treat CONVERSATION itself as a list of messages.
      (when (and (listp conversation)
                 (every #'listp conversation))
        conversation)))

(defun %message-role (message)
  "Return the role of MESSAGE (:user, :assistant, etc.)."
  (or (getf message :role)
      (getf message :speaker)
      :unknown))

(defun %message-content (message)
  "Return the content string of MESSAGE."
  (or (getf message :content)
      (getf message :text)
      ""))

(defun %last-user-message (conversation)
  "Return the last user message in CONVERSATION, or NIL."
  (let ((messages (%conversation-messages conversation)))
    (when messages
      (loop for msg in (reverse messages)
            when (eql (%message-role msg) :user)
              do (return msg)))))

(defun %recent-messages-window (conversation &key (max-count 6))
  "Return up to MAX-COUNT most recent messages from CONVERSATION."
  (let* ((messages (%conversation-messages conversation))
         (len (length messages)))
    (if (<= len max-count)
        messages
        (subseq messages (- len max-count) len))))

;;; ------------------------------------------------------------
;;; Heuristics: Detect explicit user \"remember\" requests
;;; ------------------------------------------------------------

(defun %explicit-remember-request-p (text)
  "Return T if TEXT looks like an explicit request to remember something."
  (and text
       (%string-contains-any-p
        text
        '("please remember"
          "remember this"
          "remember that"
          "can you remember"
          "i want you to remember"
          "store this"
          "save this for later"))))

(defun %extract-explicit-remember-snippet (text)
  "Extract a snippet from TEXT that follows an explicit remember phrase.
This is a simple heuristic: we look for the first remember phrase and
return the substring from there to the end. Callers may further trim."
  (let* ((lower (%safe-string-downcase text))
         (phrases '("please remember"
                    "remember this"
                    "remember that"
                    "can you remember"
                    "i want you to remember"
                    "store this"
                    "save this for later"))
         (pos (loop for p in phrases
                    for idx = (search p lower)
                    when idx do (return idx))))
    (if pos
        (string-trim '(#\Space #\Tab #\Newline #\.)
                     (subseq text pos))
        (string-trim '(#\Space #\Tab #\Newline) text))))

;;; ------------------------------------------------------------
;;; Heuristics: User preferences
;;; ------------------------------------------------------------

(defun %user-preference-sentence-p (sentence)
  "Return T if SENTENCE looks like a user preference or habit."
  (let ((s (%safe-string-downcase sentence)))
    (or (%string-contains-any-p
         s
         '("i prefer"
           "my preference is"
           "i like when you"
           "i like it when you"
           "i usually"
           "i always"
           "i tend to"
           "i want you to"
           "please be more"
           "please be less"
           "from now on"
           "in the future, please"
           "by default, please"))
        (%explicit-remember-request-p s))))

(defun %split-into-sentences (text)
  "Very simple sentence splitter based on '.', '!' and '?'.
Returns a list of trimmed sentence strings."
  (let ((result '())
        (start 0)
        (len (length text)))
    (labels ((flush (end)
               (when (> end start)
                 (let ((segment (string-trim '(#\Space #\Tab #\Newline)
                                             (subseq text start end))))
                   (when (> (length segment) 0)
                     (push segment result))))))
      (loop for i from 0 below len
            for ch = (char text i) do
              (when (or (char= ch #\.)
                        (char= ch #\!)
                        (char= ch #\?))
                (flush i)
                (setf start (1+ i))))
      (flush len)
      (nreverse result))))

(defun %extract-user-preference-candidates (conversation subject-id)
  "Extract durable memory candidates for user preferences from CONVERSATION.
Returns a list of DURABLE-MEMORY-CANDIDATE structs."
  (let* ((last-user (%last-user-message conversation))
         (content (%message-content last-user)))
    (when (and last-user (> (length content) 0))
      (let* ((sentences (%split-into-sentences content))
             (preference-sentences
               (remove-if-not #'%user-preference-sentence-p sentences)))
        (mapcar (lambda (sentence)
                  (make-durable-memory-candidate
                   :kind :user
                   :subject-id subject-id
                   :content sentence
                   :source :conversation
                   :context-window (%recent-messages-window conversation)
                   :explicit-user-request-p (%explicit-remember-request-p sentence)
                   :tags '(:user-preference)))
                preference-sentences)))))

;;; ------------------------------------------------------------
;;; Heuristics: Project facts / decisions
;;; ------------------------------------------------------------

(defun %project-fact-sentence-p (sentence)
  "Return T if SENTENCE looks like a project fact or decision."
  (let ((s (%safe-string-downcase sentence)))
    (%string-contains-any-p
     s
     '("we decided to"
       "we decided that"
       "we will use"
       "we are going to use"
       "for this project"
       "in this project"
       "the project will use"
       "our stack will be"
       "we'll use"
       "we are using"
       "we should use"
       "let's use"
       "the architecture will"
       "the design will"
       "we agreed to"
       "we agreed that"))))

(defun %extract-project-fact-candidates (conversation subject-id)
  "Extract durable memory candidates for project facts from CONVERSATION.
SUBJECT-ID is typically a project identifier or conversation id."
  (let* ((messages (%conversation-messages conversation))
         ;; Look at a small window of recent messages from both user and assistant
         (recent (last messages (min 8 (length messages))))
         (sentences '()))
    (dolist (msg recent)
      (let ((role (%message-role msg)))
        (when (or (eql role :user) (eql role :assistant))
          (dolist (s (%split-into-sentences (%message-content msg)))
            (when (%project-fact-sentence-p s)
              (push s sentences))))))
    (mapcar (lambda (sentence)
              (make-durable-memory-candidate
               :kind :project
               :subject-id subject-id
               :content sentence
               :source :conversation
               :context-window (%recent-messages-window conversation)
               :explicit-user-request-p (%explicit-remember-request-p sentence)
               :tags '(:project-fact)))
            (nreverse sentences))))

;;; ------------------------------------------------------------
;;; Heuristics: Feedback about assistant behavior
;;; ------------------------------------------------------------

(defun %feedback-sentence-p (sentence)
  "Return T if SENTENCE looks like feedback about assistant behavior."
  (let ((s (%safe-string-downcase sentence)))
    (or (%string-contains-any-p
         s
         '("your last answer"
           "your previous answer"
           "you were too"
           "you are too"
           "you were not"
           "you are not"
           "be more concise"
           "be less verbose"
           "be more verbose"
           "give shorter answers"
           "give longer answers"
           "don't do that"
           "please don't"
           "stop doing"
           "i didn't like"
           "i don't like when you"
           "that was not helpful"
           "that was helpful"
           "please be more explicit"
           "please be more detailed"))
        (%string-contains-any-p
         s
         '("from now on, please"
           "in the future, please"
           "next time, please")))))

(defun %extract-feedback-candidates (conversation subject-id)
  "Extract durable memory candidates for user feedback from CONVERSATION."
  (let* ((last-user (%last-user-message conversation))
         (content (%message-content last-user)))
    (when (and last-user (> (length content) 0))
      (let* ((sentences (%split-into-sentences content))
             (feedback-sentences
               (remove-if-not #'%feedback-sentence-p sentences)))
        (mapcar (lambda (sentence)
                  (make-durable-memory-candidate
                   :kind :feedback
                   :subject-id subject-id
                   :content sentence
                   :source :conversation
                   :context-window (%recent-messages-window conversation)
                   :explicit-user-request-p (%explicit-remember-request-p sentence)
                   :tags '(:user-feedback)))
                feedback-sentences)))))

;;; ------------------------------------------------------------
;;; Heuristics: References (links, docs, recurring resources)
;;; ------------------------------------------------------------

(defun %extract-urls-from-text (text)
  "Extract a list of URL-like substrings from TEXT using simple heuristics."
  (let ((lower (%safe-string-downcase text))
        (urls '()))
    ;; Very simple heuristic: split on whitespace and keep tokens that
    ;; look like URLs.
    (dolist (token (split-sequence:split-sequence #\Space text :remove-empty-subseqs t))
      (let ((t-lower (string-downcase token)))
        (when (or (search "http://" t-lower)
                  (search "https://" t-lower)
                  (and (search "www." t-lower)
                       (search "." t-lower)))
          (push (string-trim '(#\Space #\Tab #\Newline #\) #\( #\, #\.)
                             token)
                urls))))
    (nreverse urls)))

(defun %reference-sentence-p (sentence)
  "Return T if SENTENCE looks like a reference to an external resource."
  (let ((s (%safe-string-downcase sentence)))
    (or (%string-contains-any-p
         s
         '("see"
           "refer to"
           "documentation"
           "docs"
           "spec"
           "issue #"
           "pull request"
           "pr #"
           "ticket #"
           "stack overflow"
           "github.com"
           "gitlab.com"))
        (not (null (%extract-urls-from-text sentence))))))

(defun %extract-reference-candidates (conversation subject-id)
  "Extract durable memory candidates for references from CONVERSATION.
We focus on URLs and explicit references that are likely to be reused."
  (let* ((messages (%conversation-messages conversation))
         (recent (last messages (min 10 (length messages))))
         (candidates '()))
    (dolist (msg recent)
      (let* ((role (%message-role msg))
             (content (%message-content msg)))
        (when (or (eql role :user) (eql role :assistant))
          ;; URL-based references
          (dolist (url (%extract-urls-from-text content))
            (push (make-durable-memory-candidate
                   :kind :reference
                   :subject-id subject-id
                   :content url
                   :source :conversation
                   :context-window (%recent-messages-window conversation)
                   :explicit-user-request-p (%explicit-remember-request-p content)
                   :tags '(:reference :url))
                  candidates))
          ;; Sentence-based references (docs, specs, etc.)
          (dolist (s (%split-into-sentences content))
            (when (%reference-sentence-p s)
              (push (make-durable-memory-candidate
                     :kind :reference
                     :subject-id subject-id
                     :content s
                     :source :conversation
                     :context-window (%recent-messages-window conversation)
                     :explicit-user-request-p (%explicit-remember-request-p s)
                     :tags '(:reference))
                    candidates))))))
    (nreverse candidates)))

;;; ------------------------------------------------------------
;;; Public API: Candidate extraction
;;; ------------------------------------------------------------

(defun extract-durable-memory-candidates (conversation session-memory subject-id)
  "Extract durable memory candidates from CONVERSATION and SESSION-MEMORY.

CONVERSATION is a plist or struct containing a list of messages.
SESSION-MEMORY is currently unused but included for future heuristics
(e.g., detecting repeated references across the session).

SUBJECT-ID is the durable memory subject identifier (e.g., user-id,
project-id, or conversation-id) and is attached to all candidates.

Returns a list of DURABLE-MEMORY-CANDIDATE structs.

Heuristics:
  - User preferences: \"I prefer\", \"I always\", \"from now on\", etc.
  - Project facts: \"We decided to use\", \"For this project\", etc.
  - Feedback: \"Your last answer was too verbose; be concise.\"
  - References: URLs and doc/spec references in recent messages.

Explicit user requests like \"please remember\" are marked via
EXPLICIT-USER-REQUEST-P on the candidate."
  (declare (ignore session-memory))
  (let* ((user-pref (%extract-user-preference-candidates conversation subject-id))
         (project-facts (%extract-project-fact-candidates conversation subject-id))
         (feedback (%extract-feedback-candidates conversation subject-id))
         (references (%extract-reference-candidates conversation subject-id)))
    (remove-if #'null (nconc user-pref project-facts feedback references))))

;;; ------------------------------------------------------------
;;; Budget / pruning helpers
;;; ------------------------------------------------------------

(defun %count-records-for-kind-and-subject (kind subject-id)
  "Return the number of durable memory records for KIND and SUBJECT-ID.
Uses LOAD-DURABLE-MEMORIES from the storage backend."
  (length (load-durable-memories kind subject-id)))

(defun %prune-oldest-records (kind subject-id max-records)
  "Prune durable memory records for KIND and SUBJECT-ID to MAX-RECORDS.
This is a simple heuristic that removes the oldest records by
CREATED-UNIVERSAL-TIME when over budget."
  (let* ((records (load-durable-memories kind subject-id))
         (count (length records)))
    (when (> count max-records)
      (let* ((sorted (sort (copy-list records)
                           #'<
                           :key #'durable-memory-record-created-universal-time))
             (to-delete (- count max-records))
             (victims (subseq sorted 0 to-delete)))
        (dolist (rec victims)
          (ignore-errors
            (delete-durable-memory-record
             (durable-memory-record-kind rec)
             (durable-memory-record-subject-id rec)
             (durable-memory-record-id rec))))
        (values (- count to-delete) to-delete)))))

(defun prune-durable-memories-if-needed (record config)
  "Prune durable memories for RECORD's kind/subject if over budget.

Uses DURABLE-MEMORY-MAX-RECORDS-PER-KIND from the durable memory config.
Returns two values: remaining-count and pruned-count (or NIL if no pruning)."
  (let* ((cfg (get-durable-memory-config config))
         (max-records (getf cfg :max-records-per-kind)))
    (when (and max-records (integerp max-records) (> max-records 0))
      (%prune-oldest-records
       (durable-memory-record-kind record)
       (durable-memory-record-subject-id record)
       max-records))))

;;; ------------------------------------------------------------
;;; Logging helper
;;; ------------------------------------------------------------

(defun %log-durable-memory-save (record decision)
  "Log that durable memory RECORD was saved, with DECISION metadata.

DECISION is the plist returned by SHOULD-SAVE-DURABLE-MEMORY-P,
containing scores and reason codes."
  (let ((logger (and (fboundp 'log-debug) #'log-debug)))
    (when logger
      (funcall logger
               :durable-memory/save
               "Saved durable memory record ~A (kind=~A subject=~A importance=~A reason=~A)"
               (durable-memory-record-id record)
               (durable-memory-record-kind record)
               (durable-memory-record-subject-id record)
               (getf decision :importance-score)
               (getf decision :reason-code)))))

;;; ------------------------------------------------------------
;;; Public API: Ingestion from session / conversation
;;; ------------------------------------------------------------

(defun ingest-durable-memory-from-session (conversation session-memory subject-id
                                                        &key (force-p nil) config)
  "Ingest durable memories from CONVERSATION and SESSION-MEMORY.

This function:
  1. Checks whether durable memory is enabled via DURABLE-MEMORY-ENABLED-P.
  2. Extracts candidates via EXTRACT-DURABLE-MEMORY-CANDIDATES.
  3. For each candidate:
     - Calls SHOULD-SAVE-DURABLE-MEMORY-P (from the scoring engine).
     - If accepted (or FORCE-P is T), persists via SAVE-DURABLE-MEMORY-RECORD.
     - Optionally prunes old records if over budget.
     - Logs the save with scores and reason codes.

SUBJECT-ID is the durable memory subject identifier (user/project/etc.).

Returns a list of saved DURABLE-MEMORY-RECORDs."
  (let* ((cfg (get-durable-memory-config config))
         (enabled-p (getf cfg :enabled)))
    (if (not enabled-p)
        '()
        (let ((candidates (extract-durable-memory-candidates
                           conversation session-memory subject-id))
              (saved '()))
          (dolist (cand candidates)
            (multiple-value-bind (save-p reason-code final-score importance anti-score)
                (should-save-durable-memory-p cand)
              (declare (ignore final-score anti-score))
              (when (or save-p force-p)
                (let* ((decision (list :reason-code reason-code
                                       :importance-score (or importance 0.0)))
                       (record (save-durable-memory-record
                                (make-durable-memory-record
                                 :id nil
                                 :kind (durable-memory-candidate-kind cand)
                                 :subject-id (durable-memory-candidate-subject-id cand)
                                 :title (or (getf (durable-memory-candidate-metadata cand)
                                                  :title)
                                            (subseq (durable-memory-candidate-content cand)
                                                    0
                                                    (min 80 (length (durable-memory-candidate-content cand)))))
                                 :content (durable-memory-candidate-content cand)
                                 :source (durable-memory-candidate-source cand)
                                 :created-universal-time (get-universal-time)
                                 :updated-universal-time (get-universal-time)
                                 :importance-score (or importance 0.0)
                                 :staleness-score 0.0
                                 :last-accessed-universal-time 0
                                 :tags (durable-memory-candidate-tags cand)
                                 :version 1
                                 :supersedes-id nil)))
                       ;; prune if needed
                       (ignore-errors
                         (prune-durable-memories-if-needed record config)))
                  (%log-durable-memory-save record decision)
                  (push record saved)))))
          (nreverse saved)))))

;;; End of Phase 6 Task 5


;;; ============================================================
;;; Phase 6 Task 6: Retrieval & Summarization Utilities
;;; ============================================================


;;;; Durable Memory — Retrieval & Summarization Utilities
;;;; Phase 6 Task 6

(in-package :claw-lisp.storage.durable-memory)

;;; ------------------------------------------------------------
;;; Utility: Kind → Section Title Mapping
;;; ------------------------------------------------------------

(defparameter *durable-memory-kind-section-titles*
  '((:user . "User Preferences")
    (:project . "Project Facts")
    (:feedback . "Feedback")
    (:reference . "References"))
  "Mapping from durable memory kind to section title for summarization.")

(defun durable-memory-kind-section-title (kind)
  "Return the human-readable section title for a durable memory KIND."
  (or (cdr (assoc kind *durable-memory-kind-section-titles*))
      (string-capitalize (symbol-name kind))))

;;; ------------------------------------------------------------
;;; Ranking Utilities
;;; ------------------------------------------------------------

(defun durable-memory-recency-score (record)
  "Compute a normalized recency score (0..1) for RECORD based on updated-universal-time.
  Newer records are closer to 1."
  (let* ((now (get-universal-time))
         (updated (or (slot-value record 'updated-universal-time)
                      (slot-value record 'created-universal-time)
                      0))
         ;; For normalization, assume anything within 30 days is 'fresh'
         (thirty-days (* 60 60 24 30))
         (age (max 0 (- now updated))))
    (max 0 (min 1 (- 1.0 (/ (float age) thirty-days))))))

(defun durable-memory-access-frequency-score (record)
  "Compute a normalized access frequency score (0..1) for RECORD.
  If not tracked, default to 0.5."
  (let ((freq (ignore-errors (slot-value record 'access-frequency))))
    (if (and freq (numberp freq))
        (min 1.0 (/ (float freq) 10.0)) ;; assume 10+ is max
        0.5)))

(defun durable-memory-importance-score (record)
  "Return the normalized importance score (0..1) for RECORD."
  (let ((score (slot-value record 'importance-score)))
    (if (and score (numberp score))
        (if (> score 1.0) (/ (float score) 100.0) (float score))
        0.0)))

(defun rank-durable-memories (records)
  "Return RECORDS sorted by weighted ranking:
  importance (0.5), recency (0.3), access-frequency (0.2)."
  (sort (copy-list records)
        #'>
        :key (lambda (rec)
               (+ (* 0.5 (durable-memory-importance-score rec))
                  (* 0.3 (durable-memory-recency-score rec))
                  (* 0.2 (durable-memory-access-frequency-score rec))))))

;;; ------------------------------------------------------------
;;; Retrieval Functions
;;; ------------------------------------------------------------

(defun get-durable-memories-for-user (user-id &key (kinds '(:user :feedback :reference :project)) (limit 20))
  "Retrieve durable memory records for USER-ID, filtered by KINDS and limited to LIMIT.
  Returns a ranked list of durable-memory-record structs."
  (let ((all-records (loop for kind in kinds
                           append (ignore-errors (load-durable-memories kind user-id)))))
    (rank-durable-memories
     (subseq all-records 0 (min (length all-records) limit)))))

(defun get-durable-memories-for-project (project-id &key (kinds '(:project :reference :feedback :user)) (limit 20))
  "Retrieve durable memory records for PROJECT-ID, filtered by KINDS and limited to LIMIT.
  Returns a ranked list of durable-memory-record structs."
  (let ((all-records (loop for kind in kinds
                           append (ignore-errors (load-durable-memories kind project-id)))))
    (rank-durable-memories
     (subseq all-records 0 (min (length all-records) limit)))))

(defun durable-memory-record-matches-query-p (record query)
  "Return T if RECORD matches QUERY (case-insensitive substring in content/title/tags)."
  (let ((q (string-downcase query)))
    (or (search q (string-downcase (or (slot-value record 'title) "")))
        (search q (string-downcase (or (slot-value record 'content) "")))
        (some (lambda (tag)
                (search q (string-downcase (princ-to-string tag))))
              (or (slot-value record 'tags) '())))))

(defun search-durable-memories (query &key (kinds '(:user :project :feedback :reference)) subject-id (limit 10))
  "Search durable memories for QUERY, optionally filtered by KINDS and SUBJECT-ID.
  Returns a ranked list of durable-memory-record structs."
  (let ((records
          (cond
            (subject-id
             (loop for kind in kinds
                   append (ignore-errors (load-durable-memories kind subject-id))))
            (t
             ;; If no subject-id, search all available memories (expensive!).
             ;; For now, return empty list.
             '()))))
    (let ((matches (remove-if-not (lambda (rec)
                                    (durable-memory-record-matches-query-p rec query))
                                  records)))
      (rank-durable-memories
       (subseq matches 0 (min (length matches) limit))))))

;;; ------------------------------------------------------------
;;; Summarization Utilities
;;; ------------------------------------------------------------

(defun durable-memory-record-summary-line (record)
  "Produce a one-line summary for RECORD for use in markdown lists."
  (let ((title (or (slot-value record 'title) ""))
        (content (or (slot-value record 'content) "")))
    (cond
      ((and title (plusp (length (string-trim " " title))))
       (format nil "- ~A" title))
      ((and content (plusp (length (string-trim " " content))))
       (format nil "- ~A" (subseq content 0 (min 80 (length content)))))
      (t
       "- [No summary available]"))))

(defun group-durable-memories-by-kind (records)
  "Group durable memory RECORDS by their kind. Returns an alist (kind . list-of-records)."
  (let ((table (make-hash-table)))
    (dolist (rec records)
      (let ((kind (slot-value rec 'kind)))
        (push rec (gethash kind table))))
    (let (result)
      (maphash (lambda (kind recs)
                 (push (cons kind (nreverse recs)) result))
               table)
      (nreverse result))))

(defun summarize-durable-memories (records &key (max-chars 1000))
  "Produce a concise, structured markdown summary of durable memory RECORDS.
  Groups by kind, respects MAX-CHARS budget."
  (let* ((grouped (group-durable-memories-by-kind records))
         (sections
           (loop for (kind . recs) in grouped
                 for section-title = (durable-memory-kind-section-title kind)
                 for lines = (mapcar #'durable-memory-record-summary-line recs)
                 collect (cons section-title lines))))
    ;; Compose markdown, truncating as needed to fit max-chars
    (let ((output (with-output-to-string (s)
                    (format s "## Durable Memory Context~%~%")
                    (dolist (section sections)
                      (destructuring-bind (title . lines) section
                        (format s "### ~A~%" title)
                        (dolist (line lines)
                          (format s "~A~%" line))
                        (format s "~%"))))))
      (if (<= (length output) max-chars)
          output
          ;; Truncate: remove lines from the end until within budget
          (let* ((lines (split-sequence:split-sequence #\Newline output))
                 (header (first lines))
                 (rest-lines (rest lines))
                 (result-lines (list header))
                 (total-len (length header)))
            (dolist (line rest-lines)
              (when (< (+ total-len (length line) 1) max-chars)
                (push line result-lines)
                (incf total-len (+ (length line) 1))))
            (with-output-to-string (s)
              (dolist (line (nreverse result-lines))
                (format s "~A~%" line))
              (format s "...~%")))))))

;;; ------------------------------------------------------------
;;; Integration Hook: Render Durable Memory Context
;;; ------------------------------------------------------------

(defun render-durable-memory-context (user-id project-id &key (max-chars 1000))
  "Generate a markdown Durable Memory Context section for USER-ID and PROJECT-ID.
  Includes top-ranked memories for both, merged and summarized to fit MAX-CHARS."
  (let* ((user-records (get-durable-memories-for-user user-id :limit 10))
         (project-records (get-durable-memories-for-project project-id :limit 10))
         (all-records (rank-durable-memories (append user-records project-records))))
    (summarize-durable-memories all-records :max-chars max-chars)))

;;; ------------------------------------------------------------
;;; End of Retrieval & Summarization Utilities
;;; ------------------------------------------------------------

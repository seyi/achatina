(in-package #:claw-lisp.config)

;;; ============================================================
;;; Global Runtime Configuration Variable
;;; ============================================================

(defparameter *runtime-config* nil
  "Global dynamic variable holding the current runtime configuration.
   Bind this variable dynamically when you need to override configuration
   for a specific scope, or set it at startup after calling LOAD-RUNTIME-CONFIG.")

;;; ============================================================
;;; Step 1: Credential structs + redact-key helper
;;; ============================================================

(defun redact-key (key)
  "Return a redacted version of KEY for safe logging."
  (cond
    ((or (null key) (<= (length key) 8)) "***REDACTED***")
    (t (format nil "~A...~A" (subseq key 0 4) (subseq key (- (length key) 4))))))

(defstruct provider-credentials
  "Base fields shared by all API-key-based providers."
  (api-key nil :type (or null string))
  (base-url nil :type (or null string)))

(defstruct (anthropic-credentials (:include provider-credentials))
  "Anthropic-specific credentials."
  (api-version "2023-06-01" :type string))

(defstruct (openrouter-credentials (:include provider-credentials)))

(defstruct (openai-credentials (:include provider-credentials))
  (organization nil :type (or null string)))

(defstruct (bedrock-credentials (:constructor %make-bedrock-credentials))
  "AWS Bedrock credentials — not API-key-based."
  (access-key nil :type (or null string))
  (secret-key nil :type (or null string))
  (region "us-east-1" :type string)
  (profile nil :type (or null string)))

;;; ============================================================
;;; Phase 7 Task 6 — Durable Memory Query Configuration
;;; ============================================================

(defstruct (durable-memory-query-config
            (:conc-name dmq-config-))
  "Configuration for durable memory query and context injection.
   
   This sub-config struct consolidates all semantic memory retrieval
   settings into a single composable unit. Use via:
   
     (runtime-config-durable-memory-query-config config)
   
   Or for REPL convenience:
   
     (current-dmq-config session)
   
   Fields:
     max-results             — Maximum memories to retrieve per query
     min-relevance-score     — Minimum score threshold (0.0-1.0)
     default-query-mode      — :semantic, :lexical, or :hybrid
     semantic-weight-by-kind — Alist of per-kind score weights
     max-injection-chars     — Budget for injected context text
     injection-enabled       — Master switch for auto-injection
     dedup-window-normal     — Turns for normal memory dedup
     dedup-window-important  — Turns for high-importance dedup
     importance-threshold    — Score threshold for 'important' memories
     evergreen-kinds         — Kinds injected at most once per session
     embedding-failure-threshold — Consecutive failures before circuit open
     embedding-cooldown-seconds  — Seconds before retry after circuit open"
  ;; Query parameters
  (max-results 5 :type fixnum)
  (min-relevance-score 0.3s0 :type single-float)
  (default-query-mode :hybrid :type keyword)
  
  ;; Score blending (per-kind alist, falls back to :default)
  (semantic-weight-by-kind '((:user . 0.8s0)
                             (:project . 0.65s0)
                             (:reference . 0.4s0)
                             (:default . 0.7s0))
                           :type list)
  
  ;; Context injection
  (max-injection-chars 1000 :type fixnum)
  (injection-enabled t :type boolean)
  
  ;; Deduplication
  (dedup-window-normal 5 :type fixnum)
  (dedup-window-important 20 :type fixnum)
  (importance-threshold 0.85s0 :type single-float)
  (evergreen-kinds '(:project) :type list)
  
  ;; Circuit breaker for embedding provider
  (embedding-failure-threshold 3 :type fixnum)
  (embedding-cooldown-seconds 120 :type fixnum))

(defun make-default-dmq-config ()
  "Create a durable-memory-query-config with production defaults.
   
   This is the canonical source for default values. Use this when
   constructing a runtime-config or for testing."
  (make-durable-memory-query-config))

;;; Dynamic variable for REPL/debugging convenience
;;; This is NOT the source of truth — it's for let-binding during testing.

(defvar *dmq-active-config* nil
  "The currently active durable-memory-query-config.
   
   Set by session initialization. Can be let-bound for testing.
   Do not modify directly in production code.
   
   Resolution order for (current-dmq-config):
     1. *dmq-active-config* (if bound)
     2. Session's runtime-config slot
     3. Default from make-default-dmq-config")

(defun current-dmq-config (&optional session)
  "Return the active durable-memory-query-config.
   
   Resolution order:
     1. *dmq-active-config* (if bound)
     2. Session's runtime-config slot
     3. Default from make-default-dmq-config
   
   INPUTS:
     SESSION — optional session struct. If provided, used for lookup.
   
   OUTPUT:
     A durable-memory-query-config struct.
   
   USAGE:
     ;; In runtime code:
     (let ((config (current-dmq-config session)))
       ...)
     
     ;; In tests:
     (let ((*dmq-active-config* (make-durable-memory-query-config
                                  :max-results 2)))
       ...))"
  (or *dmq-active-config*
      (when session
        (getf (claw-lisp.core.domain:agent-session-state session) :dmq-config))
      (make-default-dmq-config)))

;;; ============================================================
;;; Step 2: Refactor runtime-config
;;; ============================================================

(defstruct (runtime-config
            (:constructor %make-runtime-config))
  (name "claw-lisp" :type string)
  (data-root ".claw-lisp/" :type string)
  (transcripts-root ".claw-lisp/transcripts/" :type string)
  (artifacts-root ".claw-lisp/artifacts/" :type string)
  (memory-root ".claw-lisp/memory/" :type string)
  (cas-objects-root ".claw-lisp/cas/objects/" :type string)
  (cas-ref-root ".claw-lisp/cas/refs/" :type string)
  (tool-result-dedup-p t :type boolean)
  (default-provider "anthropic" :type string)
  (default-model "claude-sonnet-4-6" :type string)
  (tool-preview-bytes 2048 :type integer)
  (tool-result-aggregate-budget-bytes 8192 :type integer)
  (tool-allowed-roots nil :type list)
  (shell-command-enabled-p t :type boolean)
  (shell-command-timeout-seconds 5 :type integer)
  (microcompact-keep-recent-tool-results 1 :type integer)
  (post-compact-keep-recent-tool-results 2 :type integer)
  (context-output-reserve 2048 :type integer)
  (token-warning-buffer 4096 :type integer)
  (compaction-trigger-buffer 1024 :type integer)
  ;; ============================================================
  ;; Phase 4: Context Management Thresholds
  ;; ============================================================
  ;; Three-tier threshold system for proactive context management
  ;; context-warning-threshold (0.75) — Trigger microcompact at 75% usage
  ;; context-compact-suggested-threshold (0.85) — Aggressive microcompact at 85%
  ;; context-compact-required-threshold (0.95) — Full compaction at 95%
  (context-warning-threshold 0.75 :type single-float)
  (context-compact-suggested-threshold 0.85 :type single-float)
  (context-compact-required-threshold 0.95 :type single-float)
  ;; ============================================================
  ;; Phase 4: Idle-Gap Trigger Settings
  ;; ============================================================
  ;; Automatic cleanup during natural pauses in conversation
  ;; idle-gap-microcompact-seconds (120) — Seconds of inactivity before trigger
  ;; idle-gap-minimum-usage-ratio (0.50) — Minimum usage ratio for idle-gap
  (idle-gap-microcompact-seconds 120 :type (integer 0))
  (idle-gap-minimum-usage-ratio 0.50 :type single-float)
  ;; ============================================================
  ;; Phase 4: Reactive Compaction Settings
  ;; ============================================================
  ;; Behavior when 413 context-exceeded error occurs
  ;; reactive-compaction-max-retries (1) — Max retry attempts after compaction
  ;; reactive-compaction-aggressive-keep-recent (2) — Tool results to preserve
  (reactive-compaction-max-retries 1 :type (integer 0))
  (reactive-compaction-aggressive-keep-recent 2 :type (integer 1))
  ;; ============================================================
  ;; Phase 5: Session Memory Settings
  ;; ============================================================
  ;; Session working memory configuration
  ;; session-memory-budget-chars (4096) — Maximum session memory size
  ;; session-memory-update-token-growth-threshold (2000) — Trigger update after N new tokens
  ;; session-memory-update-tool-activity-threshold (3) — Trigger update after N tool calls
  ;; session-memory-idle-update-seconds (30) — Trigger update after idle gap
  ;; session-memory-max-staleness-seconds (300) — Mark stale after N seconds
  (session-memory-budget-chars 4096 :type integer)
  (session-memory-update-token-growth-threshold 2000 :type integer)
  (session-memory-update-tool-activity-threshold 3 :type integer)
  (session-memory-idle-update-seconds 30 :type (integer 0))
  (session-memory-max-staleness-seconds 300 :type (integer 0))
  ;; ============================================================
  ;; Phase 6: Durable Memory Settings
  ;; ============================================================
  ;; Durable long-term memory configuration
  ;; durable-memory-enabled-p (T) — Enable/disable durable memory subsystem
  ;; durable-user-memory-budget-chars (2048) — Budget for user memory
  ;; durable-feedback-memory-budget-chars (1024) — Budget for feedback memory
  ;; durable-project-memory-budget-chars (4096) — Budget for project memory
  ;; durable-reference-memory-budget-chars (2048) — Budget for reference memory
  ;; durable-memory-max-records-per-kind (50) — Max records per kind
  ;; durable-memory-max-record-age-days (90) — Soft age limit in days
  (durable-memory-enabled-p t :type boolean)
  (durable-user-memory-budget-chars 2048 :type integer)
  (durable-feedback-memory-budget-chars 1024 :type integer)
  (durable-project-memory-budget-chars 4096 :type integer)
  (durable-reference-memory-budget-chars 2048 :type integer)
  (durable-memory-max-records-per-kind 50 :type integer)
  (durable-memory-max-record-age-days 90 :type integer)
  ;; Durable memory orchestration options (Task 7)
  (durable-memory-include-in-prompts-p t :type boolean)
  (durable-memory-max-context-chars 1000 :type integer)
  ;; Phase 7: Embedding Configuration
  (embedding-enabled-p nil :type boolean)
  (embedding-provider :openai :type keyword)
  (embedding-model "text-embedding-3-small" :type string)
  (embedding-max-batch-size 100 :type (integer 1 *))
  (embedding-timeout-seconds 30 :type (integer 1 *))
  (embedding-log-errors-p t :type boolean)
  (embedding-default-dimension nil :type (or null (integer 1 *)))
  (embedding-max-text-length 8192 :type (integer 1 *))
  ;; Phase 7: Semantic Search Configuration
  (semantic-search-enabled-p t :type boolean)
  (semantic-search-default-limit 10 :type (integer 1 *))
  (semantic-search-max-candidates 500 :type (integer 1 *))
  (semantic-search-min-score 0.0 :type single-float)
  (semantic-search-hybrid-weight 0.7 :type single-float)
  ;; Phase 7 Task 6: Durable Memory Query Configuration
  (durable-memory-query-config (make-default-dmq-config)
   :type durable-memory-query-config
   :read-only t)
  ;; ============================================================
  ;; Phase 10: Compaction IR Settings
  ;; ============================================================
  ;; compaction-summary-token-budget (NIL) — Maximum tokens for rendered summary.
  ;; NIL means no limit. When set (e.g. 2000), low-priority sections are trimmed.
  (compaction-summary-token-budget nil)
  ;; Provider credentials keyed by provider name keyword
  (provider-credentials (make-hash-table :test 'eq) :type hash-table))

(defun config-credentials (config provider-name)
  "Retrieve credentials for PROVIDER-NAME (a keyword like :anthropic)."
  (gethash provider-name (runtime-config-provider-credentials config)))

(defun (setf config-credentials) (creds config provider-name)
  (setf (gethash provider-name (runtime-config-provider-credentials config)) creds))

;;; ============================================================
;;; Step 3: Configuration loading
;;; ============================================================

(defun read-config-file (path)
  "Read configuration from PATH. Signals config-error on failure."
  (handler-case
      (let ((*read-eval* nil)  ; Prevent #. reader macros
            (*package* (find-package :keyword)))
        (with-open-file (stream path :direction :input)
          (read stream)))
    (error (e)
      (error 'claw-lisp.core.conditions:config-error
             :path path
             :message (format nil "Failed to read config file: ~A" e)))))

(defun apply-provider-config-from-plist (config provider-key plist)
  "Create and store credentials for PROVIDER-KEY from PLIST."
  (case provider-key
    (:anthropic
     (setf (config-credentials config provider-key)
           (make-anthropic-credentials
            :api-key (getf plist :api-key)
            :base-url (getf plist :base-url)
            :api-version (getf plist :api-version "2023-06-01"))))
    (:openrouter
     (setf (config-credentials config provider-key)
           (make-openrouter-credentials
            :api-key (getf plist :api-key)
            :base-url (getf plist :base-url))))
    (:openai
     (setf (config-credentials config provider-key)
           (make-openai-credentials
            :api-key (getf plist :api-key)
            :base-url (getf plist :base-url)
            :organization (getf plist :organization))))
    (:bedrock
     (setf (config-credentials config provider-key)
           (%make-bedrock-credentials
            :access-key (getf plist :access-key)
            :secret-key (getf plist :secret-key)
            :region (or (getf plist :region) "us-east-1")
            :profile (getf plist :profile))))))

(defun apply-config-file (config path)
  "Apply configuration from PATH to CONFIG."
  (let ((data (read-config-file path)))
    (unless (and (listp data) (evenp (length data)))
      (error 'claw-lisp.core.conditions:config-error :path path :message "Config must be a plist"))
    
    ;; Apply global settings
    (let ((val (getf data :default-provider :not-present)))
      (unless (eq val :not-present)
        (setf (runtime-config-default-provider config) val)))
    (let ((val (getf data :default-model :not-present)))
      (unless (eq val :not-present)
        (setf (runtime-config-default-model config) val)))
    (let ((val (getf data :tool-allowed-roots :not-present)))
      (unless (eq val :not-present)
        (setf (runtime-config-tool-allowed-roots config) val)))
    (let ((val (getf data :cas-objects-root :not-present)))
      (unless (eq val :not-present)
        (setf (runtime-config-cas-objects-root config) val)))
    (let ((val (getf data :cas-ref-root :not-present)))
      (unless (eq val :not-present)
        (setf (runtime-config-cas-ref-root config) val)))
    (let ((val (getf data :tool-result-dedup-p :not-present)))
      (unless (eq val :not-present)
        (setf (runtime-config-tool-result-dedup-p config) val)))
    (let ((val (getf data :shell-command-enabled-p :not-present)))
      (unless (eq val :not-present)
        (setf (runtime-config-shell-command-enabled-p config) val)))
    
    ;; Apply provider credentials
    (dolist (provider '(:anthropic :openrouter :openai :bedrock))
      (let ((provider-plist (getf data provider)))
        (when provider-plist
          (apply-provider-config-from-plist config provider provider-plist))))
    config))

(defun load-provider-credentials-from-env (config provider-key)
  "Load credentials for PROVIDER-KEY from environment variables."
  (case provider-key
    (:anthropic
     (let ((key (uiop:getenv "ANTHROPIC_API_KEY"))
           (url (uiop:getenv "ANTHROPIC_BASE_URL")))
       (when key
         (setf (config-credentials config provider-key)
               (make-anthropic-credentials
                :api-key key
                :base-url (or url "https://api.anthropic.com/v1/messages"))))))
    (:openrouter
     (let ((key (uiop:getenv "OPENROUTER_API_KEY"))
           (url (uiop:getenv "OPENROUTER_BASE_URL")))
       (when key
         (setf (config-credentials config provider-key)
               (make-openrouter-credentials
                :api-key key
                :base-url (or url "https://openrouter.ai/api/v1/chat/completions"))))))
    (:openai
     (let ((key (uiop:getenv "OPENAI_API_KEY"))
           (url (uiop:getenv "OPENAI_BASE_URL"))
           (org (uiop:getenv "OPENAI_ORGANIZATION")))
       (when key
         (setf (config-credentials config provider-key)
               (make-openai-credentials
                :api-key key
                :base-url (or url "https://api.openai.com/v1/chat/completions")
                :organization org)))))
    (:bedrock
     (let ((ak (uiop:getenv "AWS_ACCESS_KEY_ID"))
           (sk (uiop:getenv "AWS_SECRET_ACCESS_KEY"))
           (region (uiop:getenv "AWS_REGION")))
       (when (or ak sk)
         (setf (config-credentials config provider-key)
               (%make-bedrock-credentials
                :access-key ak
                :secret-key sk
                :region (or region "us-east-1")))))))
  config)

(defun apply-environment-config (config)
  "Apply environment variables to CONFIG."
  ;; Global settings
  (let ((provider (uiop:getenv "CLAW_DEFAULT_PROVIDER")))
    (when provider
      (setf (runtime-config-default-provider config) provider)))
  (let ((model (uiop:getenv "CLAW_DEFAULT_MODEL")))
    (when model
      (setf (runtime-config-default-model config) model)))
  
  ;; Provider credentials
  (dolist (provider '(:anthropic :openrouter :openai :bedrock))
    (load-provider-credentials-from-env config provider))
  config)

(defun find-config-file ()
  "Find configuration file in standard locations.
   Project-local config deferred to Phase 3 (security review needed)."
  (or (let ((env-path (uiop:getenv "CLAW_CONFIG_FILE")))
        (when (and env-path (uiop:file-exists-p env-path))
          (pathname env-path)))
      ;; Project-local intentionally omitted — see Phase 3 security review
      (let ((user-path (merge-pathnames "claw-lisp/config.lisp"
                                         (uiop:xdg-config-home))))
        (when (uiop:file-exists-p user-path) user-path))))

(defun check-config-file-permissions (path)
  "Warn if config file at PATH has overly permissive permissions.
   Uses POSIX stat via SBCL extension or skips check on unsupported platforms."
  #+sbcl
  (handler-case
      (let ((mode (sb-posix:stat-mode (sb-posix:stat path))))
        ;; Check for world-readable (o+r = #o004) or group-readable (#o040)
        (when (plusp (logand mode #o044))
          (warn "Configuration file ~A is readable by other users (mode ~4,'0O). ~
                 Consider: chmod 600 ~A" path mode path)))
    (error () nil))  ; Silently skip if sb-posix unavailable
  #-sbcl
  nil)

(defun validate-config (config)
  "Validate configuration, signaling config-error for critical issues."
  ;; Check file permissions if config file exists
  (let ((path (find-config-file)))
    (when path
      (check-config-file-permissions path)))

  ;; Validate provider credentials structure
  (maphash (lambda (provider creds)
             (unless (or (null creds)
                         (typep creds 'provider-credentials)
                         (typep creds 'bedrock-credentials))
               (error 'claw-lisp.core.conditions:config-error
                      :message (format nil "Invalid credentials type for ~A" provider))))
           (runtime-config-provider-credentials config))
  config)

(defun load-runtime-config (&key (overrides nil) (config-file nil))
  "Load runtime configuration with standard priority:
   OVERRIDES > env vars > config file > defaults"
  (let ((config (%make-runtime-config)))
    ;; 1. Apply defaults (struct initforms)
    ;; 2. Apply config file if exists
    (let ((path (or config-file (find-config-file))))
      (when path
        (apply-config-file config path)))
    ;; 3. Apply environment variables (overrides file)
    (apply-environment-config config)
    ;; 4. Apply runtime overrides
    (when overrides
      (apply-overrides config overrides))
    ;; 5. Validate
    (validate-config config)
    config))

(defun apply-overrides (config overrides)
  "Apply OVERRIDES plist to CONFIG.
   Keys are keywords matching runtime-config slot names."
  (loop for (key value) on overrides by #'cddr
        do (case key
             (:default-provider
              (setf (runtime-config-default-provider config) value))
             (:default-model
              (setf (runtime-config-default-model config) value))
             (:tool-allowed-roots
              (setf (runtime-config-tool-allowed-roots config) value))
             (:cas-objects-root
              (setf (runtime-config-cas-objects-root config) value))
             (:cas-ref-root
              (setf (runtime-config-cas-ref-root config) value))
             (:tool-result-dedup-p
              (setf (runtime-config-tool-result-dedup-p config) value))
             (:shell-command-enabled-p
              (setf (runtime-config-shell-command-enabled-p config) value))
             (:shell-command-timeout-seconds
              (setf (runtime-config-shell-command-timeout-seconds config) value))
             (:data-root
              (setf (runtime-config-data-root config) value))
             (otherwise
              (warn "Unknown config override key: ~S" key)))))

;;; Backward compatibility
(defun make-default-runtime-config ()
  "Return the baseline runtime configuration.
   Deprecated: Use LOAD-RUNTIME-CONFIG instead."
  (load-runtime-config))

;;; ============================================================
;;; Phase 7: Embedding Configuration Accessors
;;; ============================================================
;;; Dynamic variable bindings for "current" embedding settings.
;;; These allow runtime rebinding without passing *runtime-config* everywhere.

(defparameter *current-embedding-provider* nil
  "Dynamically bound current embedding provider keyword.
   When NIL, falls back to (runtime-config-embedding-provider *runtime-config*).")

(defparameter *current-embedding-model* nil
  "Dynamically bound current embedding model name.
   When NIL, falls back to (runtime-config-embedding-model *runtime-config*).")

(defparameter *current-embedding-max-batch-size* nil
  "Dynamically bound current embedding batch size.
   When NIL, falls back to (runtime-config-embedding-max-batch-size *runtime-config*).")

(defparameter *current-embedding-enabled-p* nil
  "Dynamically bound current embedding enabled flag.
   When NIL, falls back to (runtime-config-embedding-enabled-p *runtime-config*).")

(defun current-embedding-provider ()
  "Return the current embedding provider keyword.
   Uses dynamic binding if available, otherwise falls back to *runtime-config*."
  (or *current-embedding-provider*
      (runtime-config-embedding-provider *runtime-config*)))

(defun current-embedding-model ()
  "Return the current embedding model name."
  (or *current-embedding-model*
      (runtime-config-embedding-model *runtime-config*)))

(defun current-embedding-max-batch-size ()
  "Return the current embedding max batch size."
  (or *current-embedding-max-batch-size*
      (runtime-config-embedding-max-batch-size *runtime-config*)))

(defun current-embedding-enabled-p ()
  "Return T if embeddings are currently enabled."
  (if (null *current-embedding-enabled-p*)
      (runtime-config-embedding-enabled-p *runtime-config*)
      *current-embedding-enabled-p*))

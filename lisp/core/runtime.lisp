(in-package #:claw-lisp.core.runtime)

(defstruct (runtime
            (:constructor %make-runtime
                (&key
                 (config (make-default-runtime-config))
                 (provider-registry (make-hash-table :test 'equal))
                 (tool-registry (make-hash-table :test 'equal))
                 (model-registry (claw-lisp.core.model-registry:make-default-model-registry))
                 (selection-lock-table (make-hash-table :test 'equal))
                 (selection-lock-guard
                  #+sb-thread (sb-thread:make-mutex :name "selection-lock-guard")
                  #-sb-thread nil)
                 (tool-result-dedup-index (make-hash-table :test 'equal))
                 (tool-result-dedup-index-head nil)
                 (tool-result-dedup-index-tail nil)
                 (project-root nil))))
  ;; NOTE: The baseline runtime assumes single-threaded registry access.
  ;; Add locking before relying on concurrent provider or tool registration.
  (settings config)
  provider-registry
  tool-registry
  model-registry
  selection-lock-table
  selection-lock-guard
  tool-result-dedup-index
  tool-result-dedup-index-head
  tool-result-dedup-index-tail
  ;; Project root for CLAUDE.md discovery and git context.
  ;; Defaults to current working directory when nil.
  (project-root nil))

(defun make-runtime (&key config provider-registry tool-registry model-registry project-root)
  "Create a runtime with optional model-registry and project-root overrides."
  (%make-runtime
   :config (or config (make-default-runtime-config))
   :provider-registry (or provider-registry (make-hash-table :test 'equal))
   :tool-registry (or tool-registry (make-hash-table :test 'equal))
   :model-registry (or model-registry
                       (claw-lisp.core.model-registry:make-default-model-registry))
   :selection-lock-table (make-hash-table :test 'equal)
   :selection-lock-guard #+sb-thread (sb-thread:make-mutex :name "selection-lock-guard")
                         #-sb-thread nil
   :project-root project-root))

(defun %session-selection-lock (runtime session)
  "Return a per-session mutex for selection updates."
  #+sb-thread
  (let ((session-id (claw-lisp.core.domain:agent-session-id session)))
    (sb-thread:with-mutex ((runtime-selection-lock-guard runtime))
      (or (gethash session-id (runtime-selection-lock-table runtime))
          (setf (gethash session-id (runtime-selection-lock-table runtime))
                (sb-thread:make-mutex
                 :name (format nil "selection-lock-~A" session-id))))))
  #-sb-thread
  (declare (ignore runtime session))
  #-sb-thread
  nil)

(defun runtime-effective-project-root (runtime)
  "Return the project root for CLAUDE.md discovery.
   Uses the runtime's project-root if set, otherwise defaults to CWD."
  (or (runtime-project-root runtime)
      (uiop:getcwd)))

(defparameter +compaction-failure-limit+ 3
  "Maximum consecutive compaction failures before the baseline circuit opens.")

(defparameter +max-provider-tool-iterations+ 4
  "Bound the baseline provider tool loop to avoid infinite local retries.")

(defun register-tool (runtime tool)
  "Register TOOL under its declared name and return RUNTIME."
  (setf (gethash (claw-lisp.core.protocols:tool-name tool)
                 (runtime-tool-registry runtime))
        tool)
  runtime)

(defun session-transcript-path (runtime session)
  "Return the transcript path for SESSION under RUNTIME configuration."
  (transcript-path-for-session (runtime-settings runtime)
                               (claw-lisp.core.domain:agent-session-id session)))

(defun session-transcript-existing-path (runtime session)
  "Return the existing transcript path for SESSION, with legacy fallback."
  (claw-lisp.storage.transcripts:transcript-existing-path-for-session
   (runtime-settings runtime)
   (claw-lisp.core.domain:agent-session-id session)))

(defun transcript-existing-path-for-session-id (runtime session-id)
  "Return the existing transcript path for SESSION-ID, with legacy fallback."
  (claw-lisp.storage.transcripts:transcript-existing-path-for-session
   (runtime-settings runtime)
   session-id))

(defun session-memory-path-for-session (runtime session)
  "Return the session-memory path for SESSION under RUNTIME configuration."
  (session-memory-path (runtime-settings runtime)
                       (claw-lisp.core.domain:agent-session-id session)))

(defun durable-memory-path-for-session (runtime session)
  "Return the durable-memory note path for SESSION under RUNTIME configuration."
  (durable-memory-note-path (runtime-settings runtime)
                            (claw-lisp.core.domain:agent-session-id session)))

;; NOTE: compact-session-with-session-memory, compact-session, and
;; try-compact-session are defined below (search "Phase 5 Task 7:
;; Compaction Integration"). The earlier shadowed copies were removed.

(defun apply-session-compaction (runtime session result)
  "Apply RESULT to SESSION and record the compaction boundary in the transcript."
  (let* ((config (runtime-settings runtime))
         (conversation (apply-compaction-result config session result))
         (restored-results
           (claw-lisp.core.domain:conversation-tool-results
            (claw-lisp.core.domain:agent-session-conversation session)))
         (ir (claw-lisp.core.domain:compaction-result-ir result))
         (transcript-path (session-transcript-path runtime session)))
    ;; Store provenance chain state on session
    (when ir
      (set-session-state-value
       session :last-compaction-fingerprint
       (claw-lisp.core.compact:compaction-ir-fingerprint ir))
      (set-session-state-value
       session :compaction-depth
       (1+ (or (session-state-value session :compaction-depth) 0))))
    (maybe-append-transcript-event
     session
     transcript-path
     (list :event "compaction_boundary"
           :session_id (claw-lisp.core.domain:agent-session-id session)
           :source (claw-lisp.core.domain:compaction-result-source result)
           :preserved_count
           (length (claw-lisp.core.domain:compaction-result-preserved-messages result))
           :restored_tool_results_count
           (length restored-results)
           :ir (when ir
                 (claw-lisp.core.compact:compaction-ir-to-plist ir))))
    (update-session-memory config session)
    conversation))

(defun session-state-value (session key &optional default)
  "Return KEY from SESSION state plist, or DEFAULT when absent."
  (getf (claw-lisp.core.domain:agent-session-state session) key default))

(defun set-session-state-value (session key value)
  "Set KEY to VALUE in SESSION state plist and return SESSION."
  (let ((state (copy-list (claw-lisp.core.domain:agent-session-state session))))
    (setf (getf state key) value)
    (setf (claw-lisp.core.domain:agent-session-state session) state)
    session))

(defun session-runtime-event-callback (session)
  "Return the optional runtime event callback for SESSION."
  (session-state-value session :runtime-event-callback))

(defun call-session-runtime-event-callback (session event)
  "Best-effort notify the runtime event callback for SESSION with EVENT."
  (let ((callback (session-runtime-event-callback session)))
    (when (functionp callback)
      (handler-case
          (funcall callback event)
        (error (condition)
          (warn "Runtime event callback failed for session ~A on event ~S: ~A"
                (claw-lisp.core.domain:agent-session-id session)
                event
                condition)))))
  nil)

(defun session-supervisor-state (session)
  "Return the Phase 8 supervisor state from SESSION, or NIL."
  (session-state-value session :agent-supervisor-state))

(defun set-session-supervisor-state (session supervisor-state)
  "Set the Phase 8 supervisor state on SESSION and return SESSION."
  (set-session-state-value session :agent-supervisor-state supervisor-state))

(defun %public-child-agent-unavailable (&optional detail)
  "Signal that child-agent orchestration depth is not included in the public build."
  (error "Child-agent orchestration is not included in the public Achatina build~@[: ~A~]."
         detail))

(defun spawn-child-agent (runtime session &rest args &key &allow-other-keys)
  "Signal that child-agent orchestration is unavailable in the public build."
  (declare (ignore runtime session args))
  (%public-child-agent-unavailable "spawn-child-agent"))

(defun send-agent-message (runtime session child-id type payload &key correlation-id timeout-seconds)
  "Signal that child-agent orchestration is unavailable in the public build."
  (declare (ignore runtime session child-id type payload correlation-id timeout-seconds))
  (%public-child-agent-unavailable "send-agent-message"))

(defun receive-agent-message (runtime session &key timeout-seconds)
  "Signal that child-agent orchestration is unavailable in the public build."
  (declare (ignore runtime session timeout-seconds))
  (%public-child-agent-unavailable "receive-agent-message"))

(defun await-child-agent (runtime session child-id &key timeout-seconds)
  "Signal that child-agent orchestration is unavailable in the public build."
  (declare (ignore runtime session child-id timeout-seconds))
  (%public-child-agent-unavailable "await-child-agent"))

(defun list-child-agents (runtime session)
  "Signal that child-agent orchestration is unavailable in the public build."
  (declare (ignore runtime session))
  (%public-child-agent-unavailable "list-child-agents"))

(defun child-progress-snapshot (runtime session child-id)
  "Signal that child-agent orchestration is unavailable in the public build."
  (declare (ignore runtime session child-id))
  (%public-child-agent-unavailable "child-progress-snapshot"))

(defun list-child-progress-snapshots (runtime session)
  "Signal that child-agent orchestration is unavailable in the public build."
  (declare (ignore runtime session))
  (%public-child-agent-unavailable "list-child-progress-snapshots"))

(defun cancel-child-agent (runtime session child-id &key reason)
  "Signal that child-agent orchestration is unavailable in the public build."
  (declare (ignore runtime session child-id reason))
  (%public-child-agent-unavailable "cancel-child-agent"))

(defun compaction-failure-count (session)
  "Return the consecutive compaction failure count for SESSION."
  (session-state-value session :compaction-failure-count 0))

(defun reset-compaction-failures (session)
  "Reset the compaction failure count for SESSION."
  (set-session-state-value session :compaction-failure-count 0))

(defun increment-compaction-failures (session)
  "Increment and return the compaction failure count for SESSION."
  (let ((count (1+ (compaction-failure-count session))))
    (set-session-state-value session :compaction-failure-count count)
    count))

(defun compaction-circuit-open-p (session)
  "Return true when SESSION has exceeded the baseline compaction failure limit."
  (>= (compaction-failure-count session) +compaction-failure-limit+))

(defun extract-session-durable-memory (runtime session)
  "Run the durable-memory ingestion pipeline for SESSION.
Returns the list of saved durable-memory records (possibly empty)."
  (extract-durable-memory (runtime-settings runtime) session))

(defun maybe-extract-durable-memory (runtime session)
  "Persist durable memory for SESSION when the baseline extraction criteria are met.

When one or more records are saved, record the extraction in the transcript
for observability. Returns the list of saved records."
  (let ((saved (extract-session-durable-memory runtime session)))
    (when saved
      (maybe-append-transcript-event
       session
       (session-transcript-path runtime session)
       (list :event "durable_memory_extract"
             :session_id (claw-lisp.core.domain:agent-session-id session)
             :saved_count (length saved))))
    saved))

(defun maybe-append-transcript-event (session pathname event)
  "Append EVENT to PATHNAME, degrading to a warning on transcript I/O failure.

Transcript persistence is baseline observability. The primary message flow
should continue even if writing the transcript fails."
  (call-session-runtime-event-callback session event)
  ;; Serializer errors are degraded here too, so unsupported transcript values
  ;; are treated as observability failures rather than runtime failures.
  (handler-case
      (append-transcript-event pathname event)
    (error (condition)
      (warn "Failed to write transcript event for session ~A: ~A"
            (claw-lisp.core.domain:agent-session-id session)
            condition)
      nil)))

(defun %cas-kind-label (kind)
  "Render KIND as a stable lowercase underscore label."
  (let* ((text (string-downcase (string kind)))
         (normalized (substitute #\_ #\- text)))
    (if (and (> (length normalized) 0)
             (char= (char normalized 0) #\:))
        (subseq normalized 1)
        normalized)))

(defun %cas-type-label (type)
  "Render CAS TYPE as a stable lowercase underscore label."
  (%cas-kind-label type))

(defun %manifest-kind-p (kind)
  "Return true when KIND appears to describe a manifest artifact."
  (and kind
       (not (null (search "manifest" (%cas-kind-label kind) :test #'char=)))))

(defun maybe-append-cas-object-written-event (session pathname descriptor)
  "Append a CAS object-written transcript event derived from DESCRIPTOR."
  (let ((cas-hash (getf descriptor :cas-hash)))
    (when cas-hash
      (maybe-append-transcript-event
       session pathname
       (list :event "cas_object_written"
             :session_id (claw-lisp.core.domain:agent-session-id session)
             :kind (%cas-kind-label (getf descriptor :kind))
             :cas_hash cas-hash
             :cas_type (%cas-type-label (getf descriptor :cas-type))
             :cas_ref_name (getf descriptor :cas-ref-name)
             :bytes (getf descriptor :bytes)
             :deduplicated_p (and (getf descriptor :deduplicated-p) t)
             :tool_name (getf descriptor :tool-name)
             :call_id (getf descriptor :call-id))))))

(defun maybe-append-cas-manifest-created-event (session pathname descriptor)
  "Append a CAS manifest-created transcript event when DESCRIPTOR represents a manifest."
  (when (and (getf descriptor :cas-hash)
             (%manifest-kind-p (getf descriptor :kind)))
    (maybe-append-transcript-event
     session pathname
     (list :event "cas_manifest_created"
           :session_id (claw-lisp.core.domain:agent-session-id session)
           :kind (%cas-kind-label (getf descriptor :kind))
           :cas_hash (getf descriptor :cas-hash)
           :cas_type (%cas-type-label (getf descriptor :cas-type))
           :cas_ref_name (getf descriptor :cas-ref-name)
           :bytes (getf descriptor :bytes)
           :metadata (getf descriptor :metadata)))))

(defun maybe-append-cas-verify-failed-event (session pathname kind cas-hash failure reason)
  "Append a CAS verification-failure transcript event."
  (maybe-append-transcript-event
   session pathname
   (list :event "cas_verify_failed"
         :session_id (claw-lisp.core.domain:agent-session-id session)
         :kind (%cas-kind-label kind)
         :cas_hash cas-hash
         :failure (%cas-kind-label failure)
         :reason (princ-to-string reason))))

(defun persist-session-artifact-to-cas (runtime session kind payload
                                         &key (type :sexp) ref-name metadata)
  "Persist PAYLOAD for SESSION and emit runtime-owned CAS transcript events."
  (multiple-value-bind (artifact descriptor)
      (claw-lisp.core.artifacts:persist-artifact-to-cas
       runtime kind payload :type type :ref-name ref-name :metadata metadata)
    (when descriptor
      (let ((transcript-path (session-transcript-path runtime session)))
        (maybe-append-cas-object-written-event session transcript-path descriptor)
        (maybe-append-cas-manifest-created-event session transcript-path descriptor)))
    artifact))

(defun store-session-tool-result-cas (runtime session tool-result &key (dedup-p t))
  "Persist TOOL-RESULT for SESSION and emit runtime-owned CAS transcript events."
  (multiple-value-bind (stored artifact descriptor)
      (claw-lisp.core.artifacts:store-tool-result-cas runtime tool-result :dedup-p dedup-p)
    (when descriptor
      (maybe-append-cas-object-written-event
       session (session-transcript-path runtime session) descriptor))
    (values stored artifact)))

(defun decode-transcript-json-line (line)
  "Decode one transcript JSONL LINE into a plist event object."
  (claw-lisp.providers.http-utils:json-decode line))

(defun inspect-transcript-file (transcript-path &key (tail-count 0))
  "Inspect transcript JSONL at TRANSCRIPT-PATH with streaming reads.

Returns a plist with:
  :path :exists :line-count :parsed-json-count :malformed-json-count
  :event-count :message-count :last-event :last-compaction-event-line
  :missing-event-count :tail-lines :error"
  (let ((summary (list :path transcript-path
                       :exists (and transcript-path (probe-file transcript-path))
                       :line-count 0
                       :parsed-json-count 0
                       :malformed-json-count 0
                       :event-count 0
                       :message-count 0
                       :last-event nil
                       :last-compaction-event-line nil
                       :missing-event-count 0
                       :tail-lines nil
                       :error nil)))
    (when (getf summary :exists)
      (handler-case
          (with-open-file (stream transcript-path :direction :input)
            (let ((ring (and (> tail-count 0)
                             (make-array tail-count :initial-element nil)))
                  (tail-index 0)
                  (tail-filled 0))
              (labels ((push-tail-line (line)
                         (when ring
                           (setf (aref ring tail-index) line
                                 tail-index (mod (1+ tail-index) tail-count))
                           (when (< tail-filled tail-count)
                             (incf tail-filled))))
                       (finalize-tail-lines ()
                         (when ring
                           (let ((start-index (if (= tail-filled tail-count)
                                                  tail-index
                                                  0)))
                             (setf (getf summary :tail-lines)
                                   (loop for i from 0 below tail-filled
                                         for slot = (mod (+ start-index i) tail-count)
                                         collect (aref ring slot)))))))
                (loop for line = (read-line stream nil nil)
                      while line
                      do
                         (incf (getf summary :line-count))
                         (push-tail-line line)
                         (handler-case
                             (let* ((event (decode-transcript-json-line line))
                                    (event-name (getf event :event)))
                               (incf (getf summary :parsed-json-count))
                               (cond
                                 ((null event-name)
                                  (incf (getf summary :missing-event-count)))
                                 ((stringp event-name)
                                  (incf (getf summary :event-count))
                                  (setf (getf summary :last-event) event-name)
                                  (when (string= event-name "message")
                                    (incf (getf summary :message-count)))
                                  (when (member event-name
                                                '("compaction_boundary" "microcompact")
                                                :test #'string=)
                                    (setf (getf summary :last-compaction-event-line) line)))))
                           (error ()
                             (incf (getf summary :malformed-json-count)))))
                (finalize-tail-lines))))
        (error (condition)
          (setf (getf summary :error) (princ-to-string condition)))))
    summary))

(defun check-runtime-dependencies ()
  "Return a list of missing external commands required by the current runtime.

With the native HTTP client (dexador) and JSON library (yason), the only
external dependency is the `timeout` command used by the shell tool."
  (remove nil
          (mapcar (lambda (command)
                    (unless (claw-lisp.providers.http-utils:command-available-p command)
                      command))
                  '("timeout"))))

(defun check-provider-configuration (runtime provider-name)
  "Return NIL when PROVIDER-NAME is ready, otherwise a human-readable error."
  (let* ((normalized-provider (%normalize-provider-name provider-name))
         (provider (resolve-provider runtime normalized-provider)))
    (cond
      ((null provider)
       (format nil "Unknown provider: ~A" normalized-provider))
      ((string= normalized-provider "mock")
       nil)
      ((or (null (claw-lisp.core.protocols:provider-api-key provider))
           (= 0 (length (claw-lisp.core.protocols:provider-api-key provider))))
       (format nil "Provider ~A is missing its API key configuration." normalized-provider))
      (t nil))))

(defun register-default-providers (runtime)
  "Register the baseline provider set for the current runtime config."
  (let ((config (runtime-settings runtime)))
    (register-provider runtime
                       (claw-lisp.providers.mock:make-mock-provider))
    (register-provider runtime
                       (claw-lisp.providers.anthropic:make-anthropic-provider
                        config))
    (register-provider runtime
                       (claw-lisp.providers.openrouter:make-openrouter-provider
                        config))
    runtime))

(defun register-default-tools (runtime)
  "Register the baseline tool set for the runtime."
  (register-tool runtime (claw-lisp.tools.echo:make-echo-tool))
  (register-tool runtime (claw-lisp.tools.file-read:make-file-read-tool))
  (register-tool runtime (claw-lisp.tools.file-write:make-file-write-tool))
  (register-tool runtime (claw-lisp.tools.file-replace:make-file-replace-tool))
  (register-tool runtime (claw-lisp.tools.glob:make-glob-tool))
  (register-tool runtime (claw-lisp.tools.grep:make-grep-tool))
  (register-tool runtime (claw-lisp.tools.shell-command:make-shell-command-tool))
  runtime)

(defun %normalize-provider-name (provider-name)
  "Return canonical provider name string (trimmed, lowercase), or NIL."
  (when provider-name
    (let ((trimmed (string-trim '(#\Space #\Tab #\Newline #\Return) provider-name)))
      (unless (zerop (length trimmed))
        (string-downcase trimmed)))))

(defun register-provider (runtime provider)
  "Register PROVIDER under its declared name and return RUNTIME."
  (let* ((canonical-name (%normalize-provider-name (provider-name provider))))
    (unless canonical-name
      (error "Provider name cannot be empty."))
    ;; Keep provider object aligned with canonical registry key.
    (setf (provider-name provider) canonical-name)
    (setf (gethash canonical-name (runtime-provider-registry runtime))
          provider)
    runtime))

(defun resolve-tool (runtime tool-name)
  "Return the tool bound to TOOL-NAME or NIL."
  (gethash tool-name (runtime-tool-registry runtime)))

(defun resolve-provider (runtime provider-name)
  "Return the provider bound to PROVIDER-NAME or NIL."
  (gethash (%normalize-provider-name provider-name) (runtime-provider-registry runtime)))

(defun list-provider-names (runtime)
  "Return registered provider names in sorted order."
  (sort
   (loop for key being the hash-keys of (runtime-provider-registry runtime)
         collect key)
   #'string<))

(defun %provider-name->keyword (provider-name)
  "Convert PROVIDER-NAME string to keyword form."
  (let ((normalized (%normalize-provider-name provider-name)))
    (and normalized
         (intern (string-upcase normalized) :keyword))))

(defun list-model-names (runtime &key provider-name)
  "Return known model names from the model registry.

When PROVIDER-NAME is supplied, only models for that provider are returned."
  (let* ((registry (runtime-model-registry runtime))
         (models (claw-lisp.core.model-registry::model-registry-models registry))
         (provider-keyword (%provider-name->keyword provider-name)))
    (sort
     (loop for key being the hash-keys of models
           using (hash-value caps)
           when (or (null provider-keyword)
                    (eq provider-keyword
                        (claw-lisp.core.domain:model-capabilities-provider caps)))
           collect key)
     #'string<)))

(defun %model-resolution-source (runtime model-id)
  "Return how MODEL-ID resolved in the model registry."
  (let* ((registry (runtime-model-registry runtime))
         (models (claw-lisp.core.model-registry::model-registry-models registry))
         (aliases (claw-lisp.core.model-registry::model-registry-aliases registry))
         (defaults (claw-lisp.core.model-registry::model-registry-provider-defaults registry))
         (alias-target (gethash model-id aliases))
         (prefix-hit (claw-lisp.core.model-registry::find-by-prefix registry model-id)))
    (cond
      ((gethash model-id models) :exact)
      ((and alias-target (gethash alias-target models)) :alias)
      (prefix-hit :prefix)
      ((gethash (claw-lisp.core.model-registry::infer-provider model-id) defaults)
       :provider-default)
      (t :fallback))))

(defun %provider-default-or-fallback-model-p (provider resolution-source)
  "Return true when RESOLUTION-SOURCE should be rejected for PROVIDER.

The mock provider is intentionally permissive in tests and local validation.
Real providers should reject provider-default/fallback model guesses before
credential checks so users see the model error they actually made."
  (and provider
       (member resolution-source '(:provider-default :fallback) :test #'eq)
       (not (string= (%normalize-provider-name
                      (claw-lisp.core.protocols:provider-name provider))
                     "mock"))))

(defun call-with-session-flag (session flag busy-error-message thunk)
  "Run THUNK with SESSION FLAG set, always clearing FLAG in unwind-protect."
  (when (session-state-value session flag)
    (error "~A" busy-error-message))
  (set-session-state-value session flag t)
  (unwind-protect
       (funcall thunk)
    (set-session-state-value session flag nil)))

(defun select-session-model (runtime session &key provider-name model profile allow-incompatible-model-p)
  "Select provider/model/profile for SESSION atomically.

Returns a plist with :ok, :provider, :model, and optional :warnings.
Signals an error on hard failures; SESSION remains unchanged in those cases.

Provider/model compatibility is validated for exact/alias/prefix model matches.
Set ALLOW-INCOMPATIBLE-MODEL-P to bypass this validation explicitly."
  (flet ((%run-selection ()
           (when (session-state-value session :turn-in-flight-p)
             (error "Session is busy executing a turn. Try again after it finishes."))
           (call-with-session-flag
            session
            :selection-in-progress
            "Session is busy updating provider/model configuration."
            (lambda ()
              (let* ((old-provider (claw-lisp.core.domain:agent-session-provider session))
                     (old-model (claw-lisp.core.domain:agent-session-model session))
                     (old-profile (session-state-value session :profile nil))
                     (effective-provider (or (%normalize-provider-name provider-name)
                                             (%normalize-provider-name old-provider)))
                     (effective-model (or model old-model))
                     (effective-profile (or profile old-profile))
                     (provider-keyword (%provider-name->keyword effective-provider))
                     (registry (runtime-model-registry runtime))
                     (resolved-caps (claw-lisp.core.model-registry:resolve-model
                                     registry effective-model))
                     (warnings nil))
                (when (or (null effective-model) (zerop (length effective-model)))
                  (error "Model cannot be empty."))
                (let* ((resolved-provider (and effective-provider
                                              (resolve-provider runtime effective-provider)))
                       (resolution-source (%model-resolution-source runtime effective-model))
                       (provider-error (and effective-provider
                                            (check-provider-configuration runtime effective-provider))))
                  (when (%provider-default-or-fallback-model-p resolved-provider resolution-source)
                    (error "Unknown model: ~A" effective-model))
                  (when provider-error
                    (error "~A" provider-error))
                  ;; For known model matches, enforce provider/model compatibility by default.
                  (when (and (member resolution-source '(:exact :alias :prefix) :test #'eq)
                             provider-keyword
                             resolved-caps
                             (not (eq provider-keyword
                                      (claw-lisp.core.domain:model-capabilities-provider resolved-caps))))
                    (if allow-incompatible-model-p
                        (push (format nil "Provider/model compatibility override enabled: model ~A resolves to provider ~A while selected provider is ~A."
                                      effective-model
                                      (claw-lisp.core.domain:model-capabilities-provider resolved-caps)
                                      effective-provider)
                              warnings)
                        (error
                         "Model ~A resolves to provider ~A, which is incompatible with selected provider ~A. Set :allow-incompatible-model-p t to override."
                         effective-model
                         (claw-lisp.core.domain:model-capabilities-provider resolved-caps)
                         effective-provider)))
                  (when (member resolution-source '(:provider-default :fallback) :test #'eq)
                    (push (format nil "Model ~A resolved via ~A capabilities; verify provider support."
                                  effective-model resolution-source)
                          warnings)))
                ;; Atomic field mutation in one critical section.
                (setf (claw-lisp.core.domain:agent-session-provider session) effective-provider
                      (claw-lisp.core.domain:agent-session-model session) effective-model)
                (set-session-state-value session :profile effective-profile)
                (maybe-append-transcript-event
                 session
                 (session-transcript-path runtime session)
                 (list :event "session_config_changed"
                       :event_version 1
                       :session_id (claw-lisp.core.domain:agent-session-id session)
                       :previous_provider old-provider
                       :previous_model old-model
                       :provider effective-provider
                       :model effective-model
                       :profile effective-profile
                       :changed_at_universal_time (get-universal-time)))
                (list :ok t
                      :provider effective-provider
                      :model effective-model
                      :profile effective-profile
                      :warnings (nreverse warnings)))))))
    #+sb-thread
    (let ((lock (%session-selection-lock runtime session)))
      (sb-thread:with-mutex (lock)
        (%run-selection)))
    #-sb-thread
    (%run-selection)))

(defun list-tool-names (runtime)
  "Return registered tool names in sorted order."
  (sort
   (loop for key being the hash-keys of (runtime-tool-registry runtime)
         collect key)
   #'string<))

(defun next-tool-call-id (conversation tool-name)
  "Return the next deterministic tool call ID for TOOL-NAME in CONVERSATION.

This baseline scheme assumes tool results are append-only within a session.
Note: When parsing real API responses, the API-assigned ID (from tool-use-block)
takes precedence over this generated ID."
  (format nil "~A-call-~D"
          tool-name
          (1+ (length (claw-lisp.core.domain:conversation-tool-results conversation)))))

(defun provider-tool-descriptors (runtime)
  "Return a full tool descriptor list for provider tool-call requests.

Each descriptor includes :name, :description, and :input_schema (JSON Schema)."
  (mapcar (lambda (tool-name)
            (let ((tool (resolve-tool runtime tool-name)))
              (list :name tool-name
                    :description (claw-lisp.core.protocols:tool-description tool)
                    :input_schema (claw-lisp.core.protocols:tool-input-schema tool))))
          (list-tool-names runtime)))

(defun response-tool-calls (response)
  "Return provider-requested tool calls from RESPONSE.

Checks the :tool-calls slot first (set by providers that parse tool_use blocks).
Falls back to :tool-calls in metadata for backward compatibility with mock provider."
  (or (claw-lisp.core.domain:transport-response-tool-calls response)
      (let ((metadata (claw-lisp.core.domain:transport-response-metadata response)))
        (and (listp metadata)
             (getf metadata :tool-calls)))))

(defun append-assistant-message (runtime session response)
  "Append a normalized assistant message for RESPONSE.

   The message is appended even when it has no visible text (e.g., a response
   that only contains tool_use blocks). This ensures the conversation history
   maintains correct user/assistant alternation for the next API call."
  (let* ((assistant-text
           (claw-lisp.core.domain:transport-response-assistant-text response))
         (conversation (claw-lisp.core.domain:agent-session-conversation session))
         (transcript-path (session-transcript-path runtime session))
         (assistant-metadata
           (list :provider (transport-response-provider response)
                 :status (transport-response-status response)
                 :ok-p (transport-response-ok-p response)
                 :error-message (transport-response-error-message response)
                 :tool-calls (response-tool-calls response))))
    (append-message conversation
                    (make-message :role :assistant
                                  :content (if (> (length assistant-text) 0)
                                               assistant-text
                                               "")
                                  :metadata assistant-metadata))
    (update-session-memory (runtime-settings runtime) session)
    (when (> (length assistant-text) 0)
      (maybe-append-transcript-event
       session
       transcript-path
       (list :event "message"
             :session_id (claw-lisp.core.domain:agent-session-id session)
             :role "assistant"
             :content assistant-text
             :metadata assistant-metadata)))
    t))

(defun normalize-provider-turn-response (provider raw-response)
  "Normalize RAW-RESPONSE from PROVIDER into a transport-response."
  (let ((normalized (normalize-response provider raw-response)))
    (if (typep normalized 'transport-response)
        normalized
        (claw-lisp.core.domain:make-transport-response
         :ok-p nil
         :status 0
         :assistant-text "[provider returned no text]"
         :raw-response ""
         :provider (provider-name provider)
         :metadata normalized))))

(defun tool-result-error-p (result)
  "Return T if RESULT looks like an error: empty/nil content, or content
that starts with '[' and contains 'error' or 'timed out'."
  (let ((content (claw-lisp.core.domain:tool-result-content result)))
    (or (null content)
        (and (stringp content) (zerop (length content)))
        (and (stringp content)
             (plusp (length content))
             (char= (char content 0) #\[)
             (let ((downcased (string-downcase content)))
               (or (search "error" downcased)
                   (search "timed out" downcased)))))))

(defun make-tool-result-message (tool-results)
  "Build a user message containing tool_result content blocks.

   TOOL-RESULTS is a list of tool-result structs produced by tool execution.
   Each result is converted to a `tool-result-block` so the Anthropic API
   can associate results with the tool_use blocks that requested them."
  (make-message
   :role :user
   :content
   (loop for result in tool-results
         collect
         (claw-lisp.core.domain:make-tool-result-block
          :tool-use-id (claw-lisp.core.domain:tool-result-call-id result)
          :content (let ((c (claw-lisp.core.domain:tool-result-content result)))
                     (if (and (stringp c) (plusp (length c)))
                         c
                         "Tool execution failed with no output."))
          :is-error (tool-result-error-p result)))))

(defun ensure-turn-not-in-flight (session)
  "Signal an error when SESSION already has a turn executing."
  (when (session-state-value session :turn-in-flight-p)
    (error "Session is busy executing a turn. Reentrant turns on the same session are not allowed.")))

(defun execute-provider-turn-loop (runtime session provider &key on-event)
  "Run the baseline provider turn loop with full context lifecycle management.

   Integrates:
   1. Idle-gap cleanup → triggered when user message arrives
   2. Proactive check   → runs before every API call
   3. Reactive handler  → wraps the API call for 413 recovery

   After executing tool calls, tool results are serialized back to the API
   as a user message with `tool_result` content blocks so the model can
   process results and produce a final answer.

   ON-EVENT (if supplied) is forwarded to stream-turn so UI layers can
   receive live SSE events (:text_delta, :tool_use_complete, etc)."
  (call-with-session-flag
   session
   :turn-in-flight-p
   "Session is busy executing a turn. Reentrant turns on the same session are not allowed."
   (lambda ()
     (let ((conversation (claw-lisp.core.domain:agent-session-conversation session))
           (model (claw-lisp.core.domain:agent-session-model session))
           (system-prompt (claw-lisp.core.system-prompt:build-system-prompt
                           :project-root (runtime-effective-project-root runtime)
                           :tool-registry (runtime-tool-registry runtime))))
       (loop for iteration from 1 to +max-provider-tool-iterations+
             do
                (let* ((supports-tools (model-supports-p (runtime-model-registry runtime)
                                                         model :tools))
                       (tools (when supports-tools
                                (provider-tool-descriptors runtime)))
                       ;; ---------------------------------------------------------
                       ;; 1. Idle-Gap Trigger (on new user message arrival)
                       ;; ---------------------------------------------------------
                       (_ (maybe-idle-gap-microcompact runtime session model
                                                       :system-prompt system-prompt
                                                       :tool-definitions tools))
                       ;; ---------------------------------------------------------
                       ;; 1b. Durable Memory Context Injection (after idle-gap)
                       ;; ---------------------------------------------------------
                       (_ (let ((user-msg (find :user
                                                (claw-lisp.core.domain:conversation-messages conversation)
                                                :key #'claw-lisp.core.domain:message-role
                                                :from-end t)))
                            (when user-msg
                              (incf (claw-lisp.core.domain:session-current-turn-id session))
                              (claw-lisp.storage.durable-memory-search:inject-durable-memory-context
                               session
                               (claw-lisp.core.domain:make-agent-turn
                                :content (claw-lisp.core.domain:message-content-text user-msg)
                                :messages (claw-lisp.core.domain:conversation-messages conversation))
                               :pass :initial
                               :force-refresh nil))))
                       ;; ---------------------------------------------------------
                       ;; 2. Proactive Context Check (pre-API call)
                       ;; ---------------------------------------------------------
                       (pre-status (check-and-manage-context runtime session model
                                                             :system-prompt system-prompt
                                                             :tool-definitions tools
                                                             :on-warning nil))
                       ;; Abort early if compaction can't free enough space
                       (_ (when (and (eq (context-status-action pre-status) :full-compaction)
                                     (>= (context-status-usage-ratio pre-status)
                                         (claw-lisp.config:runtime-config-context-compact-required-threshold
                                          (runtime-settings runtime))))
                            (warn "Context still at ~,1F% after compaction. Aborting turn."
                                  (* 100 (context-status-usage-ratio pre-status)))
                            (return-from execute-provider-turn-loop nil)))
                       ;; ---------------------------------------------------------
                       ;; 3. Reactive 413 Handler (wraps actual API call)
                       ;; ---------------------------------------------------------
                       (response
                         (handler-case
                             (let ((api-response
                                     (normalize-provider-turn-response
                                      provider
                                      (stream-turn provider
                                                   conversation
                                                   :model model
                                                   :tools tools
                                                   :on-event on-event
                                                   :system system-prompt))))
                               ;; Success: record interaction time for future idle-gap detection
                               (update-last-interaction-time session)
                               api-response)
                           (context-exceeded-error ()
                             ;; Reactive recovery: compact + retry exactly once
                             (handle-context-exceeded runtime session model
                                                      :system-prompt system-prompt
                                                      :tool-definitions tools
                                                      :retry-thunk
                                                      (lambda ()
                                                        (normalize-provider-turn-response
                                                         provider
                                                         (stream-turn provider
                                                                      conversation
                                                                      :model model
                                                                      :tools tools
                                                                      :on-event on-event
                                                                      :system system-prompt)))))))
                       (tool-calls (response-tool-calls response)))
                  (append-assistant-message runtime session response)
                  (if tool-calls
                      (let ((result-count-before
                              (length (claw-lisp.core.domain:conversation-tool-results conversation))))
                        (dolist (tool-call tool-calls)
                          (handler-case
                              (execute-registered-tool runtime
                                                       session
                                                       (getf tool-call :name)
                                                       (getf tool-call :input)
                                                       :call-id (getf tool-call :id))
                            (error (e)
                              (let ((error-result
                                      (claw-lisp.core.domain:make-tool-result
                                       :call-id (or (getf tool-call :id) "")
                                       :tool-name (or (getf tool-call :name) "")
                                       :content (format nil "[error] Tool ~A failed: ~A"
                                                        (getf tool-call :name) e))))
                                (claw-lisp.core.domain:record-tool-result
                                 (claw-lisp.core.domain:agent-session-conversation session)
                                 error-result)))))
                        ;; Collect the tool results produced in this iteration
                        ;; and send them back to the API as a user message.
                        (let* ((all-results (claw-lisp.core.domain:conversation-tool-results conversation))
                               (new-results (nthcdr result-count-before all-results)))
                          (when new-results
                            (let ((result-msg (make-tool-result-message new-results)))
                              (append-message conversation result-msg)
                              (let ((transcript-path (session-transcript-path runtime session)))
                                (maybe-append-transcript-event
                                 session
                                 transcript-path
                                 (list :event "tool_results_sent"
                                       :session_id (claw-lisp.core.domain:agent-session-id session)
                                       :count (length new-results))))))))
                      (return response)))
                finally
                   (error "Provider tool loop exceeded ~D iterations for session ~A."
                          +max-provider-tool-iterations+
                          (claw-lisp.core.domain:agent-session-id session)))))))

(defun default-tool-allowed-roots (config)
  "Return the baseline local roots allowed for filesystem tools."
  (list (uiop:ensure-directory-pathname (uiop:getcwd))
        (uiop:ensure-directory-pathname
         (merge-pathnames (claw-lisp.config:runtime-config-data-root config)
                          (uiop:getcwd)))
        #P"/tmp/"))

(defun effective-tool-allowed-roots (config)
  "Return configured filesystem tool roots, or the baseline default set."
  (let ((configured (claw-lisp.config:runtime-config-tool-allowed-roots config)))
    (if configured
        (mapcar (lambda (root)
                  (uiop:ensure-directory-pathname
                   (merge-pathnames root (uiop:getcwd))))
                configured)
        (default-tool-allowed-roots config))))

(defun tool-path-allowed-p (config path)
  "Return true when PATH is under one of the allowed filesystem tool roots."
  (let ((candidate (merge-pathnames path (uiop:getcwd))))
    (some (lambda (root)
            (or (equal candidate root)
                (uiop:subpathp candidate root)))
          (effective-tool-allowed-roots config))))

(defun authorize-tool-input (runtime tool-name input)
  "Enforce the baseline tool permission boundary for TOOL-NAME and INPUT."
  (let ((config (runtime-settings runtime)))
    (cond
      ((member tool-name '("file-read" "file-write" "file-replace" "glob" "grep") :test #'string=)
       (let ((path (getf input :path)))
         (unless (tool-path-allowed-p config path)
           (error "Tool ~A cannot access path outside allowed roots: ~A"
                  tool-name
                  path))))
      ((string= tool-name "shell-command")
       (unless (claw-lisp.config:runtime-config-shell-command-enabled-p config)
         (error "Tool ~A is disabled by runtime policy." tool-name)))))
  input)

(defun execute-registered-tool (runtime session tool-name input &key call-id)
  "Execute the registered TOOL-NAME with INPUT and record the normalized result.

CALL-ID is the API-assigned tool_use_id when available. If nil, a local
ID is generated for backward compatibility with the mock provider.

This is the baseline local tool path. Provider-mediated tool calling lands in a
later phase, but the runtime can already validate, execute, and persist local
tool results."
  (let* ((tool (resolve-tool runtime tool-name))
         (conversation (claw-lisp.core.domain:agent-session-conversation session))
         (effective-call-id (or call-id (next-tool-call-id conversation tool-name)))
         (transcript-path (session-transcript-path runtime session))
         (validated-input nil)
         (authorized-input nil)
         (call nil)
         (normalized-result nil))
    (unless tool
      (error "Unknown tool: ~A" tool-name))
    (handler-case
        (let* ((validated (claw-lisp.core.protocols:validate-tool-input tool input))
               (authorized (progn
                             (setf validated-input validated)
                             (authorize-tool-input runtime tool-name validated))))
          (setf authorized-input authorized
                call (make-tool-call :id effective-call-id
                                     :name tool-name
                                     :input authorized))
          (maybe-append-transcript-event
           session
           transcript-path
           (list :event "tool_start"
                 :session_id (claw-lisp.core.domain:agent-session-id session)
                 :call_id (claw-lisp.core.domain:tool-call-id call)
                 :tool_name tool-name
                 :input authorized-input))
          (let ((raw-result (execute-tool tool authorized-input runtime)))
            (setf normalized-result (normalize-tool-result tool raw-result))))
      (error (condition)
        (maybe-append-transcript-event
         session
         transcript-path
         (append (list :event "tool_error"
                       :session_id (claw-lisp.core.domain:agent-session-id session)
                       :call_id effective-call-id
                       :tool_name tool-name
                       :error (cond
                                (authorized-input
                                 (princ-to-string condition))
                                (validated-input
                                 (format nil "Tool ~A request rejected before execution."
                                         tool-name))
                                (t
                                 (format nil "Tool ~A input validation failed before execution."
                                         tool-name))))
                 (when authorized-input
                   (list :input authorized-input))))
        (error condition)))
    (let* ((cas-result
             (multiple-value-bind (stored artifact)
                 (store-session-tool-result-cas
                  runtime session
                  (claw-lisp.core.domain::%copy-tool-result-with
                   normalized-result
                   :call-id (claw-lisp.core.domain:tool-call-id call))
                  :dedup-p (claw-lisp.config:runtime-config-tool-result-dedup-p
                            (runtime-settings runtime)))
               (declare (ignore artifact))
               stored))
           (stored-result (store-tool-result (runtime-settings runtime)
                                             session
                                             cas-result))
           (result stored-result))
      (record-tool-result conversation result)
      (let ((budget-cleared-count
              (enforce-tool-result-aggregate-budget
               (runtime-settings runtime)
               conversation)))
        (when (> budget-cleared-count 0)
          (maybe-append-transcript-event
           session
           transcript-path
           (list :event "tool_result_budget_trim"
                 :session_id (claw-lisp.core.domain:agent-session-id session)
                 :cleared_count budget-cleared-count
                 :aggregate_budget_bytes
                 (claw-lisp.config:runtime-config-tool-result-aggregate-budget-bytes
                  (runtime-settings runtime))))))
      (let ((cleared-count
              (microcompact-conversation-tool-results
               (runtime-settings runtime)
               conversation)))
        (when (> cleared-count 0)
          (maybe-append-transcript-event
           session
           transcript-path
           (list :event "microcompact"
                 :session_id (claw-lisp.core.domain:agent-session-id session)
                 :cleared_count cleared-count
                 :keep_recent
                 (claw-lisp.config:runtime-config-microcompact-keep-recent-tool-results
                  (runtime-settings runtime))))))
      (update-session-memory (runtime-settings runtime) session)
      (maybe-append-transcript-event
       session
       transcript-path
       (list :event "tool_result"
             :session_id (claw-lisp.core.domain:agent-session-id session)
             :call_id (claw-lisp.core.domain:tool-call-id call)
             :tool_name tool-name
             :input authorized-input
             :result (append (tool-result->plist result)
                             (list :is_error (and (tool-result-error-p result) t)))))
      (maybe-extract-durable-memory runtime session)
      result)))

(defun start-session (runtime &key provider-name model session-id)
  "Create a baseline agent session. No provider traffic is sent here."
  (let* ((config (runtime-settings runtime))
         (effective-provider
           (%normalize-provider-name
            (or provider-name
                (claw-lisp.config:runtime-config-default-provider config))))
         (effective-model
           (or model
               (claw-lisp.config:runtime-config-default-model config))))
    (let ((session
            (make-agent-session
             :id (or session-id "session-0")
             :provider effective-provider
             :model effective-model
             :conversation (make-conversation :id (or session-id "session-0"))
             :state '(:initialized t))))
      (handler-case
          (ensure-session-transcript config session)
        (error (condition)
          (warn "Failed to initialize transcript for session ~A: ~A"
                (claw-lisp.core.domain:agent-session-id session)
                condition)))
      (handler-case
          (update-session-memory config session)
        (error (condition)
          (warn "Failed to initialize session memory for session ~A: ~A"
                (claw-lisp.core.domain:agent-session-id session)
                condition)))
      session)))

(defun %transcript-session-start-settings (transcript-path)
  "Return (values provider model) from TRANSCRIPT-PATH session_start event, or NIL values."
  (with-open-file (stream transcript-path :direction :input)
    (loop for line = (read-line stream nil nil)
          for line-number from 1
          while line
          do (handler-case
                 (let* ((event (claw-lisp.providers.http-utils:json-decode line))
                        (event-name (getf event :event)))
                   (when (and (stringp event-name)
                              (string= event-name "session_start"))
                     (return (values (getf event :provider)
                                     (getf event :model)))))
               (error (condition)
                 (warn "Skipping malformed transcript line ~D in ~A while inferring session settings: ~A"
                       line-number transcript-path condition)))
          finally (return (values nil nil)))))

(defun resume-session (runtime session-id &key provider-name model)
  "Resume SESSION-ID from transcript by rebuilding user/assistant text messages.

If PROVIDER-NAME and MODEL are not supplied, they are inferred from the
transcript session_start event when available.

This baseline resume path restores only `message` events (user/assistant text).
It does not restore tool results, child-agent events, compaction metadata, or
other runtime state."
  (let* ((config (runtime-settings runtime))
         (transcript-path (claw-lisp.storage.transcripts:transcript-existing-path-for-session
                           config session-id)))
    (unless (probe-file transcript-path)
      (error "No transcript found for session: ~A" session-id))
    (multiple-value-bind (inferred-provider inferred-model)
        (%transcript-session-start-settings transcript-path)
      (let* ((session (start-session runtime
                                     :provider-name (or provider-name inferred-provider)
                                     :model (or model inferred-model)
                                     :session-id session-id))
             (conversation (claw-lisp.core.domain:agent-session-conversation session))
             (restored-count 0))
        (with-open-file (stream transcript-path :direction :input)
          (loop for line = (read-line stream nil nil)
                for line-number from 1
                while line
                do (handler-case
                       (let* ((event (claw-lisp.providers.http-utils:json-decode line))
                              (event-name (getf event :event))
                              (role (getf event :role))
                              (content (getf event :content)))
                         (when (and (stringp event-name)
                                    (string= event-name "message")
                                    (stringp role)
                                    (stringp content)
                                    (member role '("user" "assistant") :test #'string=))
                           (append-message
                            conversation
                            (make-message :role (if (string= role "assistant")
                                                    :assistant
                                                    :user)
                                          :content content))
                           (incf restored-count)))
                     (error (condition)
                       (warn "Skipping malformed transcript line ~D in ~A during resume of session ~A: ~A"
                             line-number transcript-path session-id condition)))))
        (set-session-state-value session :resumed-from-transcript t)
        (set-session-state-value session :resumed-message-count restored-count)
        session))))

(defun rough-token-estimate (text)
  "Conservative byte-to-token estimate used before provider-specific counting exists."
  (max 1 (ceiling (length text) 4)))

(defun session-provider-object (runtime session)
  "Return the provider object for SESSION when registered."
  (resolve-provider runtime (claw-lisp.core.domain:agent-session-provider session)))

(defun session-token-estimate (session)
  "Estimate token usage for SESSION from accumulated message content."
  (let* ((conversation (claw-lisp.core.domain:agent-session-conversation session))
         (messages (conversation-messages conversation)))
    (reduce #'+
            messages
            :key (lambda (message)
                   (rough-token-estimate
                    (claw-lisp.core.domain:message-content-text message)))
            :initial-value 0)))

(defun session-context-token-estimate (runtime session)
  "Return the best available token estimate for SESSION under RUNTIME."
  (let* ((provider (session-provider-object runtime session))
         (messages (conversation-messages
                    (claw-lisp.core.domain:agent-session-conversation session))))
    (if provider
        (handler-case
            (count-tokens provider
                          messages
                          :model (claw-lisp.core.domain:agent-session-model session))
          (error ()
            (session-token-estimate session)))
        (session-token-estimate session))))

(defun effective-context-window (runtime session)
  "Return the effective context budget for SESSION after output reserve."
  (let* ((model (claw-lisp.core.domain:agent-session-model session))
         (registry (runtime-model-registry runtime))
         (config (runtime-settings runtime))
         (max-window (model-context-window registry model))
         (reserve (claw-lisp.config:runtime-config-context-output-reserve config)))
    (max 1 (- max-window reserve))))

(defun compaction-threshold (runtime session)
  "Return the token threshold at which SESSION should compact."
  (let* ((config (runtime-settings runtime))
         (effective-window (effective-context-window runtime session))
         (buffer (claw-lisp.config:runtime-config-compaction-trigger-buffer config)))
    (max 1 (- effective-window buffer))))

(defun warning-threshold (runtime session)
  "Return the token threshold at which SESSION should warn about approaching compaction."
  (let* ((config (runtime-settings runtime))
         (effective-window (effective-context-window runtime session))
         (buffer (claw-lisp.config:runtime-config-token-warning-buffer config)))
    (max 1 (- effective-window buffer))))

(defun context-warning-p (runtime session)
  "Return true when SESSION is approaching its effective context budget."
  (>= (session-context-token-estimate runtime session)
      (warning-threshold runtime session)))

(defun compaction-needed-p (runtime session)
  "Return true when SESSION has exceeded the baseline compaction threshold."
  (>= (session-context-token-estimate runtime session)
      (compaction-threshold runtime session)))

(defun session-context-status (runtime session)
  "Return a compact context-budget status plist for SESSION."
  (let ((tokens (session-context-token-estimate runtime session))
        (effective-window (effective-context-window runtime session))
        (warning-threshold (warning-threshold runtime session))
        (compaction-threshold (compaction-threshold runtime session)))
    (list :tokens tokens
          :effective-window effective-window
          :warning-threshold warning-threshold
          :compaction-threshold compaction-threshold
          :warning-p (>= tokens warning-threshold)
          :compaction-needed-p (>= tokens compaction-threshold))))

(defun submit-user-message (runtime session text &key on-event runtime-event-callback)
  "Append a user message and run the baseline provider turn path for SESSION.

This baseline path now supports a bounded local tool loop when the provider
returns tool-call metadata. Streaming and richer orchestration still land in
later phases."
  (ensure-turn-not-in-flight session)
  (let* ((conversation (claw-lisp.core.domain:agent-session-conversation session))
         (provider-name (claw-lisp.core.domain:agent-session-provider session))
         (provider (resolve-provider runtime provider-name))
         (transcript-path (session-transcript-path runtime session))
         (old-runtime-event-callback (session-runtime-event-callback session)))
    (unwind-protect
         (progn
           (when runtime-event-callback
             (set-session-state-value session :runtime-event-callback runtime-event-callback))
           (append-message conversation (make-message :role :user :content text))
           (maybe-append-transcript-event
            session
            transcript-path
            (list :event "message"
                  :session_id (claw-lisp.core.domain:agent-session-id session)
                  :role "user"
                  :content text))
           (when provider
             (execute-provider-turn-loop runtime session provider :on-event on-event)
             (maybe-extract-durable-memory runtime session))
           (unless provider
             (update-session-memory (runtime-settings runtime) session)
             (maybe-extract-durable-memory runtime session))
           session)
      (when runtime-event-callback
        (set-session-state-value session :runtime-event-callback old-runtime-event-callback)))))

;;; ============================================================
;;; Phase 4: Context Lifecycle Management Functions
;;; ============================================================

(defun update-last-interaction-time (session)
  "Record the current monotonic time as the last interaction time on SESSION."
  (set-session-state-value session
                           :last-interaction-time
                           (get-internal-real-time)))

(defun idle-gap-seconds (session)
  "Return seconds since last interaction, or NIL if no prior interaction recorded.
   Uses monotonic time to avoid NTP/DST artifacts."
  (let ((last-time (session-state-value session :last-interaction-time)))
    (when last-time
      (/ (- (get-internal-real-time) last-time)
         internal-time-units-per-second))))

(defun maybe-idle-gap-microcompact (runtime session model-id
                                    &key system-prompt tool-definitions)
  "Run microcompact if the user has been idle long enough and context usage
   exceeds the minimum ratio. Called on new user message arrival."
  (let* ((config (runtime-settings runtime))
         (gap (idle-gap-seconds session)))
    (when gap
      (let* ((conversation (claw-lisp.core.domain:agent-session-conversation session))
             (registry (runtime-model-registry runtime))
             (status (assess-context config registry model-id conversation
                                     :system-prompt system-prompt
                                     :tool-definitions tool-definitions))
             (ratio (context-status-usage-ratio status)))
        (when (idle-gap-microcompact-needed-p config gap ratio)
          (let ((cleared (microcompact-conversation-tool-results
                          config
                          conversation)))
            (when (> cleared 0)
              (enforce-tool-result-aggregate-budget
               config
               conversation)
              (update-last-interaction-time session)
              (warn "Idle-gap microcompact triggered after ~D seconds. Usage: ~,1F%"
                    gap (* 100 ratio))
              t)))))))

(defun check-and-manage-context (runtime session model-id
                                 &key system-prompt tool-definitions on-warning)
  "Proactive pre-query context check. Assesses usage, runs microcompact or
   full compaction as needed, and returns the final context-status.

   ON-WARNING — optional callback (lambda (warning-string)) for UI display."
  (let* ((config (runtime-settings runtime))
         (registry (runtime-model-registry runtime))
         (conversation (claw-lisp.core.domain:agent-session-conversation session))
         (status (assess-context config registry model-id conversation
                                 :system-prompt system-prompt
                                 :tool-definitions tool-definitions)))
    ;; Emit warning if applicable
    (let ((warning-text (format-context-warning status)))
      (when warning-text
        (maybe-append-transcript-event
         session
         (session-transcript-path runtime session)
         (list :event "context_warning"
               :session_id (claw-lisp.core.domain:agent-session-id session)
               :action (context-status-action status)
               :usage-ratio (context-status-usage-ratio status)
               :estimated-tokens (context-status-estimated-tokens status)
               :context-limit (context-status-context-limit status)))
        (when on-warning
          (funcall on-warning warning-text))))

    ;; Execute the recommended action
    (case (context-status-action status)
      (:microcompact
       (microcompact-conversation-tool-results
        config
        conversation))

      (:aggressive-microcompact
       (microcompact-conversation-tool-results
        config
        conversation)
       (enforce-tool-result-aggregate-budget
        config
        conversation))

      (:full-compaction
       ;; Try lightweight cleanup first; it might be enough
       (microcompact-conversation-tool-results
        config
        conversation)
       (enforce-tool-result-aggregate-budget
        config
        conversation)

       ;; Re-assess after cleanup
       (let ((post-mc-status
               (assess-context config registry model-id conversation
                               :system-prompt system-prompt
                               :tool-definitions tool-definitions)))
         (when (>= (context-status-usage-ratio post-mc-status)
                   (claw-lisp.config:runtime-config-context-compact-required-threshold config))
           ;; Still critical → attempt full compaction
           (handler-case
               (try-compact-session runtime session)
             (error (e)
               ;; Log but don't crash; caller can inspect final status
               (warn "Full compaction failed: ~A" e)))))))

    ;; Return final status for caller inspection
    (assess-context config registry model-id
                    (claw-lisp.core.domain:agent-session-conversation session)
                    :system-prompt system-prompt
                    :tool-definitions tool-definitions)))

;;; ============================================================
;;; Phase 5 Task 7: Session Memory Integration
;;; ============================================================

(defun read-session-memory-text (runtime session)
  "Return the session-memory file contents for SESSION, or NIL if missing/empty.
   Errors are degraded to NIL so callers can treat missing/failed reads as
   absence of session memory."
  (let ((path (session-memory-path-for-session runtime session)))
    (let ((path (or (claw-lisp.storage.session-memory:session-memory-existing-path
                     (runtime-settings runtime)
                     (claw-lisp.core.domain:agent-session-id session))
                    path)))
    (handler-case
        (when (probe-file path)
          (let ((text (uiop:read-file-string path)))
            (when (> (length text) 0)
              text)))
      (error (condition)
        (warn "Failed to read session memory for session ~A: ~A"
              (claw-lisp.core.domain:agent-session-id session)
              condition)
        nil)))))

(defun session-memory-update-needed-p (runtime session)
  "Return T when session memory should be updated for SESSION.
   Conditions:
   - No existing session-memory file → initial update.
   - Existing metadata indicates staleness via session-memory-stale-p."
  (let* ((config (runtime-settings runtime))
         (text (read-session-memory-text runtime session)))
    (if (null text)
        t
        (let ((metadata (claw-lisp.storage.session-memory:parse-session-memory-header
                         text)))
          (and metadata
               (claw-lisp.storage.session-memory:session-memory-stale-p
                config metadata session))))))

(defun perform-session-memory-update (runtime session)
  "Perform a session memory update for SESSION and return the updated metadata.
   This function computes metadata, writes the session-memory note, and returns
   the metadata used. Errors are propagated to the caller."
  (let* ((config (runtime-settings runtime))
         (conversation (claw-lisp.core.domain:agent-session-conversation session))
         (current-tokens (claw-lisp.core.token-estimation:estimate-conversation-tokens
                          conversation))
         (current-tool-count
           (length (claw-lisp.core.domain:conversation-tool-results conversation)))
         (existing-text (read-session-memory-text runtime session))
         (metadata (or (and existing-text
                            (claw-lisp.storage.session-memory:parse-session-memory-header
                             existing-text))
                       (claw-lisp.storage.session-memory:make-session-memory-metadata)))
         (now (get-universal-time)))
    ;; Update metadata fields
    (setf (claw-lisp.storage.session-memory:session-memory-metadata-update-count metadata)
          (1+ (claw-lisp.storage.session-memory:session-memory-metadata-update-count
               metadata)))
    (setf (claw-lisp.storage.session-memory:session-memory-metadata-last-updated-universal-time
           metadata)
          now)
    (setf (claw-lisp.storage.session-memory:session-memory-metadata-tokens-at-last-update
           metadata)
          current-tokens)
    (setf (claw-lisp.storage.session-memory:session-memory-metadata-tool-count-at-last-update
           metadata)
          current-tool-count)
    ;; Budget max from config if not already set
    (when (zerop (claw-lisp.storage.session-memory:session-memory-metadata-budget-chars-max
                  metadata))
      (setf (claw-lisp.storage.session-memory:session-memory-metadata-budget-chars-max
             metadata)
            (claw-lisp.config:runtime-config-session-memory-budget-chars config)))
    ;; Render and write note (+ structured sidecar in storage layer)
    (let* ((note-text (claw-lisp.storage.session-memory:render-session-memory
                       session :metadata metadata)))
      ;; Keep metadata consistent with the just-rendered note before sidecar write.
      (setf (claw-lisp.storage.session-memory:session-memory-metadata-budget-chars-used
             metadata)
            (length note-text))
      (claw-lisp.storage.session-memory:update-session-memory
       config session :metadata metadata :rendered-note-text note-text)
      metadata)))

(defun maybe-update-session-memory (runtime session)
  "Evaluate triggers and conditionally update session memory for SESSION.
   This function is designed to be called by the query loop after each assistant
   turn (and after tool results are processed). It is failure-tolerant: errors
   are logged and recorded in the transcript, but never propagated.
   Returns T when an update was performed, NIL otherwise."
  (handler-case
      (when (session-memory-update-needed-p runtime session)
        (let* ((metadata (perform-session-memory-update runtime session))
               (path (session-memory-path-for-session runtime session)))
          ;; Record transcript event for observability
          (maybe-append-transcript-event
           session
           (session-transcript-path runtime session)
           (list :event "session_memory_update"
                 :session_id (claw-lisp.core.domain:agent-session-id session)
                 :path (namestring path)
                 :update_count
                 (claw-lisp.storage.session-memory:session-memory-metadata-update-count
                  metadata)
                 :budget_chars_used
                 (claw-lisp.storage.session-memory:session-memory-metadata-budget-chars-used
                  metadata)
                 :budget_chars_max
                 (claw-lisp.storage.session-memory:session-memory-metadata-budget-chars-max
                  metadata)
                 :stale_p
                 (claw-lisp.storage.session-memory:session-memory-metadata-stale-p
                  metadata)))
          t))
    (error (condition)
      (warn "Session memory update failed for session ~A: ~A"
            (claw-lisp.core.domain:agent-session-id session)
            condition)
      (maybe-append-transcript-event
       session
       (session-transcript-path runtime session)
       (list :event "session_memory_update_error"
             :session_id (claw-lisp.core.domain:agent-session-id session)
             :error (princ-to-string condition)))
      nil)))

;;; ============================================================
;;; Phase 5 Task 7: Compaction Integration
;;; ============================================================

(defun compact-session-with-session-memory (runtime session &key (keep-recent-messages 4))
  "Try the baseline session-memory compaction path for SESSION.
   This uses the selective session-memory-assisted compaction. Returns a
   compaction-result or NIL when session memory is absent or stale."
  (claw-lisp.core.compact:try-session-memory-compaction
   (runtime-settings runtime)
   session
   :keep-recent-messages keep-recent-messages))

(defun compact-session (runtime session &key (keep-recent-messages 4))
  "Return the baseline local compaction result for SESSION.
   This conservative path reuses session memory when available. If not, it falls
   back to a deterministic local summary derived from the current session state."
  (claw-lisp.core.compact:compact-session-locally
   (runtime-settings runtime)
   session
   :keep-recent-messages keep-recent-messages))

(defun try-compact-session (runtime session &key (keep-recent-messages 4))
  "Attempt compaction for SESSION unless the failure circuit is open.

   Order of operations:
   1. Try session-memory-assisted compaction via compact-session-with-session-memory.
   2. If that returns NIL or errors, fall back to compact-session.
   3. On success, apply the compaction result and reset the failure count.
   4. On error, increment the failure count and re-signal the error so callers
      can observe it."
  (when (compaction-circuit-open-p session)
    (return-from try-compact-session nil))
  (handler-case
      (let ((result nil))
        ;; First attempt: session-memory-assisted compaction
        (handler-case
            (setf result (compact-session-with-session-memory
                          runtime session
                          :keep-recent-messages keep-recent-messages))
          (error (condition)
            (warn "Session-memory compaction failed for session ~A: ~A"
                  (claw-lisp.core.domain:agent-session-id session)
                  condition)
            (setf result nil)))
        ;; Fallback: baseline compaction if needed
        (when (null result)
          (setf result (compact-session runtime
                                        session
                                        :keep-recent-messages keep-recent-messages)))
        (when result
          (apply-session-compaction runtime session result))
        (reset-compaction-failures session)
        result)
    (error (condition)
      (let ((failures (increment-compaction-failures session)))
        (declare (ignore failures)))
      (signal condition))))

(defun handle-context-exceeded (runtime session model-id
                                &key system-prompt tool-definitions retry-thunk)
  "Reactive handler for context-exceeded-error (413). Compacts and retries once.

   RETRY-THUNK — zero-arg function that re-executes the failed API call.
   Returns the result of RETRY-THUNK on success, or signals the original error."
  (if (compaction-circuit-open-p session)
      (error 'claw-lisp.core.conditions:context-exceeded-error
             :message "Context exceeded and compaction circuit breaker is open."
             :status-code 413)
      (progn
        (maybe-append-transcript-event
         session
         (session-transcript-path runtime session)
         (list :event "context_413_recovery"
               :session_id (claw-lisp.core.domain:agent-session-id session)))
        ;; Aggressive microcompact first
        (let ((config (runtime-settings runtime))
              (conversation (claw-lisp.core.domain:agent-session-conversation session)))
          (microcompact-conversation-tool-results
           config
           conversation)
          (enforce-tool-result-aggregate-budget
           config
           conversation))
        ;; Full compaction
        (try-compact-session runtime session)
        ;; Post-compact validation: abort if still critical
        (let* ((config (runtime-settings runtime))
               (registry (runtime-model-registry runtime))
               (conversation (claw-lisp.core.domain:agent-session-conversation session))
               (post-status (assess-context config registry model-id conversation
                                            :system-prompt system-prompt
                                            :tool-definitions tool-definitions)))
          (when (>= (context-status-usage-ratio post-status)
                    (claw-lisp.config:runtime-config-context-compact-required-threshold config))
            (increment-compaction-failures session)
            (error 'claw-lisp.core.conditions:context-exceeded-error
                   :message (format nil "Context still at ~,1F% after compaction. Cannot retry."
                                    (* 100 (context-status-usage-ratio post-status)))
                   :status-code 413)))
        ;; Retry exactly once
        (when retry-thunk
          (funcall retry-thunk)))))

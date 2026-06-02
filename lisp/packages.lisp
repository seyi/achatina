(defpackage #:claw-lisp.config
  (:use #:cl)
  (:export
   ;; Global runtime config
   #:*runtime-config*
   ;; Structs
   #:runtime-config
   #:runtime-config-name
   #:runtime-config-state-root
   #:runtime-config-data-root
   #:runtime-config-transcripts-root
   #:runtime-config-artifacts-root
   #:runtime-config-memory-root
   #:runtime-config-cas-objects-root
   #:runtime-config-cas-ref-root
   #:runtime-config-tool-result-dedup-p
   #:runtime-config-default-provider
   #:runtime-config-default-model
   #:runtime-config-tool-preview-bytes
   #:runtime-config-tool-result-aggregate-budget-bytes
   #:runtime-config-tool-allowed-roots
   #:runtime-config-shell-command-enabled-p
   #:runtime-config-shell-command-timeout-seconds
   #:runtime-config-microcompact-keep-recent-tool-results
   #:runtime-config-post-compact-keep-recent-tool-results
   #:runtime-config-context-output-reserve
   #:runtime-config-token-warning-buffer
   #:runtime-config-compaction-trigger-buffer
   #:runtime-config-context-warning-threshold
   #:runtime-config-context-compact-suggested-threshold
   #:runtime-config-context-compact-required-threshold
   #:runtime-config-idle-gap-microcompact-seconds
   #:runtime-config-idle-gap-minimum-usage-ratio
   #:runtime-config-reactive-compaction-max-retries
   #:runtime-config-reactive-compaction-aggressive-keep-recent
   ;; Phase 10: Compaction IR Settings
   #:runtime-config-compaction-summary-token-budget
   ;; Phase 5: Session Memory Settings
   #:runtime-config-session-memory-budget-chars
   #:runtime-config-session-memory-update-token-growth-threshold
   #:runtime-config-session-memory-update-tool-activity-threshold
   #:runtime-config-session-memory-idle-update-seconds
   #:runtime-config-session-memory-max-staleness-seconds
   ;; Phase 6: Durable Memory Settings
   #:runtime-config-durable-memory-enabled-p
   #:runtime-config-durable-user-memory-budget-chars
   #:runtime-config-durable-feedback-memory-budget-chars
   #:runtime-config-durable-project-memory-budget-chars
   #:runtime-config-durable-reference-memory-budget-chars
   #:runtime-config-durable-memory-max-records-per-kind
   #:runtime-config-durable-memory-max-record-age-days
   #:runtime-config-durable-memory-enabled-p
   #:runtime-config-durable-memory-include-in-prompts-p
   #:runtime-config-durable-memory-max-context-chars
   ;; Phase 7: Embedding Configuration
   #:runtime-config-embedding-enabled-p
   #:runtime-config-embedding-provider
   #:runtime-config-embedding-model
   #:runtime-config-embedding-max-batch-size
   #:runtime-config-embedding-timeout-seconds
   #:runtime-config-embedding-log-errors-p
   #:runtime-config-embedding-default-dimension
   #:runtime-config-embedding-max-text-length
   ;; Phase 7: Semantic Search Configuration
   #:runtime-config-semantic-search-enabled-p
   #:runtime-config-semantic-search-default-limit
   #:runtime-config-semantic-search-max-candidates
   #:runtime-config-semantic-search-min-score
   #:runtime-config-semantic-search-hybrid-weight
   ;; Phase 7 Task 6: Durable Memory Query Configuration
   #:durable-memory-query-config
   #:make-durable-memory-query-config
   #:make-default-dmq-config
   #:runtime-config-durable-memory-query-config
   #:dmq-config-max-results
   #:dmq-config-min-relevance-score
   #:dmq-config-default-query-mode
   #:dmq-config-semantic-weight-by-kind
   #:dmq-config-max-injection-chars
   #:dmq-config-injection-enabled
   #:dmq-config-dedup-window-normal
   #:dmq-config-dedup-window-important
   #:dmq-config-importance-threshold
   #:dmq-config-evergreen-kinds
   #:dmq-config-embedding-failure-threshold
   #:dmq-config-embedding-cooldown-seconds
   #:current-dmq-config
   #:*dmq-active-config*
   ;; Struct constructor
   #:make-runtime-config
   ;; Phase 7: Current embedding accessors
   #:*current-embedding-provider*
   #:*current-embedding-model*
   #:*current-embedding-max-batch-size*
   #:*current-embedding-enabled-p*
   #:current-embedding-provider
   #:current-embedding-model
   #:current-embedding-max-batch-size
   #:current-embedding-enabled-p
   #:runtime-config-provider-credentials
   ;; Credential structs
   #:provider-credentials
   #:provider-credentials-api-key
   #:provider-credentials-base-url
   #:anthropic-credentials
   #:anthropic-credentials-api-version
   #:openrouter-credentials
   #:openai-credentials
   #:openai-credentials-organization
   #:bedrock-credentials
   #:bedrock-credentials-access-key
   #:bedrock-credentials-secret-key
   #:bedrock-credentials-region
   #:bedrock-credentials-profile
   #:make-anthropic-credentials
   #:make-openrouter-credentials
   #:make-openai-credentials
   #:%make-bedrock-credentials
   ;; Accessors
   #:config-credentials
   ;; Functions
   #:load-runtime-config
   #:find-config-file
   #:apply-state-root
   #:runtime-config-compatibility-root
   #:redact-key
   ;; Backward compatibility
   #:make-default-runtime-config))

(defpackage #:claw-lisp.core.domain
  (:use #:cl)
  (:export
   #:model-capabilities
   #:model-capabilities-name
   #:model-capabilities-provider
   #:model-capabilities-context-window
   #:model-capabilities-max-output-tokens
   #:model-capabilities-default-output-tokens
   #:model-capabilities-tools-p
   #:model-capabilities-streaming-p
   #:model-capabilities-thinking-p
   #:model-capabilities-adaptive-thinking-p
   #:model-capabilities-json-output-p
   #:model-capabilities-vision-p
   #:model-capabilities-prompt-caching-p
   #:model-capabilities-input-price-per-mtok
   #:model-capabilities-output-price-per-mtok
   #:model-capabilities-cache-read-price-per-mtok
   #:model-capabilities-cache-write-price-per-mtok
   #:make-model-capabilities
   #:transport-response
   #:transport-response-ok-p
   #:transport-response-status
   #:transport-response-assistant-text
   #:transport-response-raw-response
   #:transport-response-error-message
   #:transport-response-provider
   #:transport-response-metadata
   #:transport-response-tool-calls
   #:make-transport-response
   ;; Content blocks
   #:content-block
   #:content-block-p
   #:text-block
   #:text-block-p
   #:text-block-text
   #:make-text-block
   #:tool-use-block
   #:tool-use-block-p
   #:tool-use-block-id
   #:tool-use-block-name
   #:tool-use-block-input
   #:make-tool-use-block
   #:tool-result-block
   #:tool-result-block-p
   #:tool-result-block-tool-use-id
   #:tool-result-block-content
   #:tool-result-block-is-error
   #:make-tool-result-block
   #:thinking-block
   #:thinking-block-p
   #:thinking-block-thinking
   #:thinking-block-signature
   #:make-thinking-block
   ;; Message
   #:message
   #:message-role
   #:message-content
   #:message-metadata
   #:make-message
   #:message-content-text
   #:message-tool-use-blocks
   ;; Tool call
   #:tool-call
   #:tool-call-id
   #:tool-call-name
   #:tool-call-input
   #:tool-call-metadata
   #:make-tool-call
   #:tool-use-block->tool-call
   ;; Tool result
  #:tool-result
  #:tool-result-call-id
  #:tool-result-tool-name
  #:tool-result-content
  #:tool-result-persisted-path
  #:tool-result-truncated-p
  #:tool-result-bytes
  #:tool-result-cas-hash
  #:tool-result-cas-type
  #:tool-result-cas-ref-name
  #:tool-result-artifact
  #:make-tool-result
  ;; CAS artifact handle
  #:artifact
  #:artifact-id
  #:artifact-kind
  #:artifact-cas-hash
  #:artifact-cas-type
  #:artifact-cas-ref-name
  #:artifact-metadata
  #:make-artifact
  #:stream-accumulator
   #:stream-accumulator-message-id
   #:stream-accumulator-model
   #:stream-accumulator-text
   #:stream-accumulator-tool-use-blocks
   #:stream-accumulator-current-tool-use
   #:stream-accumulator-stop-reason
   #:stream-accumulator-stop-sequence
   #:stream-accumulator-usage
   #:stream-accumulator-done
   #:stream-accumulator-on-event
   #:make-stream-accumulator
   #:compaction-result
   #:compaction-result-source
   #:compaction-result-summary
   #:compaction-result-ir
   #:compaction-result-preserved-messages
   #:make-compaction-result
   ;; Phase 10: Structured Compaction IR
   #:compaction-ir
   #:make-compaction-ir
   #:compaction-ir-id
   #:compaction-ir-source
   #:compaction-ir-created-universal-time
   #:compaction-ir-session-id
   #:compaction-ir-predecessor-fingerprint
   #:compaction-ir-provenance
   #:compaction-ir-sections
   #:compaction-ir-token-budget
   #:compaction-ir-tokens-used
   #:compaction-ir-provenance
   #:make-compaction-ir-provenance
   #:compaction-ir-provenance-session-memory-used-p
   #:compaction-ir-provenance-uncovered-messages-count
   #:compaction-ir-provenance-summarized-messages-count
   #:compaction-ir-provenance-preserved-tail-count
   #:compaction-ir-provenance-total-messages-before
   #:compaction-ir-provenance-tool-results-summarized-count
   #:compaction-ir-provenance-compaction-depth
   #:compaction-ir-section
   #:make-compaction-ir-section
   #:compaction-ir-section-kind
   #:compaction-ir-section-heading
   #:compaction-ir-section-items
   #:compaction-ir-section-tokens-estimated
   #:compaction-ir-section-trimmed-p
   #:compaction-ir-section-priority
   #:compaction-ir-item
   #:make-compaction-ir-item
   #:compaction-ir-item-type
   #:compaction-ir-item-text
   #:compaction-ir-item-role
   #:compaction-ir-item-tool-name
   #:compaction-ir-item-persisted-path
   #:compaction-ir-item-call-id
   #:compaction-ir-item-bytes
   #:compaction-ir-item-message-index
   #:conversation
   #:conversation-id
   #:conversation-messages
   #:conversation-tool-results
   #:conversation-metadata
   #:make-conversation
   #:append-message
   #:record-tool-result
   #:replace-tool-results
   #:agent-turn
   #:agent-turn-content
   #:agent-turn-metadata
   #:agent-turn-messages
   #:agent-turn-tool-results
   #:make-agent-turn
   #:agent-session
   #:agent-session-id
   #:agent-session-provider
   #:agent-session-model
   #:agent-session-conversation
   #:agent-session-state
   #:make-agent-session
   ;; Phase 8: Multi-agent domain structs
   #:agent-envelope
   #:agent-envelope-id
   #:agent-envelope-from-agent-id
   #:agent-envelope-to-agent-id
   #:agent-envelope-type
   #:agent-envelope-payload
   #:agent-envelope-correlation-id
   #:agent-envelope-reply-to-id
   #:agent-envelope-created-universal-time
   #:agent-envelope-deadline-universal-time
   #:agent-envelope-attempt
   #:make-agent-envelope
   #:agent-mailbox-state
   #:agent-mailbox-state-mailbox-id
   #:agent-mailbox-state-owner-agent-id
   #:agent-mailbox-state-buffer
   #:agent-mailbox-state-head-index
   #:agent-mailbox-state-tail-index
   #:agent-mailbox-state-count
   #:agent-mailbox-state-max-depth
   #:agent-mailbox-state-backpressure-mode
   #:agent-mailbox-state-closed-p
   #:agent-mailbox-state-dropped-count
   #:agent-mailbox-state-dead-letter-queue
   #:agent-mailbox-state-mutex
   #:agent-mailbox-state-waitqueue
   #:make-agent-mailbox-state
   #:child-agent-spec
   #:child-agent-spec-child-id
   #:child-agent-spec-provider-name
   #:child-agent-spec-model
   #:child-agent-spec-initial-user-message
   #:child-agent-spec-timeout-seconds
   #:child-agent-spec-supervisor-policy
   #:child-agent-spec-metadata
   #:make-child-agent-spec
   #:child-agent-handle
   #:child-agent-handle-child-id
   #:child-agent-handle-parent-id
   #:child-agent-handle-session
   #:child-agent-handle-thread
   #:child-agent-handle-mailbox
   #:child-agent-handle-status
   #:child-agent-handle-started-universal-time
   #:child-agent-handle-finished-universal-time
   #:child-agent-handle-last-error
   #:child-agent-handle-restart-count
   #:child-agent-handle-start-order
   #:make-child-agent-handle
   #:agent-supervisor-state
   #:agent-supervisor-state-supervisor-id
   #:agent-supervisor-state-parent-session-id
   #:agent-supervisor-state-policy
   #:agent-supervisor-state-children
   #:agent-supervisor-state-mailbox
   #:agent-supervisor-state-max-restarts
   #:agent-supervisor-state-restart-window-seconds
   #:agent-supervisor-state-restart-events
   #:agent-supervisor-state-mutex
   #:make-agent-supervisor-state
   #:child-progress-snapshot
   #:child-progress-snapshot-child-id
   #:child-progress-snapshot-status
   #:child-progress-snapshot-summary-text
   #:child-progress-snapshot-last-updated-universal-time
   #:child-progress-snapshot-tool-calls-count
   #:child-progress-snapshot-messages-count
   #:make-child-progress-snapshot
   #:session-current-turn-id
   #:session-memory-injection-log
   #:tool-result->plist))

(defpackage #:claw-lisp.core.protocols
  (:use #:cl)
  (:import-from #:claw-lisp.core.domain
                #:conversation
                #:message
                #:tool-call
                #:tool-result)
  (:export
   #:provider
   #:provider-name
   #:provider-api-key
   #:provider-base-url
   #:provider-model-registry
   #:send-turn
   #:stream-turn
   #:count-tokens
   #:normalize-response
   #:tool
   #:tool-name
   #:tool-description
   #:tool-input-schema
   #:validate-tool-input
   #:execute-tool
   #:normalize-tool-result))

(defpackage #:claw-lisp.providers.http-json
  (:use #:cl)
  (:export
   #:value->json-safe
   #:plist-to-json-object))

(defpackage #:claw-lisp.storage.transcripts
  (:use #:cl)
  (:import-from #:claw-lisp.config
                #:runtime-config
                #:runtime-config-transcripts-root)
  (:import-from #:claw-lisp.core.domain
                #:agent-session-conversation
                #:agent-session-id
                #:agent-session-model
                #:agent-session-provider
                #:conversation-id)
  (:import-from #:claw-lisp.providers.http-json
                #:value->json-safe)
  (:export
   #:transcript-path-for-session
   #:transcript-existing-path-for-session
   #:ensure-session-transcript
   #:append-transcript-event))

(defpackage #:claw-lisp.storage.tool-results
  (:use #:cl)
  (:import-from #:claw-lisp.config
                #:runtime-config-artifacts-root
                #:runtime-config-tool-preview-bytes)
  (:import-from #:claw-lisp.core.domain
                #:agent-session-id
                #:make-tool-result
                #:tool-result-artifact
                #:tool-result-cas-hash
                #:tool-result-cas-ref-name
                #:tool-result-cas-type
                #:tool-result-bytes
                #:tool-result-call-id
                #:tool-result-content
                #:tool-result-persisted-path
                #:tool-result-tool-name
                #:tool-result-truncated-p)
  (:export
   #:delete-session-tool-results
   #:read-persisted-tool-result
   #:store-tool-result))

(defpackage #:claw-lisp.storage.cas
  (:use #:cl)
  (:import-from #:claw-lisp.config
                #:runtime-config-cas-objects-root)
  (:export
   #:cas-error
   #:cas-invalid-hash-error
   #:cas-write-error
   #:+hash-algorithm-prefix+
   #:cas-hash
   #:cas-hash-bytes
   #:valid-versioned-hash-p
   #:parse-versioned-hash
   #:hash-algorithm
   #:hash-digest
   #:hash-shard-prefix
   #:hash-shard-remainder
   #:cas-object-path
   #:cas-put
   #:cas-put-bytes
   #:cas-get
   #:cas-get-bytes
   #:cas-cleanup-temp-files
   #:cas-delete
   #:cas-exists-p
   #:valid-versioned-hash-p
   #:canonicalize-plist))

(defpackage #:claw-lisp.storage.cas-ref
  (:use #:cl)
  (:import-from #:claw-lisp.storage.cas
                #:valid-versioned-hash-p
                #:cas-exists-p
                #:canonicalize-plist)
  (:export
   #:cas-ref-error
   #:cas-ref-invalid-name-error
   #:cas-ref-invalid-name-error-name
   #:cas-ref-invalid-hash-error
   #:cas-ref-invalid-hash-error-name
   #:cas-ref-invalid-hash-error-hash
   #:cas-ref-conflict-error
   #:cas-ref-conflict-error-name
   #:cas-ref-conflict-error-expected
   #:cas-ref-conflict-error-actual
   #:cas-ref-dangling-error
   #:cas-ref-dangling-error-name
   #:cas-ref-dangling-error-hash
   #:valid-cas-ref-name-p
   #:cas-ref-path
   #:read-cas-ref
   #:write-cas-ref
   #:delete-cas-ref
   #:list-cas-refs
   #:resolve-cas-ref))

(defpackage #:claw-lisp.core.artifacts
  (:use #:cl)
  ;; `artifact` is a domain type; this package is the CAS facade and re-exports
  ;; the domain handle so callers can use the higher-level storage API directly.
  (:import-from #:claw-lisp.core.domain
                #:artifact
                #:artifact-cas-hash
                #:artifact-cas-ref-name
                #:artifact-cas-type
                #:artifact-id
                #:artifact-kind
                #:artifact-metadata
                #:make-artifact
                #:tool-result
                #:tool-result-artifact
                #:tool-result-bytes
                #:tool-result-call-id
                #:tool-result-content
                #:tool-result-cas-hash
                #:tool-result-cas-ref-name
                #:tool-result-cas-type
                #:tool-result-persisted-path
                #:tool-result-tool-name
                #:tool-result-truncated-p
                #:make-tool-result)
  (:import-from #:claw-lisp.config
                #:runtime-config-cas-objects-root
                #:runtime-config-cas-ref-root
                #:runtime-config-tool-result-dedup-p)
  (:import-from #:claw-lisp.storage.cas
                #:cas-exists-p
                #:cas-get
                #:cas-hash
                #:cas-put
                #:canonicalize-plist
                #:hash-digest
                #:valid-versioned-hash-p)
  (:import-from #:claw-lisp.storage.cas-ref
                #:resolve-cas-ref
                #:write-cas-ref)
  (:export
   #:artifact
   #:artifact-id
   #:artifact-kind
   #:artifact-cas-hash
   #:artifact-cas-type
   #:artifact-cas-ref-name
   #:artifact-metadata
   #:make-artifact
   #:tool-result->artifact
   #:compute-tool-result-dedup-key
   #:clear-tool-result-dedup-index
   #:tool-result-dedup-index-size
   #:runtime-effective-cas-root
   #:runtime-effective-cas-ref-root
   #:legacy-path-cas-ref-name
   #:store-tool-result-cas
   #:ensure-tool-result-cas-compatibility
   #:resolve-tool-result-cas
   #:migrate-legacy-artifact-path-to-cas
   #:migrate-legacy-tool-results-to-cas
   #:persist-artifact-to-cas
   #:resolve-artifact-from-cas))

(defpackage #:claw-lisp.ir.conditions
  (:use #:cl)
  (:export
   #:ir-error
   #:ir-storage-error
   #:ir-storage-error-operation
   #:ir-storage-error-reason
   #:ir-serialization-error
   #:ir-serialization-error-object
   #:ir-serialization-error-reason
   #:ir-deserialization-error
   #:ir-deserialization-error-payload
   #:ir-deserialization-error-reason
   #:ir-validation-error
   #:ir-validation-error-subject
   #:ir-validation-error-reason
   #:ir-version-mismatch-error
   #:ir-version-mismatch-error-expected
   #:ir-version-mismatch-error-actual
   #:ir-version-mismatch-error-object-type))

(defpackage #:claw-lisp.ir.schema
  (:use #:cl)
  (:import-from #:claw-lisp.core.domain
                #:artifact
                #:artifact-cas-hash
                #:artifact-cas-ref-name
                #:artifact-cas-type
                #:artifact-id
                #:artifact-kind
                #:artifact-metadata
                #:make-artifact)
  (:import-from #:claw-lisp.storage.cas
                #:canonicalize-plist)
  (:import-from #:claw-lisp.ir.conditions
                #:ir-deserialization-error
                #:ir-serialization-error
                #:ir-version-mismatch-error)
  (:export
   #:+default-ir-version+
   #:ir-node
   #:ir-node-p
   #:ir-node-id
   #:ir-node-kind
   #:ir-node-payload
   #:ir-node-metadata
   #:make-ir-node
   #:ir-edge
   #:ir-edge-p
   #:ir-edge-from-id
   #:ir-edge-to-id
   #:ir-edge-kind
   #:ir-edge-metadata
   #:make-ir-edge
   #:ir-graph
   #:ir-graph-p
   #:ir-graph-id
   #:ir-graph-ir-version
   #:ir-graph-node-type
   #:ir-graph-nodes
   #:ir-graph-edges
   #:ir-graph-metadata
   #:make-ir-graph
   #:make-semantic-ir
   #:make-validated-ir
   #:make-optimized-ir
   #:make-execution-plan-ir
   #:ir-object->canonical-sexp
   #:ir-object->canonical-string
   #:canonical-sexp->ir-object))

(defpackage #:claw-lisp.ir.cas-bridge
  (:use #:cl)
  (:import-from #:claw-lisp.core.domain
                #:artifact
                #:artifact-cas-hash
                #:artifact-cas-ref-name
                #:artifact-id
                #:artifact-kind
                #:artifact-metadata
                #:make-artifact)
  (:import-from #:claw-lisp.core.artifacts
                #:runtime-effective-cas-root
                #:runtime-effective-cas-ref-root)
  (:import-from #:claw-lisp.storage.cas
                #:cas-get
                #:cas-put
                #:valid-versioned-hash-p
                #:canonicalize-plist)
  (:import-from #:claw-lisp.storage.cas-ref
                #:read-cas-ref
                #:resolve-cas-ref
                #:valid-cas-ref-name-p
                #:write-cas-ref)
  (:import-from #:claw-lisp.ir.schema
                #:ir-graph
                #:ir-graph-id
                #:ir-graph-ir-version
                #:ir-graph-node-type
                #:ir-node
                #:ir-node-id
                #:ir-object->canonical-string
                #:canonical-sexp->ir-object)
  (:import-from #:claw-lisp.ir.conditions
                #:ir-deserialization-error
                #:ir-storage-error
                #:ir-version-mismatch-error)
  (:export
   #:+max-ir-payload-chars+
   #:materialize-ir-to-cas
   #:load-ir-from-cas
   #:resolve-ir-from-cas
   #:resolve-ir-ref))

(defpackage #:claw-lisp.cas.crypto
  (:use #:cl)
  (:export
   #:sign-manifest-root
   #:verify-manifest-root-signature
   #:*manifest-signing-key*))

(defpackage #:claw-lisp.cas.manifest
  (:use #:cl)
  (:import-from #:claw-lisp.storage.cas
                #:cas-put
                #:cas-get
                #:cas-hash
                #:cas-exists-p
                #:canonicalize-plist)
  (:export
   #:manifest
   #:manifest-entry
   #:cas-manifest-error
   #:cas-manifest-integrity-error
   #:cas-manifest-signature-error
   #:cas-manifest-parse-error
   #:cas-manifest-parse-error-text
   #:cas-manifest-parse-error-reason
   #:make-manifest
   #:make-manifest-entry
   ;; manifest-entry accessors
   #:manifest-entry-role
   #:manifest-entry-cas-hash
   #:manifest-entry-type
   #:manifest-entry-metadata
   ;; manifest accessors
   #:manifest-root-digest
   #:manifest-entries
   #:manifest-metadata
   #:manifest-signature
   #:serialize-manifest
   #:deserialize-manifest
   #:compute-manifest-root-digest
   #:store-manifest
   #:load-manifest
   #:verify-manifest-integrity))

(defpackage #:claw-lisp.ir.validate
  (:use #:cl)
  (:import-from #:claw-lisp.storage.cas
                #:valid-versioned-hash-p)
  (:import-from #:claw-lisp.ir.schema
                #:ir-graph-p
                #:ir-graph-id
                #:ir-graph-node-type
                #:ir-graph-metadata
                #:ir-graph-nodes
                #:ir-graph-edges
                #:ir-node-p
                #:ir-node-id
                #:ir-node-kind
                #:ir-edge-p
                #:ir-edge-from-id
                #:ir-edge-to-id
                #:ir-edge-kind)
  (:import-from #:claw-lisp.ir.conditions
                #:ir-validation-error)
  (:export
   #:+allowed-achatina-pipeline-stages+
   #:validate-achatina-ir-graph))

(defpackage #:claw-lisp.ir.expander
  (:use #:cl)
  (:import-from #:claw-lisp.core.domain
                #:artifact-cas-hash
                #:artifact-cas-ref-name
                #:artifact-cas-type
                #:artifact-id
                #:artifact-kind
                #:artifact-metadata
                #:make-artifact)
  (:import-from #:claw-lisp.core.artifacts
                #:runtime-effective-cas-root
                #:runtime-effective-cas-ref-root)
  (:import-from #:claw-lisp.storage.cas
                #:canonicalize-plist)
  (:import-from #:claw-lisp.storage.cas-ref
                #:write-cas-ref)
  (:import-from #:claw-lisp.cas.manifest
                #:make-manifest
                #:make-manifest-entry
                #:serialize-manifest
                #:store-manifest)
  (:import-from #:claw-lisp.ir.schema
                #:ir-graph-id
                #:ir-graph-ir-version
                #:ir-graph-node-type
                #:ir-graph-metadata
                #:ir-graph-nodes
                #:ir-graph-edges
                #:make-ir-graph
                #:make-validated-ir)
  (:import-from #:claw-lisp.ir.validate
                #:validate-achatina-ir-graph)
  (:import-from #:claw-lisp.ir.cas-bridge
                #:materialize-ir-to-cas)
  (:export
   #:ir-pass-context
   #:ir-pass-context-p
   #:ir-pass-context-identity
   #:ir-pass-context-capabilities
   #:ir-pass-context-policies
   #:ir-pass-context-tools
   #:ir-pass-context-memory-scopes
   #:ir-pass-context-parent-ir-hash
   #:ir-pass-context-parent-ir-ref-name
   #:ir-pass-context-metadata
   #:make-ir-pass-context
   #:ir-pass-result
   #:ir-pass-result-p
   #:ir-pass-result-pass-name
   #:ir-pass-result-input-stage
   #:ir-pass-result-output-stage
   #:ir-pass-result-graph
   #:ir-pass-result-metadata
   #:ir-pipeline-step
   #:ir-pipeline-step-p
   #:ir-pipeline-step-pass-name
   #:ir-pipeline-step-output-stage
   #:ir-pipeline-step-transform-fn
   #:ir-pipeline-step-ref-name
   #:ir-pipeline-step-manifest-ref-name
   #:ir-pipeline-step-metadata
   #:make-ir-pipeline-step
   #:ir-pipeline-run
   #:ir-pipeline-run-p
   #:ir-pipeline-run-results
   #:ir-pipeline-run-persisted-results
   #:ir-pipeline-run-final-result
   #:ir-pipeline-run-final-graph-artifact
   #:ir-pipeline-run-final-manifest-artifact
   #:run-achatina-ir-pass
   #:run-achatina-ir-pipeline
   #:prepare-validated-ir
   #:persist-ir-pass-result))

(defpackage #:claw-lisp.ir.optimize
  (:use #:cl)
  (:import-from #:claw-lisp.ir.schema
                #:ir-graph-id
                #:ir-graph-ir-version
                #:ir-graph-metadata
                #:ir-graph-nodes
                #:ir-graph-edges
                #:ir-node-id
                #:ir-node-kind
                #:ir-node-metadata
                #:ir-node-payload
                #:ir-edge-from-id
                #:ir-edge-to-id
                #:ir-edge-kind
                #:ir-edge-metadata
                #:make-optimized-ir)
  (:import-from #:claw-lisp.ir.expander
                #:ir-pipeline-run-final-result
                #:make-ir-pipeline-step
                #:run-achatina-ir-pipeline)
  (:export
   #:make-default-optimization-pipeline-steps
   #:optimize-validated-ir-pipeline
   #:optimize-validated-ir))

(defpackage #:claw-lisp.ir.semantic
  (:use #:cl)
  (:import-from #:claw-lisp.ir.schema
                #:ir-graph-id
                #:ir-graph-ir-version
                #:ir-graph-metadata
                #:ir-graph-nodes
                #:ir-graph-edges
                #:ir-node-id
                #:ir-node-kind
                #:ir-node-payload
                #:make-ir-node
                #:make-ir-edge
                #:make-semantic-ir)
  (:import-from #:claw-lisp.ir.expander
                #:ir-pipeline-run-final-result
                #:make-ir-pipeline-step
                #:run-achatina-ir-pipeline)
  (:export
   #:make-default-semantic-expansion-steps
   #:expand-semantic-ir-pipeline
   #:expand-semantic-ir))

(defpackage #:claw-lisp.ir.prepare
  (:use #:cl)
  (:import-from #:claw-lisp.ir.schema
                #:ir-graph-id
                #:ir-graph-metadata
                #:ir-graph-nodes
                #:ir-graph-edges
                #:ir-node-id
                #:ir-node-kind
                #:ir-node-payload
                #:ir-edge-from-id
                #:ir-edge-to-id
                #:ir-edge-kind)
  (:import-from #:claw-lisp.ir.validate
                #:validate-achatina-ir-graph)
  (:import-from #:claw-lisp.ir.conditions
                #:ir-validation-error)
  (:export
   #:execution-preparation
   #:execution-preparation-p
   #:execution-preparation-workflow-id
   #:execution-preparation-pipeline-stage
   #:execution-preparation-tools
   #:execution-preparation-memory-scopes
   #:execution-preparation-capabilities
   #:execution-preparation-policies
   #:execution-preparation-parent-ir-hash
   #:execution-preparation-parent-ir-ref-name
   #:execution-preparation-graph
   #:prepare-execution-input))

(defpackage #:claw-lisp.ir.execution-plan
  (:use #:cl)
  (:import-from #:claw-lisp.ir.schema
                #:ir-graph-id
                #:ir-graph-ir-version
                #:ir-graph-nodes
                #:ir-graph-edges
                #:ir-node-id
                #:ir-node-kind
                #:ir-node-payload
                #:ir-node-metadata
                #:ir-edge-from-id
                #:ir-edge-to-id
                #:ir-edge-kind
                #:ir-edge-metadata
                #:make-ir-node
                #:make-ir-edge
                #:make-execution-plan-ir)
  (:import-from #:claw-lisp.ir.prepare
                #:execution-preparation
                #:execution-preparation-p
                #:execution-preparation-workflow-id
                #:execution-preparation-pipeline-stage
                #:execution-preparation-tools
                #:execution-preparation-memory-scopes
                #:execution-preparation-capabilities
                #:execution-preparation-policies
                #:execution-preparation-parent-ir-hash
                #:execution-preparation-parent-ir-ref-name
                #:execution-preparation-graph
                #:prepare-execution-input)
  (:import-from #:claw-lisp.ir.expander
                #:ir-pass-context
                #:ir-pass-context-identity
                #:ir-pass-context-capabilities
                #:ir-pass-context-policies
                #:ir-pass-context-tools
                #:ir-pass-context-memory-scopes
                #:ir-pass-context-parent-ir-hash
                #:ir-pass-context-parent-ir-ref-name
                #:ir-pass-context-metadata
                #:make-ir-pass-context
                #:ir-pass-result
                #:ir-pass-result-graph
                #:ir-pipeline-run-final-result
                #:run-achatina-ir-pass
                #:persist-ir-pass-result)
  (:import-from #:claw-lisp.ir.conditions
                #:ir-validation-error)
  (:export
   #:lower-to-execution-plan
   #:persist-execution-plan))

(defpackage #:claw-lisp.ir.surface-form
  (:use #:cl)
  (:import-from #:claw-lisp.ir.schema
                #:make-semantic-ir
                #:make-ir-node
                #:make-ir-edge)
  (:import-from #:claw-lisp.ir.conditions
                #:ir-validation-error)
  (:import-from #:claw-lisp.ir.expander
                #:make-ir-pass-context)
  (:export
   #:compile-minimal-workflow-form
   #:make-workflow-form-context))

(defpackage #:claw-lisp.ir.local-execution
  (:use #:cl)
  (:import-from #:claw-lisp.ir.schema
                #:ir-graph-id
                #:ir-graph-node-type
                #:ir-graph-nodes
                #:ir-graph-edges
                #:ir-node-id
                #:ir-node-kind
                #:ir-node-payload
                #:ir-edge-from-id
                #:ir-edge-to-id
                #:ir-edge-kind)
  (:import-from #:claw-lisp.ir.conditions
                #:ir-validation-error)
  (:import-from #:claw-lisp.ir.surface-form
                #:compile-minimal-workflow-form
                #:make-workflow-form-context)
  (:import-from #:claw-lisp.ir.cas-bridge
                #:materialize-ir-to-cas
                #:load-ir-from-cas)
  (:import-from #:claw-lisp.ir.expander
                #:prepare-validated-ir
                #:ir-pass-result-graph
                #:ir-pipeline-run-final-result
                #:ir-pipeline-run-final-graph-artifact
                #:persist-ir-pass-result)
  (:import-from #:claw-lisp.ir.optimize
                #:optimize-validated-ir-pipeline)
  (:import-from #:claw-lisp.ir.semantic
                #:expand-semantic-ir)
  (:import-from #:claw-lisp.ir.execution-plan
                #:persist-execution-plan)
  (:import-from #:claw-lisp.core.domain
                #:artifact-cas-hash
                #:artifact-cas-ref-name
                #:tool-result-content)
  (:export
   #:execute-execution-plan-locally
   #:run-workflow-form-locally))

(defpackage #:claw-lisp.ir.compaction
  (:use #:cl)
  (:import-from #:claw-lisp.core.domain
                #:artifact-cas-hash
                #:artifact-cas-ref-name
                #:agent-session-id
                #:compaction-result
                #:compaction-result-ir
                #:compaction-result-source
                #:compaction-ir
                #:compaction-ir-id
                #:compaction-ir-source
                #:compaction-ir-created-universal-time
                #:compaction-ir-session-id
                #:compaction-ir-predecessor-fingerprint
                #:compaction-ir-provenance
                #:compaction-ir-sections
                #:compaction-ir-token-budget
                #:compaction-ir-tokens-used
                #:compaction-ir-provenance-session-memory-used-p
                #:compaction-ir-provenance-uncovered-messages-count
                #:compaction-ir-provenance-summarized-messages-count
                #:compaction-ir-provenance-preserved-tail-count
                #:compaction-ir-provenance-total-messages-before
                #:compaction-ir-provenance-tool-results-summarized-count
                #:compaction-ir-provenance-compaction-depth
                #:compaction-ir-section-kind
                #:compaction-ir-section-heading
                #:compaction-ir-section-items
                #:compaction-ir-section-tokens-estimated
                #:compaction-ir-section-trimmed-p
                #:compaction-ir-section-priority
                #:compaction-ir-item-type
                #:compaction-ir-item-text
                #:compaction-ir-item-role
                #:compaction-ir-item-tool-name
                #:compaction-ir-item-persisted-path
                #:compaction-ir-item-call-id
                #:compaction-ir-item-bytes
                #:compaction-ir-item-message-index
                #:make-artifact)
  (:import-from #:claw-lisp.core.artifacts
                #:persist-artifact-to-cas
                #:runtime-effective-cas-root
                #:runtime-effective-cas-ref-root)
  (:import-from #:claw-lisp.cas.manifest
                #:make-manifest
                #:make-manifest-entry
                #:store-manifest)
  (:import-from #:claw-lisp.storage.cas-ref
                #:write-cas-ref)
  (:import-from #:claw-lisp.ir.schema
                #:make-ir-edge
                #:make-ir-graph
                #:make-ir-node)
  (:import-from #:claw-lisp.ir.cas-bridge
                #:materialize-ir-to-cas)
  (:export
   #:compaction-domain-ir->graph
   #:persist-compaction-result-ir))

(defpackage #:claw-lisp.cas.integrity
  (:use #:cl)
  (:import-from #:claw-lisp.storage.cas
                #:cas-exists-p
                #:cas-get-bytes
                #:cas-hash-bytes)
  (:import-from #:claw-lisp.storage.cas-ref
                #:read-cas-ref)
  (:import-from #:claw-lisp.cas.manifest
                #:load-manifest
                #:manifest-entries
                #:manifest-entry-cas-hash
                #:verify-manifest-integrity)
  (:export
   #:cas-integrity-error
   #:cas-integrity-missing-object-error
   #:cas-integrity-missing-object-error-hash
   #:cas-integrity-corrupt-object-error
   #:cas-integrity-corrupt-object-error-hash
   #:cas-integrity-corrupt-object-error-expected
   #:cas-integrity-corrupt-object-error-actual
   #:cas-integrity-missing-ref-error
   #:cas-integrity-missing-ref-error-name
   #:cas-integrity-dangling-ref-error
   #:cas-integrity-dangling-ref-error-name
   #:cas-integrity-dangling-ref-error-hash
   #:cas-integrity-manifest-error
   #:cas-integrity-manifest-error-cause
   #:integrity-failure
   #:integrity-failure-kind
   #:integrity-failure-subject
   #:integrity-failure-expected
   #:integrity-failure-actual
   #:integrity-failure-context
   #:integrity-report
   #:integrity-report-target-kind
   #:integrity-report-target
   #:integrity-report-verified-count
   #:integrity-report-failures
   #:integrity-report-metadata
   #:integrity-report-failure-count
   #:integrity-report-ok-p
   #:verify-cas-object-integrity
   #:verify-cas-ref-integrity
   #:verify-manifest-graph-integrity))

(defpackage #:claw-lisp.storage.session-memory
  (:use #:cl)
  (:import-from #:claw-lisp.config
                #:runtime-config-memory-root)
  (:import-from #:claw-lisp.core.domain
                #:agent-session-conversation
                #:agent-session-id
                #:agent-session-model
                #:agent-session-provider
                #:conversation-messages
                #:conversation-tool-results
                #:message-content-text
                #:message-role
                #:tool-result-content
                #:tool-result-call-id
                #:tool-result-persisted-path
                #:tool-result-tool-name)
  (:export
   ;; Session memory metadata (Phase 5 Task 1)
   #:session-memory-metadata
   #:make-session-memory-metadata
   #:session-memory-metadata-update-count
   #:session-memory-metadata-last-updated-universal-time
   #:session-memory-metadata-budget-chars-used
   #:session-memory-metadata-budget-chars-max
   #:session-memory-metadata-stale-p
   #:session-memory-metadata-tokens-at-last-update
   #:session-memory-metadata-tool-count-at-last-update
   #:parse-session-memory-header
   ;; Phase 5 Task 3: Update triggers
   #:session-memory-update-trigger
   ;; Phase 5 Task 4: Budget controls
   #:within-budget-p
   #:enforce-session-memory-budget
   ;; Phase 5 Task 5: Staleness detection
   #:session-memory-stale-p
   ;; Existing exports
   #:render-session-memory
   #:render-session-memory-structured
   #:session-memory-path
   #:session-memory-existing-path
   #:session-memory-structured-path
   #:update-session-memory))

(defpackage #:claw-lisp.storage.durable-memory
  (:use #:cl)
  (:export
   ;; Domain model (Phase 6 Task 1)
   #:durable-memory-record
   #:make-durable-memory-record
   #:copy-durable-memory-record
   #:durable-memory-record-id
   #:durable-memory-record-kind
   #:durable-memory-record-subject-id
   #:durable-memory-record-title
   #:durable-memory-record-content
   #:durable-memory-record-source
   #:durable-memory-record-created-universal-time
   #:durable-memory-record-updated-universal-time
   #:durable-memory-record-importance-score
   #:durable-memory-record-staleness-score
   #:durable-memory-record-last-accessed-universal-time
   #:durable-memory-record-tags
   #:durable-memory-record-version
   #:durable-memory-record-supersedes-id
   #:durable-memory-record-superseded-by-id
   ;; Phase 7: Embedding fields
   #:durable-memory-record-embedding
   #:durable-memory-record-embedding-model
   #:durable-memory-record-embedding-version
   #:durable-memory-kind-p
   #:*durable-memory-kinds*
   #:make-user-memory
   #:make-feedback-memory
   #:make-project-memory
   #:make-reference-memory
   #:validate-durable-memory-record
   #:durable-memory-record-to-plist
   #:plist-to-durable-memory-record
   #:test-durable-memory-constructors
   #:test-durable-memory-serialization
   ;; Policy registry (Phase 6 Task 2)
   #:*durable-memory-policies*
   #:get-durable-memory-config
   #:durable-memory-kind-policy
   ;; Scoring engine (Phase 6 Task 3)
   #:durable-memory-candidate
   #:make-durable-memory-candidate
   #:compute-durable-memory-importance-score
   #:compute-durable-memory-anti-score
   #:should-save-durable-memory-p
   #:detect-explicit-user-request-p
   ;; Storage backend (Phase 6 Task 4)
   #:*durable-memory-storage-root*
   #:durable-memory-kind-directory
   #:durable-memory-file-path
   #:durable-memory-metadata
   #:make-durable-memory-metadata
   #:render-durable-memory-header
   #:parse-durable-memory-header
   #:durable-memory-record-to-plist
   #:plist->durable-memory-record
   #:serialize-durable-memory-record
   #:deserialize-durable-memory-record
   #:split-header-and-records
   #:parse-durable-memory-records-section
   #:write-durable-memory-file
   ;; Phase 7: Embedding index
   #:*durable-memory-embedding-index*
   #:update-embedding-index
   #:get-embedding-from-index
   #:preload-all-embeddings
   #:retrieve-record-embedding
   #:generate-durable-memory-id
   #:load-durable-memories
   #:save-durable-memory-record
   #:update-durable-memory-record
   #:delete-durable-memory-record
   ;; Retrieval & summarization (Phase 6 Task 6)
   #:get-durable-memories-for-user
   #:get-durable-memories-for-project
   #:search-durable-memories
   #:rank-durable-memories
   #:summarize-durable-memories
   #:render-durable-memory-context
   ;; Ingestion pipeline (Phase 6 Task 5)
   #:extract-durable-memory-candidates
   #:ingest-durable-memory-from-session
   #:prune-durable-memories-if-needed
   #:extract-durable-memory
   ;; Backward compatibility (for consolidate.lisp)
   #:durable-memory-index-path
   #:durable-memory-note-path
   #:update-durable-memory-index
   ;; Utility (shared with search package)
   #:%string-empty-or-whitespace-p))

(defpackage #:claw-lisp.storage.durable-memory-search
  (:use #:cl)
  (:documentation "Durable memory search, query, and context injection.
   
   Core search: durable-memory-search.lisp
   Query helpers & runtime integration: durable-memory-query.lisp")
  (:import-from #:claw-lisp.storage.durable-memory
                #:durable-memory-record
                #:durable-memory-record-id
                #:durable-memory-record-kind
                #:durable-memory-record-subject-id
                #:durable-memory-record-title
                #:durable-memory-record-content
                #:durable-memory-record-importance-score
                #:durable-memory-record-tags
                #:durable-memory-record-superseded-by-id
                #:durable-memory-record-embedding
                #:durable-memory-record-embedding-model
                #:durable-memory-record-embedding-version
                #:load-durable-memories
                #:save-durable-memory-record
                #:%string-empty-or-whitespace-p)
  (:import-from #:claw-lisp.config
                #:*runtime-config*
                #:runtime-config
                #:current-dmq-config
                #:dmq-config-max-results
                #:dmq-config-min-relevance-score
                #:dmq-config-default-query-mode
                #:dmq-config-semantic-weight-by-kind
                #:dmq-config-max-injection-chars
                #:dmq-config-injection-enabled
                #:dmq-config-dedup-window-normal
                #:dmq-config-dedup-window-important
                #:dmq-config-importance-threshold
                #:dmq-config-evergreen-kinds
                #:dmq-config-embedding-failure-threshold
                #:dmq-config-embedding-cooldown-seconds)
  (:import-from #:claw-lisp.core.domain
                #:conversation
                #:conversation-messages
                #:make-message
                #:message-content
                #:message-role
                #:agent-turn-content
                #:agent-turn-metadata
                #:agent-turn-messages
                #:agent-turn-tool-results
                #:tool-result-content
                #:tool-result-tool-name
                #:session-memory-injection-log
                #:session-current-turn-id)
  (:export
   ;; Core search (Task 4)
   #:compute-cosine-similarity
   #:compute-cosine-similarity-arrays
   #:coerce-to-float-array
   #:semantic-search-durable-memory
   #:gather-candidate-embeddings
   ;; Search result struct
   #:search-result
   #:make-search-result
   #:search-result-record
   #:search-result-semantic-score
   #:search-result-importance-score
   #:search-result-final-score
   #:search-result-rank
   ;; Hybrid scoring
   #:compute-hybrid-score
   #:compute-record-importance-component
   #:resolve-hybrid-weight
   ;; Candidate gathering
   #:gather-candidate-embeddings
   #:embed-query-text
   ;; Configuration (Task 6)
   #:durable-memory-query-config
   #:make-durable-memory-query-config
   #:make-default-dmq-config
   #:current-dmq-config
   #:*dmq-active-config*
   ;; Query helpers (Task 6)
   #:query-durable-memory
   #:inject-durable-memory-context
   #:summarize-memory-results
   ;; Utilities (Task 6)
   #:embedding-available-p
   #:reset-embedding-circuit-breaker
   #:record-embedding-success
   #:record-embedding-failure
   #:filter-dedup-results
   ;; Circuit breaker state (Task 6)
   #:*dmq-embedding-failures*
   #:*dmq-circuit-open-until*
   ;; Memory injection (Task 6 Step 3)
   #:memory-injection-record
   #:make-memory-injection-record
   #:memory-injection-record-memory-id
   #:memory-injection-record-turn-id
   #:memory-injection-record-importance
   #:memory-injection-record-kind
   #:memory-injection-record-timestamp
   #:record-memory-injection
   #:extract-query-text
   #:insert-memory-context-message
   #:summarize-tool-results
   ;; Configuration parameters (Task 4)
   #:*semantic-search-default-hybrid-weight*
   #:*semantic-search-kind-hybrid-weights*
   #:*default-search-kinds*))

(defpackage #:claw-lisp.storage.consolidation-lock
  (:use #:cl)
  (:import-from #:claw-lisp.config
                #:runtime-config-memory-root)
  (:export
   #:consolidation-lock-path
   #:with-consolidation-lock))

(defpackage #:claw-lisp.core.claude-md
  (:use #:cl)
  (:export
   #:user-claude-md-path
   #:project-claude-md-paths
   #:read-claude-md-file
   #:load-claude-md-files))

(defpackage #:claw-lisp.core.system-prompt
  (:use #:cl)
  (:import-from #:claw-lisp.core.claude-md
                #:load-claude-md-files)
  (:import-from #:claw-lisp.core.protocols
                #:tool-description
                #:tool-input-schema)
  (:export
   #:build-system-prompt))

(defpackage #:claw-lisp.core.message-normalization
  (:use #:cl)
  (:import-from #:yason
                #:encode)
  (:import-from #:claw-lisp.core.domain
                #:make-message
                #:make-model-capabilities
                #:make-tool-result
                #:make-text-block
                #:make-thinking-block
                #:make-tool-use-block
                #:make-tool-result-block
                #:make-conversation
                #:message-content
                #:message-role
                #:message-metadata
                #:message-content-text
                #:model-capabilities-thinking-p
                #:thinking-block-p
                #:tool-use-block-p
                #:tool-use-block-id
                #:tool-result-call-id
                #:conversation-id
                #:conversation-messages
                #:conversation-tool-results
                #:conversation-metadata)
  (:export #:*validation-mode*
           #:*payload-capture-path*
           #:capture-payload
           #:normalize-messages-for-api
           #:normalize-conversation-for-anthropic
           #:validate-normalization-roundtrip))

(defpackage #:claw-lisp.core.microcompact
  (:use #:cl)
  (:import-from #:claw-lisp.config
                #:runtime-config-tool-result-aggregate-budget-bytes
                #:runtime-config-microcompact-keep-recent-tool-results)
  (:import-from #:claw-lisp.core.domain
                #:conversation-tool-results
                #:make-tool-result
                #:replace-tool-results
                #:tool-result-bytes
                #:tool-result-call-id
                #:tool-result-content
                #:tool-result-persisted-path
                #:tool-result-tool-name)
  (:export
   #:enforce-tool-result-aggregate-budget
   #:microcompact-conversation-tool-results))

(defpackage #:claw-lisp.core.model-registry
  (:use #:cl)
  (:import-from #:claw-lisp.core.domain
                #:make-model-capabilities
                #:model-capabilities-name
                #:model-capabilities-tools-p
                #:model-capabilities-streaming-p
                #:model-capabilities-thinking-p
                #:model-capabilities-adaptive-thinking-p
                #:model-capabilities-json-output-p
                #:model-capabilities-vision-p
                #:model-capabilities-prompt-caching-p
                #:model-capabilities-context-window
                #:model-capabilities-max-output-tokens)
  (:export
   #:model-registry
   #:make-model-registry
   #:register-model
   #:register-alias
   #:register-provider-default
   #:resolve-model
   #:model-supports-p
   #:model-context-window
   #:model-max-output-tokens
   #:model-translate-name
   #:register-known-models
   #:make-default-model-registry
   ;; Re-export from domain for convenience
   #:model-capabilities-context-window
   #:model-capabilities-max-output-tokens))

(defpackage #:claw-lisp.core.compact
  (:use #:cl)
  (:import-from #:claw-lisp.config
                #:runtime-config-post-compact-keep-recent-tool-results
                #:runtime-config-compaction-summary-token-budget)
  (:import-from #:claw-lisp.core.domain
                #:agent-session-conversation
                #:agent-session-id
                #:agent-session-model
                #:agent-session-provider
                #:agent-session-state
                #:compaction-result-preserved-messages
                #:compaction-result-source
                #:compaction-result-summary
                #:compaction-result-ir
                #:conversation-messages
                #:conversation-tool-results
                #:make-message
                #:make-compaction-result
                #:make-compaction-ir
                #:make-compaction-ir-provenance
                #:make-compaction-ir-section
                #:make-compaction-ir-item
                #:compaction-ir-id
                #:compaction-ir-source
                #:compaction-ir-session-id
                #:compaction-ir-predecessor-fingerprint
                #:compaction-ir-provenance
                #:compaction-ir-sections
                #:compaction-ir-token-budget
                #:compaction-ir-tokens-used
                #:compaction-ir-provenance-compaction-depth
                #:compaction-ir-section-kind
                #:compaction-ir-section-heading
                #:compaction-ir-section-items
                #:compaction-ir-section-tokens-estimated
                #:compaction-ir-section-trimmed-p
                #:compaction-ir-section-priority
                #:compaction-ir-item-type
                #:compaction-ir-item-text
                #:compaction-ir-item-role
                #:compaction-ir-item-tool-name
                #:compaction-ir-item-persisted-path
                #:compaction-ir-item-call-id
                #:compaction-ir-item-bytes
                #:compaction-ir-item-message-index
                #:message-content
                #:message-content-text
                #:message-role
                #:replace-tool-results
                #:tool-result-bytes
                #:tool-result-call-id
                #:tool-result-content
                #:tool-result-persisted-path
                #:tool-result-tool-name)
  (:import-from #:claw-lisp.storage.session-memory
                #:session-memory-path)
  (:export
   ;; Selective compaction (Phase 5 Task 6)
   #:strip-session-memory-metadata
   #:extract-session-memory-recent-activity-lines
   #:render-message-bullet-for-coverage
   #:find-covered-message-suffix-length
   #:build-selective-compaction-summary
   #:try-session-memory-compaction
   ;; Existing exports
   #:apply-compaction-result
   #:compact-session-locally
   ;; Phase 10: Structured Compaction IR
   #:render-compaction-ir-to-markdown
   #:compaction-ir-to-plist
   #:compaction-ir-provenance-to-plist
   #:compaction-ir-section-to-plist
   #:compaction-ir-fingerprint
   #:estimate-and-trim-ir-sections
   #:compaction-result-rendered-summary
   #:fallback-compaction-ir
   #:build-selective-compaction-ir
   #:session-compaction-depth
   #:session-last-compaction-fingerprint))

(defpackage #:claw-lisp.core.token-estimation
  (:use #:cl)
  (:import-from #:claw-lisp.core.domain
                #:message-content-text
                #:conversation-messages
                #:conversation-tool-results
                #:tool-result-content)
  (:export
   ;; Constants
   #:+chars-per-token+
   #:+message-overhead-tokens+
   #:+safety-margin+
   ;; Functions
   #:estimate-string-tokens
   #:estimate-message-tokens
   #:estimate-conversation-tokens
   #:estimate-system-prompt-tokens
   #:estimate-tool-definitions-tokens
   #:estimate-total-request-tokens))

(defpackage #:claw-lisp.core.context-monitor
  (:use #:cl)
  (:import-from #:claw-lisp.core.token-estimation
                #:estimate-total-request-tokens)
  (:import-from #:claw-lisp.core.model-registry
                #:resolve-model
                #:model-capabilities-context-window
                #:model-capabilities-max-output-tokens)
  (:import-from #:claw-lisp.config
                #:runtime-config-context-warning-threshold
                #:runtime-config-context-compact-suggested-threshold
                #:runtime-config-context-compact-required-threshold)
  (:export
   ;; Struct
   #:context-status
   #:make-context-status
   #:context-status-action
   #:context-status-threshold-name
   #:context-status-usage-ratio
   #:context-status-estimated-tokens
   #:context-status-context-limit
   #:context-status-headroom-tokens
   ;; Functions
   #:assess-context
   #:assess-context-with-limits
   #:format-context-warning
   #:idle-gap-microcompact-needed-p))

(defpackage #:claw-lisp.core.conditions
  (:use #:cl)
  (:export
   #:claw-error
   #:claw-error-message
   #:provider-error
   #:provider-error-provider
   #:provider-error-status
   #:provider-error-response-body
   #:rate-limit-error
   #:rate-limit-retry-after
   #:auth-error
   #:context-exceeded-error
   #:tool-error
   #:tool-error-tool-name
   #:permission-error
   #:permission-error-tool-name
   #:permission-error-path
   #:storage-error
   #:compaction-error
   #:orchestration-error
   #:orchestration-error-session-id
   #:child-cancelled-error
   #:child-cancelled-error-child-id
   #:child-cancelled-error-reason
   #:child-timeout-error
   #:child-timeout-error-child-id
   #:child-timeout-error-timeout-seconds
   #:child-supervisor-restart-limit-error
   #:child-supervisor-restart-limit-error-child-id
   #:child-supervisor-restart-limit-error-restart-count
   #:child-supervisor-restart-limit-error-max-restarts
   #:config-error
   #:config-error-path
   ;; Phase errors (coding CLI)
   #:invalid-phase-transition
   #:invalid-phase-transition-from
   #:invalid-phase-transition-to
   #:phase-violation-error
   #:phase-violation-tool
   #:phase-violation-current-phase
   #:phase-violation-valid-phases
   #:http-status->error-type))

(defpackage #:claw-lisp.core.phases
  (:use #:cl)
  (:export
   ;; Phase state accessors
   #:get-current-phase
   #:get-phase-history
   #:get-phase-counter
   #:get-phase-started-at
   #:get-turn-count
   #:get-last-verify-result
   #:get-last-turn-tool-count
   ;; Phase state mutators
   #:set-session-state-value
   #:increment-phase-counter
   #:increment-turn-count
   #:set-last-turn-tool-count
   #:set-last-verify-result
   ;; Phase transition
   #:valid-transition-p
   #:validate-transition
   #:transition-phase
   ;; Phase queries
   #:phase-duration-seconds
   #:phase-transition-count
   #:in-phase-p
   #:has-entered-phase-p
   ;; Initialization
   #:initialize-phase-state
   ;; Summary
   #:phase-summary))

(defpackage #:claw-lisp.core.agent-mailbox
  (:use #:cl)
  (:import-from #:claw-lisp.core.domain
                #:agent-envelope
                #:agent-mailbox-state
                #:agent-mailbox-state-mailbox-id
                #:agent-mailbox-state-buffer
                #:agent-mailbox-state-head-index
                #:agent-mailbox-state-tail-index
                #:agent-mailbox-state-count
                #:agent-mailbox-state-max-depth
                #:agent-mailbox-state-backpressure-mode
                #:agent-mailbox-state-closed-p
                #:agent-mailbox-state-dropped-count
                #:agent-mailbox-state-dead-letter-queue
                #:agent-mailbox-state-mutex
                #:agent-mailbox-state-waitqueue
                #:make-agent-mailbox-state)
  (:export
   #:make-agent-mailbox
   #:mailbox-send
   #:mailbox-receive
   #:mailbox-close
   #:mailbox-depth
   #:mailbox-dead-letters))

(defpackage #:claw-lisp.core.agent-supervisor
  (:use #:cl)
  (:import-from #:claw-lisp.core.domain
                #:agent-session
                #:agent-session-id
                #:agent-session-state
                #:agent-session-conversation
                #:conversation-messages
                #:conversation-tool-results
                #:agent-supervisor-state
                #:agent-supervisor-state-children
                #:agent-supervisor-state-mailbox
                #:agent-supervisor-state-parent-session-id
                #:agent-supervisor-state-mutex
                #:child-agent-spec
                #:child-agent-spec-child-id
                #:child-agent-spec-provider-name
                #:child-agent-spec-model
                #:child-agent-spec-initial-user-message
                #:child-agent-spec-timeout-seconds
                #:make-child-agent-spec
                #:child-agent-handle
                #:child-agent-handle-child-id
                #:child-agent-handle-thread
                #:child-agent-handle-session
                #:child-agent-handle-mailbox
                #:child-agent-handle-status
                #:child-agent-handle-last-error
                #:child-agent-handle-finished-universal-time
                #:child-agent-handle-restart-count
                #:child-agent-handle-start-order
                #:child-agent-handle-parent-id
                #:child-agent-handle-started-universal-time
                #:make-child-agent-handle
                #:child-progress-snapshot
                #:make-child-progress-snapshot
                #:make-agent-envelope
                #:make-agent-supervisor-state
                #:agent-supervisor-state-policy
                #:agent-supervisor-state-max-restarts
                #:agent-supervisor-state-restart-window-seconds
                #:agent-supervisor-state-restart-events
                #:agent-supervisor-state-supervisor-id)
  (:import-from #:claw-lisp.core.agent-mailbox
                #:make-agent-mailbox
                #:mailbox-send
                #:mailbox-receive
                #:mailbox-close
                #:mailbox-depth)
  (:import-from #:claw-lisp.core.conditions
                #:child-cancelled-error
                #:child-timeout-error
                #:child-supervisor-restart-limit-error)
  (:export
   #:ensure-agent-supervisor
   #:register-child-handle
   #:find-child-handle
   #:list-child-handles
   #:update-child-status
   #:spawn-child-agent
   #:send-agent-message
   #:receive-agent-message
   #:await-child-agent
   #:list-child-agents
   #:child-progress-snapshot
   #:list-child-progress-snapshots
   #:cancel-child-agent
   #:supervisor-handle-child-failure))

(defpackage #:claw-lisp.core.runtime
  (:use #:cl)
  (:import-from #:claw-lisp.config
                #:make-default-runtime-config)
  (:import-from #:claw-lisp.core.artifacts
                #:persist-artifact-to-cas
                #:resolve-artifact-from-cas
                #:resolve-tool-result-cas
                #:runtime-effective-cas-ref-root
                #:runtime-effective-cas-root
                #:store-tool-result-cas)
  (:import-from #:claw-lisp.core.domain
                #:agent-session
                #:agent-session-id
                #:append-message
                #:conversation
                #:conversation-messages
                #:conversation-tool-results
                #:record-tool-result
                #:make-agent-session
                #:make-conversation
                #:make-message
                #:make-tool-call
                #:make-tool-result-block
                #:tool-result->plist
                #:transport-response
                #:transport-response-assistant-text
                #:transport-response-ok-p
                #:transport-response-provider
                #:transport-response-status
                #:transport-response-error-message
                #:transport-response-tool-calls)
  (:import-from #:claw-lisp.core.model-registry
                #:model-registry
                #:make-default-model-registry
                #:resolve-model
                #:model-supports-p
                #:model-context-window
                #:model-max-output-tokens)
  (:import-from #:claw-lisp.core.system-prompt
                #:build-system-prompt)
  (:import-from #:claw-lisp.core.conditions
                #:context-exceeded-error)
  (:import-from #:claw-lisp.storage.transcripts
                #:append-transcript-event
                #:ensure-session-transcript
                #:transcript-path-for-session)
  (:import-from #:claw-lisp.storage.tool-results
                #:store-tool-result)
  (:import-from #:claw-lisp.storage.session-memory
                #:session-memory-path
                #:update-session-memory)
  (:import-from #:claw-lisp.storage.durable-memory
                #:durable-memory-note-path
                #:extract-durable-memory)
  (:import-from #:claw-lisp.core.microcompact
                #:enforce-tool-result-aggregate-budget
                #:microcompact-conversation-tool-results)
  (:import-from #:claw-lisp.core.compact
                #:apply-compaction-result
                #:compact-session-locally
                #:try-session-memory-compaction)
  (:import-from #:claw-lisp.core.context-monitor
                #:assess-context
                #:context-status-action
                #:context-status-usage-ratio
                #:format-context-warning
                #:idle-gap-microcompact-needed-p)
  (:import-from #:claw-lisp.core.conditions
                #:context-exceeded-error)
  (:import-from #:claw-lisp.core.protocols
                #:count-tokens
                #:execute-tool
                #:normalize-response
                #:normalize-tool-result
                #:provider
                #:provider-name
                #:send-turn
                #:stream-turn)
  (:export
   #:runtime
   #:runtime-settings
   #:runtime-provider-registry
   #:runtime-tool-registry
   #:runtime-model-registry
   #:runtime-project-root
   #:runtime-effective-project-root
   #:runtime-effective-cas-root
   #:runtime-effective-cas-ref-root
   #:runtime-tool-result-dedup-index
   #:make-runtime
   #:register-provider
   #:register-default-providers
   #:register-default-tools
   #:register-tool
   #:resolve-provider
   #:resolve-tool
   #:list-provider-names
   #:list-model-names
   #:list-tool-names
   #:check-runtime-dependencies
   #:check-provider-configuration
   #:select-session-model
   #:compaction-needed-p
   #:context-warning-p
   #:effective-context-window
   #:start-session
   #:resume-session
   #:apply-session-compaction
   #:compaction-circuit-open-p
   #:compact-session
   #:compact-session-with-session-memory
   #:durable-memory-path-for-session
   #:extract-session-durable-memory
   #:session-memory-path-for-session
   #:session-transcript-path
   #:session-transcript-existing-path
   #:transcript-existing-path-for-session-id
   #:execute-registered-tool
   #:execute-provider-turn-loop
   #:maybe-append-transcript-event
   #:maybe-append-cas-verify-failed-event
   #:session-context-status
   #:try-compact-session
   #:submit-user-message
   #:session-token-estimate
   ;; Phase 4 context management
   #:check-and-manage-context
   #:handle-context-exceeded
   #:compaction-circuit-open-p
   #:increment-compaction-failures
   #:reset-compaction-failures
   #:update-last-interaction-time
   #:idle-gap-seconds
   #:maybe-idle-gap-microcompact
   ;; Phase 5 session memory integration
   #:session-memory-path-for-session
   #:read-session-memory-text
   #:session-memory-update-needed-p
   #:perform-session-memory-update
   #:maybe-update-session-memory
   #:compact-session-with-session-memory
   #:compact-session
   ;; Phase 8 supervisor state helpers
   #:session-supervisor-state
   #:set-session-supervisor-state
   ;; Phase 8 spawn API wrappers
   #:spawn-child-agent
   #:send-agent-message
   #:receive-agent-message
   #:await-child-agent
   #:list-child-agents
   #:child-progress-snapshot
   #:list-child-progress-snapshots
   #:cancel-child-agent))

(defpackage #:claw-lisp.tools.echo
  (:use #:cl)
  (:import-from #:claw-lisp.core.domain
                #:make-tool-result)
  (:import-from #:claw-lisp.core.protocols
                #:execute-tool
                #:normalize-tool-result
                #:tool
                #:tool-input-schema
                #:validate-tool-input)
  (:export
   #:echo-tool
   #:make-echo-tool))

(defpackage #:claw-lisp.tools.file-read
  (:use #:cl)
  (:import-from #:claw-lisp.core.domain
                #:make-tool-result)
  (:import-from #:claw-lisp.core.protocols
                #:execute-tool
                #:normalize-tool-result
                #:tool
                #:tool-input-schema
                #:validate-tool-input)
  (:export
   #:file-read-tool
   #:make-file-read-tool))

(defpackage #:claw-lisp.tools.file-write
  (:use #:cl)
  (:import-from #:claw-lisp.core.domain
                #:make-tool-result)
  (:import-from #:claw-lisp.core.protocols
                #:execute-tool
                #:normalize-tool-result
                #:tool
                #:tool-input-schema
                #:validate-tool-input)
  (:export
   #:file-write-tool
   #:make-file-write-tool))

(defpackage #:claw-lisp.tools.file-replace
  (:use #:cl)
  (:import-from #:claw-lisp.core.domain
                #:make-tool-result)
  (:import-from #:claw-lisp.core.protocols
                #:execute-tool
                #:normalize-tool-result
                #:tool
                #:tool-input-schema
                #:validate-tool-input)
  (:export
   #:file-replace-tool
   #:make-file-replace-tool))

(defpackage #:claw-lisp.tools.glob
  (:use #:cl)
  (:import-from #:claw-lisp.core.domain
                #:make-tool-result)
  (:import-from #:claw-lisp.core.protocols
                #:execute-tool
                #:normalize-tool-result
                #:tool
                #:tool-input-schema
                #:validate-tool-input)
  (:export
   #:glob-tool
   #:make-glob-tool))

(defpackage #:claw-lisp.tools.grep
  (:use #:cl)
  (:import-from #:claw-lisp.core.domain
                #:make-tool-result)
  (:import-from #:claw-lisp.core.protocols
                #:execute-tool
                #:normalize-tool-result
                #:tool
                #:tool-input-schema
                #:validate-tool-input)
  (:export
   #:grep-tool
   #:make-grep-tool))

(defpackage #:claw-lisp.tools.shell-command
  (:use #:cl)
  (:import-from #:claw-lisp.core.domain
                #:make-tool-result)
  (:import-from #:claw-lisp.core.protocols
                #:execute-tool
                #:normalize-tool-result
                #:tool
                #:tool-input-schema
                #:validate-tool-input)
  (:export
   #:shell-command-tool
   #:make-shell-command-tool))

(defpackage #:claw-lisp.providers
  (:use #:cl)
  (:import-from #:claw-lisp.config
                #:runtime-config-embedding-enabled-p
                #:runtime-config-embedding-provider
                #:runtime-config-embedding-model
                #:runtime-config-embedding-max-batch-size
                #:runtime-config-embedding-timeout-seconds
                #:runtime-config-embedding-log-errors-p
                #:runtime-config-embedding-default-dimension)
  (:export
   ;; Error condition
   #:embedding-error
   #:embedding-error-provider
   #:embedding-error-model
   #:embedding-error-reason
   #:embedding-error-underlying-condition
   ;; Provider registry
   #:*embedding-providers*
   #:register-embedding-provider
   #:resolve-embedding-provider
   ;; Provider dispatch
   #:provider-compute-embeddings
   ;; Public API
   #:compute-embeddings
   #:compute-embeddings-batched
   ;; Utilities
   #:log-embedding-error
   #:embedding-provider-supported-models
   ;; Data structures
   #:embedding-result
   #:make-embedding-result
   #:embedding-result-text
   #:embedding-result-embedding
   #:embedding-result-model
   #:embedding-result-provider
   #:embedding-result-metadata))

(defpackage #:claw-lisp.providers.retry
  (:use #:cl)
  (:export
   #:call-with-retry
   #:exponential-delay
   #:retryable-status-p
   #:+default-max-retries+
   #:+default-base-delay-seconds+
   #:+default-max-delay-seconds+
   #:+retryable-status-codes+
   ;; Rate limit tracking
   #:rate-limit-state
   #:make-rate-limit-state
   #:rate-limit-state-remaining-requests
   #:rate-limit-state-reset-time
   #:rate-limit-state-limit
   #:rate-limit-state-retry-after
   #:update-rate-limit-state
   #:rate-limit-exhausted-p
   #:rate-limit-approaching-p
   #:time-until-reset
   #:should-pause-for-rate-limit))

(defpackage #:claw-lisp.providers.auth
  (:use #:cl)
  (:import-from #:claw-lisp.config
                #:config-credentials
                #:provider-credentials
                #:provider-credentials-api-key
                #:provider-credentials-base-url
                #:bedrock-credentials
                #:bedrock-credentials-access-key
                #:bedrock-credentials-secret-key)
  (:export
   #:credentials-configured-p
   #:get-missing-config))

(defpackage #:claw-lisp.providers.sse-parser
  (:use #:cl)
  (:export
   #:read-sse-event))

(defpackage #:claw-lisp.providers.http-utils
  (:use #:cl)
  (:import-from #:claw-lisp.core.domain
                #:conversation-messages
                #:message-content
                #:message-role
                #:message-content-text
                #:text-block
                #:text-block-p
                #:text-block-text
                #:tool-use-block
                #:tool-use-block-id
                #:tool-use-block-name
                #:tool-use-block-input
                #:tool-use-block-p
                #:tool-result-block
                #:tool-result-block-tool-use-id
                #:tool-result-block-content
                #:tool-result-block-is-error
                #:thinking-block
                #:thinking-block-thinking
                #:thinking-block-signature
                #:content-block-p)
  (:import-from #:claw-lisp.providers.http-json
                #:value->json-safe
                #:plist-to-json-object)
  (:export
   #:*http-debug-p*
   #:command-available-p
   #:conversation->anthropic-json
   #:conversation->chat-json
   #:message->anthropic-block
   #:extract-anthropic-response-text
   #:extract-anthropic-tool-calls
   #:extract-openrouter-response-text
   #:extract-openrouter-tool-calls
   #:http-post-result-success-p
   #:json-decode
   #:json-encode
   #:json-encode-string
   #:post-json
   #:post-json-with-headers
   #:dexador-error-response
   #:dexador-response-status
   #:dexador-response-body
   #:dexador-response-headers
   #:value->json-safe
   #:plist-to-json-object))

(defpackage #:claw-lisp.providers.stream-accumulator
  (:use #:cl)
  (:import-from #:claw-lisp.core.domain
                #:stream-accumulator
                #:stream-accumulator-message-id
                #:stream-accumulator-model
                #:stream-accumulator-text
                #:stream-accumulator-tool-use-blocks
                #:stream-accumulator-current-tool-use
                #:stream-accumulator-stop-reason
                #:stream-accumulator-stop-sequence
                #:stream-accumulator-usage
                #:stream-accumulator-done
                #:make-stream-accumulator
                #:make-transport-response)
  (:import-from #:claw-lisp.providers.http-utils
                #:json-decode)
  (:export
   #:process-stream-event
   #:accumulator->transport-response
   #:make-stream-accumulator))

(defpackage #:claw-lisp.providers.bedrock
  (:use #:cl)
  (:import-from #:claw-lisp.core.domain
                #:make-transport-response
                #:conversation-messages
                #:message-role
                #:message-content
                #:message-content-text)
  (:import-from #:claw-lisp.core.protocols
                #:provider
                #:send-turn
                #:stream-turn
                #:normalize-response
                #:count-tokens)
  (:import-from #:claw-lisp.providers.http-utils
                #:json-encode-string
                #:json-decode)
  (:export
   #:bedrock-provider
   #:provider-model-id
   #:provider-region
   #:make-bedrock-provider))

(defpackage #:claw-lisp.providers.rate-limit
  (:use #:cl)
  (:export
   #:rate-limit-state
   #:rate-limit-state-remaining
   #:rate-limit-state-limit
   #:rate-limit-state-reset-time
   #:rate-limit-state-retry-after
   #:rate-limit-state-last-updated
   #:rate-limit-state-provider
   #:make-rate-limit-state
   #:update-rate-limit-state
   #:rate-limit-exhausted-p
   #:rate-limit-warning-p
   #:seconds-until-reset
   #:check-rate-limit
   #:clear-retry-after
   #:rate-limit-summary
   #:*warning-threshold-fraction+
   #:*warning-threshold-absolute+
   #:*max-sensible-retry-after+
   ;; Internal functions for testing
   #:parse-header-integer
   #:header-value
   #:parse-reset-header))

(defpackage #:claw-lisp.providers.anthropic
  (:use #:cl)
  (:import-from #:claw-lisp.config
                #:runtime-config
                #:config-credentials
                #:provider-credentials
                #:provider-credentials-api-key
                #:provider-credentials-base-url
                #:anthropic-credentials
                #:anthropic-credentials-api-version)
  (:import-from #:claw-lisp.core.protocols
                #:count-tokens
                #:normalize-response
                #:provider
                #:provider-model-registry
                #:send-turn
                #:stream-turn)
  (:import-from #:claw-lisp.providers.auth
                #:credentials-configured-p)
  (:import-from #:claw-lisp.providers.http-utils
                #:conversation->anthropic-json
                #:extract-anthropic-response-text
                #:extract-anthropic-tool-calls
                #:http-post-result-success-p
                #:post-json
                #:post-json-with-headers
                #:json-encode-string
                #:dexador-response-status
                #:dexador-response-headers
                #:dexador-response-body)
  (:import-from #:claw-lisp.providers.sse-parser
                #:read-sse-event)
  (:import-from #:claw-lisp.providers.stream-accumulator
                #:process-stream-event
                #:accumulator->transport-response
                #:make-stream-accumulator)
  (:export
   #:anthropic-provider
   #:anthropic-provider-credentials
   #:make-anthropic-provider))

(defpackage #:claw-lisp.providers.openrouter
  (:use #:cl)
  (:import-from #:claw-lisp.config
                #:runtime-config
                #:config-credentials
                #:provider-credentials
                #:provider-credentials-api-key
                #:provider-credentials-base-url
                #:openrouter-credentials)
  (:import-from #:claw-lisp.core.protocols
                #:count-tokens
                #:normalize-response
                #:provider
                #:send-turn
                #:stream-turn)
  (:import-from #:claw-lisp.providers.auth
                #:credentials-configured-p)
  (:import-from #:claw-lisp.providers.http-utils
                #:conversation->chat-json
                #:extract-openrouter-response-text
                #:extract-openrouter-tool-calls
                #:http-post-result-success-p
                #:post-json)
  (:export
   #:openrouter-provider
   #:openrouter-provider-credentials
   #:make-openrouter-provider))

(defpackage #:claw-lisp.providers.mock
  (:use #:cl)
  (:import-from #:claw-lisp.core.protocols
                #:count-tokens
                #:normalize-response
                #:provider
                #:send-turn
                #:stream-turn)
  (:export
   #:mock-provider
   #:make-mock-provider))

(defpackage #:claw-lisp.storage.durable-memory-embeddings
  (:use #:cl)
  (:import-from #:claw-lisp.config
                #:*runtime-config*
                #:runtime-config-embedding-enabled-p
                #:runtime-config-embedding-provider
                #:runtime-config-embedding-model
                #:runtime-config-embedding-max-batch-size
                #:runtime-config-embedding-timeout-seconds
                #:runtime-config-embedding-log-errors-p
                #:runtime-config-embedding-default-dimension
                #:runtime-config-memory-root
                #:runtime-config-provider-credentials
                #:current-embedding-provider
                #:current-embedding-model
                #:current-embedding-max-batch-size
                #:current-embedding-enabled-p
                #:config-credentials)
  (:import-from #:claw-lisp.providers.http-json
                #:value->json-safe
                #:plist-to-json-object)
  (:import-from #:claw-lisp.storage.durable-memory
                #:durable-memory-record
                #:durable-memory-record-id
                #:durable-memory-record-kind
                #:durable-memory-record-content
                #:durable-memory-record-title
                #:durable-memory-record-embedding
                #:durable-memory-record-embedding-model
                #:durable-memory-record-embedding-version
                #:durable-memory-record-tags
                #:load-durable-memories
                #:save-durable-memory-record
                #:*durable-memory-embedding-index*
                #:update-embedding-index
                #:get-embedding-from-index
                #:preload-all-embeddings
                #:retrieve-record-embedding
                #:ingest-durable-memory-from-session
                #:durable-memory-kind-directory)
  (:export
   ;; Configuration
   #:*durable-memory-embedding-kinds*
   #:*durable-memory-embedding-refresh-enabled-p*
   #:*durable-memory-embedding-refresh-batch-size*
   #:*durable-memory-embedding-pending-mark*
   #:*durable-memory-embedding-max-text-length*
   ;; Single record embedding
   #:generate-embedding-for-record
   ;; Batch embedding
   #:generate-embeddings-for-records
   ;; Background refresh
   #:find-records-missing-embeddings
   #:refresh-missing-embeddings
    ;; Integration
    #:ingest-durable-memory-with-embedding
    ;; Subject ID discovery (used by search)
    #:discover-subject-ids))

(defpackage #:claw-lisp.tests
  (:use #:cl)
  (:import-from #:claw-lisp.core.runtime
                #:compact-session-with-session-memory
                #:execute-registered-tool
                #:extract-session-durable-memory
                #:make-runtime
                #:list-tool-names
                #:register-default-providers
                #:register-tool
                #:runtime-tool-registry
                #:session-transcript-path
                #:start-session
                #:submit-user-message
                #:check-and-manage-context
                #:handle-context-exceeded
                #:compaction-circuit-open-p
                #:increment-compaction-failures
                #:reset-compaction-failures)
  (:import-from #:claw-lisp.core.message-normalization
                #:normalize-messages-for-api
                #:validate-normalization-roundtrip
                #:capture-payload
                #:*validation-mode*
                #:*payload-capture-path*)
  (:import-from #:claw-lisp.providers.http-utils
                #:json-decode
                #:json-encode-string
                #:extract-anthropic-response-text
                #:extract-anthropic-tool-calls
                #:extract-openrouter-response-text
                #:extract-openrouter-tool-calls
                #:conversation->anthropic-json
                #:conversation->chat-json)
  (:import-from #:claw-lisp.core.domain
                #:make-conversation
                #:make-message
                #:append-message
                #:conversation-tool-results
                #:tool-result-content
                #:tool-result-tool-name
                #:message-content-text
                #:text-block
                #:text-block-text
                #:text-block-p
                #:make-text-block
                #:tool-use-block
                #:tool-use-block-id
                #:tool-use-block-name
                #:tool-use-block-p
                #:make-tool-use-block
                #:message-content)
  (:import-from #:claw-lisp.storage.transcripts
                #:transcript-path-for-session)
  (:import-from #:claw-lisp.storage.tool-results
                #:delete-session-tool-results
                #:read-persisted-tool-result)
  (:import-from #:claw-lisp.storage.session-memory
                #:render-session-memory
                #:session-memory-path)
  (:import-from #:claw-lisp.storage.durable-memory
                #:durable-memory-index-path
                #:durable-memory-note-path)
  (:import-from #:claw-lisp.core.compact
                #:try-session-memory-compaction)
  (:import-from #:claw-lisp.tools.echo
                #:make-echo-tool)
  (:import-from #:claw-lisp.tools.file-read
                #:make-file-read-tool)
  (:import-from #:claw-lisp.tools.file-write
                #:make-file-write-tool)
  (:import-from #:claw-lisp.tools.file-replace
                #:make-file-replace-tool)
  (:import-from #:claw-lisp.tools.glob
                #:make-glob-tool)
  (:import-from #:claw-lisp.tools.grep
                #:make-grep-tool)
  (:import-from #:claw-lisp.tools.shell-command
                #:make-shell-command-tool)
  (:export #:run-tests
           #:run-rate-limit-tests
           #:run-token-estimation-tests
           #:run-context-monitor-tests
           #:run-runtime-context-tests
           #:run-phase4-integration-tests
           #:run-session-memory-tests
           #:run-durable-memory-scoring-tests
           #:run-durable-memory-tests
           #:run-durable-memory-e2e-test
           #:run-durable-memory-query-tests
           #:run-durable-memory-runtime-integration-tests
           ;; Phase 7 E2E fixtures
           #:run-phase7-smoke-test
           #:run-phase7-e2e-tests
           #:run-phase7-conversation-tests
           #:run-phase7-circuit-breaker-tests
           #:run-phase7-consolidation-tests
           #:run-phase7-multi-session-tests
           #:run-phase7-performance-tests
           #:run-cas-tests
           #:run-cas-integrity-tests))

(defpackage #:claw-lisp.cli
  (:use #:cl)
  (:import-from #:claw-lisp.config
                #:runtime-config-data-root
                #:runtime-config-default-provider
                #:runtime-config-default-model
                #:runtime-config-tool-preview-bytes
                #:runtime-config-shell-command-enabled-p
                #:make-default-runtime-config)
  (:import-from #:claw-lisp.core.domain
                #:agent-session-conversation
                #:agent-session-id
                #:agent-session-model
                #:agent-session-provider
                #:conversation-messages
                #:conversation-tool-results
                #:make-message
                #:message-content
                #:message-role
                #:transport-response-assistant-text
                #:transport-response-tool-calls
                #:make-conversation)
  (:import-from #:claw-lisp.core.runtime
                #:make-runtime
                #:runtime-settings
                #:register-default-providers
                #:register-tool
                #:resolve-provider
                #:list-tool-names
                #:list-provider-names
                #:start-session
                #:execute-provider-turn-loop
                #:session-transcript-path
                #:session-memory-path-for-session
                #:maybe-append-transcript-event)
  (:import-from #:claw-lisp.core.artifacts
                #:runtime-effective-cas-root
                #:runtime-effective-cas-ref-root)
  (:import-from #:claw-lisp.storage.cas
                #:cas-exists-p
                #:cas-get
                #:cas-object-path
                #:valid-versioned-hash-p)
  (:import-from #:claw-lisp.storage.cas-ref
                #:read-cas-ref
                #:resolve-cas-ref)
  (:import-from #:claw-lisp.cas.manifest
                #:load-manifest
                #:manifest-entries
                #:manifest-metadata
                #:manifest-root-digest
                #:manifest-signature
                #:verify-manifest-integrity)
  (:import-from #:claw-lisp.tools.echo
                #:make-echo-tool)
  (:import-from #:claw-lisp.tools.file-read
                #:make-file-read-tool)
  (:import-from #:claw-lisp.tools.file-write
                #:make-file-write-tool)
  (:import-from #:claw-lisp.tools.file-replace
                #:make-file-replace-tool)
  (:import-from #:claw-lisp.tools.glob
                #:make-glob-tool)
  (:import-from #:claw-lisp.tools.grep
                #:make-grep-tool)
  (:import-from #:claw-lisp.tools.shell-command
                #:make-shell-command-tool)
  (:export
   #:run-cli
   #:run-repl-loop
   #:make-cli-runtime
   #:print-welcome
   #:print-help
   #:main
   #:main-entry-point))

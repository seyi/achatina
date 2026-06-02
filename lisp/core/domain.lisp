(in-package #:claw-lisp.core.domain)

(defstruct (model-capabilities
            (:constructor make-model-capabilities
                (&key
                 (name "")
                 (provider :unknown)
                 (context-window 200000)
                 (max-output-tokens 8192)
                 (default-output-tokens 8192)
                 (tools-p t)
                 (streaming-p t)
                 (thinking-p nil)
                 (adaptive-thinking-p nil)
                 (json-output-p nil)
                 (vision-p nil)
                 (prompt-caching-p nil)
                 (input-price-per-mtok 0.0)
                 (output-price-per-mtok 0.0)
                 (cache-read-price-per-mtok 0.0)
                 (cache-write-price-per-mtok 0.0))))
  (name "" :type string)
  (provider :unknown :type keyword)
  (context-window 200000 :type integer)
  (max-output-tokens 8192 :type integer)
  (default-output-tokens 8192 :type integer)
  (tools-p t :type boolean)
  (streaming-p t :type boolean)
  (thinking-p nil :type boolean)
  (adaptive-thinking-p nil :type boolean)
  (json-output-p nil :type boolean)
  (vision-p nil :type boolean)
  (prompt-caching-p nil :type boolean)
  (input-price-per-mtok 0.0 :type float)
  (output-price-per-mtok 0.0 :type float)
  (cache-read-price-per-mtok 0.0 :type float)
  (cache-write-price-per-mtok 0.0 :type float))

(defstruct (transport-response
            (:constructor make-transport-response
                (&key
                 (ok-p nil)
                 (status 0)
                 (assistant-text "")
                 (raw-response "")
                 (error-message nil)
                 (provider "")
                 (metadata nil)
                 (tool-calls nil))))
  (ok-p nil :type boolean)
  (status 0 :type integer)
  (assistant-text "" :type string)
  (raw-response "" :type string)
  error-message
  (provider "" :type string)
  metadata
  (tool-calls nil :type list))

;; --- Content Block Model ---
;;
;; The Anthropic API returns messages with content as an array of blocks:
;;   {"role": "assistant", "content": [
;;     {"type": "text", "text": "I'll read that file."},
;;     {"type": "tool_use", "id": "toolu_01A", "name": "Read", "input": {...}}
;;   ]}
;;
;; The internal model supports both a simple string (for user messages and
;; legacy compatibility) and a list of content blocks (for assistant messages
;; with mixed text/tool-use content).

(defstruct content-block)

(defstruct (text-block (:include content-block))
  "Plain text content within an assistant message."
  (text "" :type string))

(defstruct (tool-use-block (:include content-block))
  "A tool-use request from the model.
   ID is the API-assigned identifier (e.g., \"toolu_01A...\").
   NAME is the tool name the model requested.
   INPUT is a plist of parameters the model provided."
  (id "" :type string)
  (name "" :type string)
  input)

(defstruct (tool-result-block (:include content-block))
  "A tool result to be sent back to the API.
   TOOL-USE-ID references the tool_use_block this result corresponds to.
   CONTENT is the result text.
   IS-ERROR indicates the tool execution failed."
  (tool-use-id "" :type string)
  (content "" :type string)
  (is-error nil :type boolean))

(defstruct (thinking-block (:include content-block))
  "Extended thinking content from Anthropic models.
   THINKING is the reasoning text.
   SIGNATURE is the optional cryptographic signature."
  (thinking "" :type string)
  (signature "" :type string))

;; --- Message ---

(defstruct (message
            (:constructor %make-message
                (&key (role :user) content (metadata nil))))
  (role :user :type keyword)
  ;; CONTENT may be either a string (user messages, legacy) or a list of
  ;; content-block structs (assistant messages with tool-use, system prompts).
  content
  metadata)

(defun make-message (&key (role :user) content (metadata nil))
  "Create a message. CONTENT can be a string or a list of content-blocks."
  (%make-message :role role :content content :metadata metadata))

(defun message-content-text (message)
  "Return the full text content of MESSAGE as a string.
   For string content, returns it directly.
   For content-block lists, concatenates text-block and thinking-block text."
  (let ((content (message-content message)))
    (if (stringp content)
        content
        (with-output-to-string (out)
          (dolist (block content)
            (typecase block
              (text-block (write-string (text-block-text block) out))
              (thinking-block (write-string (thinking-block-thinking block) out))))))))

(defun message-tool-use-blocks (message)
  "Return the list of tool-use-blocks in MESSAGE, or NIL."
  (let ((content (message-content message)))
    (if (listp content)
        (remove-if-not #'tool-use-block-p content)
        nil)))

(defstruct (tool-call
            (:constructor make-tool-call
                (&key id name input (metadata nil))))
  (id "" :type string)
  (name "" :type string)
  input
  metadata)

(defstruct (artifact
            (:constructor make-artifact
                (&key id kind cas-hash cas-type cas-ref-name metadata)))
  "Lightweight handle for a CAS-backed artifact."
  id
  (kind :unknown :type keyword)
  cas-hash
  (cas-type :sexp :type keyword)
  cas-ref-name
  metadata)

(defun tool-use-block->tool-call (block)
  "Convert a tool-use-block to a tool-call struct."
  (make-tool-call
   :id (tool-use-block-id block)
   :name (tool-use-block-name block)
   :input (tool-use-block-input block)))

(defstruct (tool-result
            (:constructor make-tool-result
                (&key
                 call-id
                 tool-name
                 content
                 (persisted-path nil)
                 (truncated-p nil)
                 (bytes 0)
                 (cas-hash nil)
                 (cas-type :markdown)
                 (cas-ref-name nil)
                 (artifact nil))))
  (call-id "" :type string)
  (tool-name "" :type string)
  (content "" :type string)
  persisted-path
  (truncated-p nil :type boolean)
  (bytes 0 :type integer)
  cas-hash
  (cas-type :markdown :type keyword)
  cas-ref-name
  artifact)

(defun %copy-tool-result-with (result &key
                                 (call-id :unspecified)
                                 (tool-name :unspecified)
                                 (content :unspecified)
                                 (persisted-path :unspecified)
                                 (truncated-p :unspecified)
                                 (bytes :unspecified)
                                 (cas-hash :unspecified)
                                 (cas-type :unspecified)
                                 (cas-ref-name :unspecified)
                                 (artifact :unspecified))
  "Clone RESULT while allowing selected slots to be overridden."
  (make-tool-result
   :call-id (if (eq call-id :unspecified)
                (tool-result-call-id result)
                call-id)
   :tool-name (if (eq tool-name :unspecified)
                  (tool-result-tool-name result)
                  tool-name)
   :content (if (eq content :unspecified)
                (tool-result-content result)
                content)
   :persisted-path (if (eq persisted-path :unspecified)
                       (tool-result-persisted-path result)
                       persisted-path)
   :truncated-p (if (eq truncated-p :unspecified)
                    (tool-result-truncated-p result)
                    truncated-p)
   :bytes (if (eq bytes :unspecified)
              (tool-result-bytes result)
              bytes)
   :cas-hash (if (eq cas-hash :unspecified)
                 (tool-result-cas-hash result)
                 cas-hash)
   :cas-type (if (eq cas-type :unspecified)
                 (tool-result-cas-type result)
                 cas-type)
   :cas-ref-name (if (eq cas-ref-name :unspecified)
                     (tool-result-cas-ref-name result)
                     cas-ref-name)
   :artifact (if (eq artifact :unspecified)
                 (tool-result-artifact result)
                 artifact)))

(defstruct (stream-accumulator
            (:constructor make-stream-accumulator
                (&key
                 (message-id "")
                 (model "")
                 (text "")
                 (tool-use-blocks nil)
                 (current-tool-use nil)
                 (stop-reason nil)
                 (stop-sequence nil)
                 (usage nil)
                 (done nil)
                 (on-event nil))))
  (message-id "" :type string)
  (model "" :type string)
  (text "" :type string)
  ;; Tool-use blocks being accumulated. Each is a plist:
  ;; (:id "" :name "" :input-json "" :input nil)
  (tool-use-blocks nil :type list)
  ;; Current tool-use block being built (or nil)
  (current-tool-use nil)
  (stop-reason nil)
  (stop-sequence nil)
  (usage nil)
  ;; Whether the stream has ended
  (done nil :type boolean)
  ;; Optional callback for UI updates: (funcall on-event event-type data)
  (on-event nil :type (or null function)))

(defstruct (compaction-result
            (:constructor make-compaction-result
                (&key
                 (source :session-memory)
                 (summary "")
                 (ir nil)
                 (preserved-messages nil))))
  (source :session-memory :type keyword)
  (summary "" :type string)
  ir
  (preserved-messages nil :type list))

;;; ============================================================
;;; Structured Compaction IR (Phase 10)
;;; ============================================================

(defstruct (compaction-ir
            (:constructor make-compaction-ir
                (&key
                 (id "")
                 (source :fallback)
                 (created-universal-time (get-universal-time))
                 (session-id "")
                 (predecessor-fingerprint nil)
                 (provenance nil)
                 (sections nil)
                 (token-budget nil)
                 (tokens-used 0))))
  (id "" :type string)
  (source :fallback :type keyword)
  (created-universal-time 0 :type integer)
  (session-id "" :type string)
  predecessor-fingerprint
  provenance
  (sections nil :type list)
  token-budget
  (tokens-used 0 :type integer))

(defstruct (compaction-ir-provenance
            (:constructor make-compaction-ir-provenance
                (&key
                 (session-memory-used-p nil)
                 (uncovered-messages-count 0)
                 (summarized-messages-count 0)
                 (preserved-tail-count 0)
                 (total-messages-before 0)
                 (tool-results-summarized-count 0)
                 (compaction-depth 0))))
  (session-memory-used-p nil :type boolean)
  (uncovered-messages-count 0 :type integer)
  (summarized-messages-count 0 :type integer)
  (preserved-tail-count 0 :type integer)
  (total-messages-before 0 :type integer)
  (tool-results-summarized-count 0 :type integer)
  (compaction-depth 0 :type integer))

(defstruct (compaction-ir-section
            (:constructor make-compaction-ir-section
                (&key
                 (kind :text)
                 (heading "")
                 (items nil)
                 (tokens-estimated 0)
                 (trimmed-p nil)
                 (priority :normal))))
  (kind :text :type keyword)
  (heading "" :type string)
  (items nil :type list)
  (tokens-estimated 0 :type integer)
  (trimmed-p nil :type boolean)
  (priority :normal :type keyword))

(defstruct (compaction-ir-item
            (:constructor make-compaction-ir-item
                (&key
                 (type :bullet)
                 (text "")
                 (role nil)
                 (tool-name nil)
                 (persisted-path nil)
                 (call-id nil)
                 (bytes 0)
                 (message-index nil))))
  (type :bullet :type keyword)
  (text "" :type string)
  role
  tool-name
  persisted-path
  call-id
  (bytes 0 :type integer)
  message-index)

(defun tool-result->plist (result)
  "Render RESULT into a plist suitable for transcript metadata."
  (append
   (list :call_id (tool-result-call-id result)
         :tool_name (tool-result-tool-name result)
         :content (tool-result-content result)
         :persisted_path (tool-result-persisted-path result)
         :truncated_p (tool-result-truncated-p result)
         :bytes (tool-result-bytes result))
   (when (tool-result-cas-hash result)
     (list :cas_hash (tool-result-cas-hash result)
           :cas_type (tool-result-cas-type result)
           :cas_ref_name (tool-result-cas-ref-name result)))))

(defstruct (conversation
            (:constructor make-conversation
                (&key id (messages nil) (tool-results nil) (metadata nil))))
  (id "default" :type string)
  (messages nil :type list)
  (tool-results nil :type list)
  metadata)

(defun append-message (conversation message)
  "Append MESSAGE to CONVERSATION and return the conversation."
  (setf (conversation-messages conversation)
        (append (conversation-messages conversation) (list message)))
  conversation)

(defun record-tool-result (conversation result)
  "Append RESULT to CONVERSATION and return the conversation."
  (setf (conversation-tool-results conversation)
        (append (conversation-tool-results conversation) (list result)))
  conversation)

(defun replace-tool-results (conversation results)
  "Replace CONVERSATION tool results with RESULTS and return the conversation."
  (setf (conversation-tool-results conversation) results)
  conversation)

(defstruct (agent-turn
            (:constructor make-agent-turn
                (&key (content "") (metadata nil) (messages nil) (tool-results nil))))
  (content "" :type string)
  metadata
  (messages nil :type list)
  (tool-results nil :type list))

(defstruct (agent-session
            (:constructor make-agent-session
                (&key id provider model conversation (state nil))))
  (id "session-0" :type string)
  provider
  (model "" :type string)
  conversation
  ;; STATE is a plist that may include phase-tracking keys:
  ;;   :current-phase       - keyword: :inspect | :edit | :verify | :complete
  ;;   :phase-history       - list of (:phase <phase> :timestamp <ts> :trigger <reason>)
  ;;   :phase-counters      - plist: (:inspect 3 :edit 1 :verify 0 :complete 0)
  ;;   :phase-started-at    - universal time when current phase started
  ;;   :last-verify-result  - boolean: T if last verify step passed
  ;;   :last-turn-tool-count - integer: number of tools called in last turn
  ;;   :turn-count          - integer: total turns in session
  state)

;;; ============================================================
;;; Multi-Agent Runtime Domain Model (Phase 8)
;;; ============================================================

(defstruct (agent-envelope
            (:constructor make-agent-envelope
                (&key
                 (id "")
                 (from-agent-id "")
                 (to-agent-id "")
                 (type :task-request)
                 (payload nil)
                 (correlation-id nil)
                 (reply-to-id nil)
                 (created-universal-time (get-universal-time))
                 (deadline-universal-time nil)
                 (attempt 0))))
  (id "" :type string)
  (from-agent-id "" :type string)
  (to-agent-id "" :type string)
  (type :task-request :type keyword)
  payload
  correlation-id
  reply-to-id
  (created-universal-time 0 :type integer)
  deadline-universal-time
  (attempt 0 :type integer))

(defstruct (agent-mailbox-state
            (:constructor make-agent-mailbox-state
                (&key
                 (mailbox-id "")
                 (owner-agent-id "")
                 (buffer #())
                 (head-index 0)
                 (tail-index 0)
                 (count 0)
                 (max-depth 0)
                 (backpressure-mode :block)
                 (closed-p nil)
                 (dropped-count 0)
                 (dead-letter-queue nil)
                 (mutex nil)
                 (waitqueue nil))))
  (mailbox-id "" :type string)
  (owner-agent-id "" :type string)
  (buffer #() :type vector)
  (head-index 0 :type integer)
  (tail-index 0 :type integer)
  (count 0 :type integer)
  (max-depth 0 :type integer)
  (backpressure-mode :block :type keyword)
  (closed-p nil :type boolean)
  (dropped-count 0 :type integer)
  (dead-letter-queue nil :type list)
  mutex
  waitqueue)

(defstruct (child-agent-spec
            (:constructor make-child-agent-spec
                (&key
                 (child-id "")
                 (provider-name nil)
                 (model nil)
                 (initial-user-message "")
                 (timeout-seconds nil)
                 (supervisor-policy :one-for-one)
                 (metadata nil))))
  (child-id "" :type string)
  provider-name
  model
  (initial-user-message "" :type string)
  timeout-seconds
  (supervisor-policy :one-for-one :type keyword)
  metadata)

(defstruct (child-agent-handle
            (:constructor make-child-agent-handle
                (&key
                 (child-id "")
                 (parent-id "")
                 (session nil)
                 (thread nil)
                 (mailbox nil)
                 (status :starting)
                 (started-universal-time (get-universal-time))
                 (finished-universal-time nil)
                 (last-error nil)
                 (restart-count 0)
                 (start-order 0))))
  (child-id "" :type string)
  (parent-id "" :type string)
  session
  thread
  mailbox
  (status :starting :type keyword)
  (started-universal-time 0 :type integer)
  finished-universal-time
  last-error
  (restart-count 0 :type integer)
  (start-order 0 :type integer))

(defstruct (agent-supervisor-state
            (:constructor make-agent-supervisor-state
                (&key
                 (supervisor-id "")
                 (parent-session-id "")
                 (policy :one-for-one)
                 (children (make-hash-table :test 'equal))
                 (mailbox nil)
                 (max-restarts 3)
                 (restart-window-seconds 60)
                 (restart-events nil)
                 (mutex nil))))
  (supervisor-id "" :type string)
  (parent-session-id "" :type string)
  (policy :one-for-one :type keyword)
  children
  mailbox
  (max-restarts 3 :type integer)
  (restart-window-seconds 60 :type integer)
  (restart-events nil :type list)
  mutex)

(defstruct (child-progress-snapshot
            (:constructor make-child-progress-snapshot
                (&key
                 (child-id "")
                 (status :starting)
                 (summary-text "")
                 (last-updated-universal-time (get-universal-time))
                 (tool-calls-count 0)
                 (messages-count 0))))
  (child-id "" :type string)
  (status :starting :type keyword)
  (summary-text "" :type string)
  (last-updated-universal-time 0 :type integer)
  (tool-calls-count 0 :type integer)
  (messages-count 0 :type integer))

;;; ============================================================
;;; Session Extensions for Memory Injection (Phase 7 Task 6)
;;; ============================================================

(defun session-memory-injection-log (session)
  "Get the memory injection log for SESSION.
   Returns a list of memory-injection-record structs.
   Initializes to NIL if not yet set."
  (or (getf (agent-session-state session) :memory-injection-log)
      '()))

(defun (setf session-memory-injection-log) (value session)
  "Set the memory injection log for SESSION."
  (setf (getf (agent-session-state session) :memory-injection-log) value))

(defun session-current-turn-id (session)
  "Get the current turn ID for SESSION."
  (or (getf (agent-session-state session) :current-turn-id)
      0))

(defun (setf session-current-turn-id) (value session)
  "Set the current turn ID for SESSION."
  (setf (getf (agent-session-state session) :current-turn-id) value))

(defun insert-message-before-user-turn (turn message)
  "Insert MESSAGE into the TURN's message list, before the user message.
   Used for synthetic memory context injection.
   
   SIDE EFFECTS: Modifies turn-messages of the turn's conversation."
  (let ((messages (agent-turn-messages turn)))
    ;; Find the user message position (last user message)
    (let ((user-pos (position :user messages :key #'message-role :from-end t)))
      (when user-pos
        ;; Insert before user message
        (setf (agent-turn-messages turn)
              (append (subseq messages 0 user-pos)
                      (list message)
                      (subseq messages user-pos)))))))

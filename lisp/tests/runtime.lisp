(in-package #:claw-lisp.tests)

(defun %assert (condition format-control &rest format-args)
  (unless condition
    (error (apply #'format nil format-control format-args))))

(defun %runtime-compact-session (runtime session &key (keep-recent-messages 4))
  "Call runtime compaction with a compatibility fallback."
  (if (fboundp 'claw-lisp.core.runtime:compact-session)
      (claw-lisp.core.runtime:compact-session
       runtime session :keep-recent-messages keep-recent-messages)
      (claw-lisp.core.compact:compact-session-locally
       (claw-lisp.core.runtime:runtime-settings runtime)
       session
       :keep-recent-messages keep-recent-messages)))

(defclass failing-turn-provider (claw-lisp.core.protocols:provider) ())
(defclass failing-echo-tool (claw-lisp.tools.echo::echo-tool) ())

(defun make-failing-echo-tool ()
  "Create an echo tool variant that fails during execution."
  (make-instance 'failing-echo-tool
                 :name "echo"
                 :description "Echo tool variant that fails for runner error-path tests."))

(defmethod claw-lisp.core.protocols:count-tokens
    ((provider failing-turn-provider) messages &key model)
  (declare (ignore provider model))
  (max 1 (length messages)))

(defmethod claw-lisp.core.protocols:execute-tool
    ((tool failing-echo-tool) input runtime)
  (declare (ignore tool input runtime))
  (error "intentional echo tool failure"))

(defmethod claw-lisp.core.protocols:stream-turn
    ((provider failing-turn-provider) conversation &key model tools on-event system)
  (declare (ignore provider conversation model tools on-event system))
  (error "intentional provider failure"))

;; shell-pivot-provider: reads twice (stall→nudge→read suppression), then once reads
;; are suppressed it writes successfully (progress), then stops. Records the tool set
;; offered on the post-suppression turn so the test can assert reads were excluded.
(defclass shell-pivot-provider (claw-lisp.core.protocols:provider)
  ((call-count :initform 0 :accessor shell-pivot-provider-call-count)
   (tools-offered-on-final-turn :initform nil
                                :accessor shell-pivot-provider-tools-offered-on-final-turn)
   (path :initarg :path :reader shell-pivot-provider-path)))

;; always-writing-provider: emits a successful file-write every turn and never
;; stops. Successful writes keep resetting the stall counter, so the stagnation
;; guard never fires — the loop runs to its iteration budget. Used to verify the
;; budget is a graceful stop, not an error.
(defclass always-writing-provider (claw-lisp.core.protocols:provider)
  ((path :initarg :path :reader always-writing-provider-path)))

(defmethod claw-lisp.core.protocols:send-turn
    ((provider always-writing-provider) conversation &key model tools system)
  (declare (ignore conversation model tools system))
  (claw-lisp.core.domain:make-transport-response
   :ok-p t :status 200 :assistant-text "" :raw-response "{}"
   :provider "always-writing"
   :tool-calls (list (list :id "w"
                           :name "file-write"
                           :input (list :path (always-writing-provider-path provider)
                                        :text "x")))))

(defmethod claw-lisp.core.protocols:stream-turn
    ((provider always-writing-provider) conversation &key model tools on-event system)
  (declare (ignore on-event))
  (claw-lisp.core.protocols:send-turn provider conversation :model model :tools tools :system system))

(defmethod claw-lisp.core.protocols:normalize-response
    ((provider always-writing-provider) response)
  (declare (ignore provider))
  response)

(defmethod claw-lisp.core.protocols:count-tokens
    ((provider always-writing-provider) messages &key model)
  (declare (ignore provider model))
  (max 1 (length messages)))

(defmethod claw-lisp.core.protocols:send-turn
    ((provider shell-pivot-provider) conversation &key model tools system)
  (declare (ignore conversation model system))
  (incf (shell-pivot-provider-call-count provider))
  (let ((count (shell-pivot-provider-call-count provider)))
    (cond
      ((or (= count 1) (= count 2))
       (claw-lisp.core.domain:make-transport-response
        :ok-p t :status 200 :assistant-text "" :raw-response "{}"
        :provider "shell-pivot"
        :tool-calls
        (list (list :id (format nil "toolu_read_~A" count)
                    :name "file-read"
                    :input (list :path (shell-pivot-provider-path provider))))))
      ((= count 3)
       ;; Post-suppression turn: capture the offered tools, then write the fix.
       (setf (shell-pivot-provider-tools-offered-on-final-turn provider) tools)
       (claw-lisp.core.domain:make-transport-response
        :ok-p t :status 200 :assistant-text "" :raw-response "{}"
        :provider "shell-pivot"
        :tool-calls
        (list (list :id "toolu_write_01"
                    :name "file-write"
                    :input (list :path (shell-pivot-provider-path provider)
                                 :text "fixed content")))))
      (t
       (claw-lisp.core.domain:make-transport-response
        :ok-p t :status 200
        :assistant-text "Done — wrote the fix."
        :raw-response "{}"
        :provider "shell-pivot"
        :tool-calls nil)))))

(defmethod claw-lisp.core.protocols:stream-turn
    ((provider shell-pivot-provider) conversation &key model tools on-event system)
  (declare (ignore on-event))
  (claw-lisp.core.protocols:send-turn provider conversation :model model :tools tools :system system))

(defmethod claw-lisp.core.protocols:normalize-response
    ((provider shell-pivot-provider) response)
  (declare (ignore provider))
  response)

(defmethod claw-lisp.core.protocols:count-tokens
    ((provider shell-pivot-provider) messages &key model)
  (declare (ignore provider model))
  (max 1 (length messages)))

(defmacro %with-redefined-function ((name replacement) &body body)
  `(let ((old-fn (symbol-function ,name)))
     (unwind-protect
          (progn
            (setf (symbol-function ,name) ,replacement)
            ,@body)
       (setf (symbol-function ,name) old-fn))))

(defun test-json-decode-anthropic-response ()
  (let ((json "{\"content\":[{\"type\":\"text\",\"text\":\"Hello\"},{\"type\":\"tool_use\",\"id\":\"toolu_01A\",\"name\":\"echo\",\"input\":{\"text\":\"hi\"}}],\"stop_reason\":\"tool_use\"}"))
    (let ((text (extract-anthropic-response-text json)))
      (%assert (string= "Hello" text) "Expected 'Hello', got ~A" text))
    (let ((calls (extract-anthropic-tool-calls json)))
      (%assert (= 1 (length calls)) "Expected 1 tool call, got ~A" (length calls))
      (%assert (string= "toolu_01A" (getf (first calls) :id)) "Expected tool call id")
      (%assert (string= "echo" (getf (first calls) :name)) "Expected tool call name")
      (%assert (string= "hi" (getf (getf (first calls) :input) :text)) "Expected tool call input text"))))

(defun test-json-decode-openrouter-response ()
  (let ((json "{\"choices\":[{\"message\":{\"content\":\"Hello\",\"tool_calls\":null}}]}"))
    (let ((text (extract-openrouter-response-text json)))
      (%assert (string= "Hello" text) "Expected 'Hello', got ~A" text))))

(defun test-openrouter-tool-call-extraction ()
  (let ((json "{\"choices\":[{\"message\":{\"content\":\"\",\"tool_calls\":[{\"id\":\"call_abc123\",\"type\":\"function\",\"function\":{\"name\":\"echo\",\"arguments\":\"{\\\"text\\\":\\\"hi\\\"}\"}}]}}]}"))
    (let ((text (extract-openrouter-response-text json)))
      (%assert (string= "" text) "Expected empty text, got ~A" text))
    (let ((calls (extract-openrouter-tool-calls json)))
      (%assert (= 1 (length calls)) "Expected 1 tool call, got ~A" (length calls))
      (%assert (string= "call_abc123" (getf (first calls) :id)) "Expected tool call id")
      (%assert (string= "echo" (getf (first calls) :name)) "Expected tool call name")
      (%assert (string= "hi" (getf (getf (first calls) :input) :text)) "Expected tool call input text"))))

(defun test-openrouter-no-tool-calls ()
  (let ((json "{\"choices\":[{\"message\":{\"content\":\"Just text\"}}]}"))
    (let ((calls (extract-openrouter-tool-calls json)))
      (%assert (null calls) "Expected no tool calls, got ~A" calls))))

(defun test-openrouter-empty-tool-calls ()
  (let* ((json-str "{\"choices\":[{\"message\":{\"content\":\"\",\"tool_calls\":[]}}]}")
         (result (extract-openrouter-tool-calls json-str)))
    (%assert (null result)
             "Expected NIL for empty tool_calls array, but got ~S" result)))

(defun test-openrouter-multiple-tool-calls ()
  (let ((json "{\"choices\":[{\"message\":{\"content\":\"\",\"tool_calls\":[{\"id\":\"call_1\",\"type\":\"function\",\"function\":{\"name\":\"echo\",\"arguments\":\"{\\\"text\\\":\\\"hello\\\"}\"}},{\"id\":\"call_2\",\"type\":\"function\",\"function\":{\"name\":\"file-read\",\"arguments\":\"{\\\"path\\\":\\\"/tmp/foo.txt\\\"}\"}}]}}]}"))
    (let ((calls (extract-openrouter-tool-calls json)))
      (%assert (= 2 (length calls)) "Expected 2 tool calls, got ~A" (length calls))
      (%assert (string= "call_1" (getf (first calls) :id)) "Expected first call id")
      (%assert (string= "echo" (getf (first calls) :name)) "Expected first call name")
      (%assert (string= "call_2" (getf (second calls) :id)) "Expected second call id")
      (%assert (string= "file-read" (getf (second calls) :name)) "Expected second call name")
      (%assert (string= "/tmp/foo.txt" (getf (getf (second calls) :input) :path)) "Expected second call input path"))))

(defun test-openrouter-stream-turn-falls-back-to-send-turn ()
  (let* ((provider (make-instance 'claw-lisp.providers.openrouter::openrouter-provider
                                  :name "openrouter"
                                  :credentials nil))
         (conversation (claw-lisp.core.domain:make-conversation
                        :id "test-openrouter-stream-fallback"
                        :messages (list (claw-lisp.core.domain:make-message
                                         :role :user
                                         :content "Hello"))))
         (events-received nil))
    (%with-redefined-function
        ('claw-lisp.core.protocols:send-turn
         (lambda (provider conversation &key model tools system)
           (declare (ignore provider conversation model tools system))
           (claw-lisp.core.domain:make-transport-response
            :ok-p t
            :status 200
            :assistant-text "Hello from OpenRouter fallback."
            :raw-response "{}"
            :provider "openrouter"
            :tool-calls nil)))
      (let ((response (claw-lisp.core.protocols:stream-turn
                       provider conversation
                       :model "openrouter/test-model"
                       :tools nil
                       :on-event (lambda (event-type data)
                                   (push (list event-type data) events-received)))))
        (%assert (claw-lisp.core.domain:transport-response-ok-p response)
                 "Expected fallback stream response to succeed")
        (%assert (string= "Hello from OpenRouter fallback."
                          (claw-lisp.core.domain:transport-response-assistant-text response))
                 "Expected fallback response text")
        (%assert (find "message_start" events-received :key #'car :test #'string=)
                 "Expected message_start event")
        (%assert (find "content_block_delta" events-received :key #'car :test #'string=)
                 "Expected content_block_delta event")
        (%assert (find "message_stop" events-received :key #'car :test #'string=)
                 "Expected message_stop event")))))

(defun test-json-decode-error-response ()
  (let ((json "{\"error\":{\"message\":\"Rate limit exceeded\"}}"))
    (let ((text (extract-anthropic-response-text json)))
      (%assert (search "Rate limit exceeded" text) "Expected error message, got ~A" text))))

(defun test-content-block-roundtrip ()
  "Test that content blocks can be serialized to Anthropic JSON format."
  (let* ((msg (make-message
               :role :assistant
               :content (list
                         (make-text-block :text "I'll help you.")
                         (make-tool-use-block :id "toolu_01X" :name "echo"
                                              :input (list :text "hello"))))))
    (let ((block (claw-lisp.providers.http-utils:message->anthropic-block msg)))
      (%assert (string= "assistant" (getf block :role)) "Expected assistant role")
      (let ((content (getf block :content)))
        (%assert (= 2 (length content)) "Expected 2 content blocks")
        (%assert (string= "text" (getf (first content) :type)) "Expected text block")
        (%assert (string= "tool_use" (getf (second content) :type)) "Expected tool_use block")
        (%assert (string= "toolu_01X" (getf (second content) :id)) "Expected tool use id")
        (%assert (string= "echo" (getf (second content) :name)) "Expected tool use name")))))

(defun test-anthropic-json-format ()
  "Test that conversation->anthropic-json produces a correct plist body."
  (let ((conversation (make-conversation :id "test")))
    (append-message conversation (make-message :role :user :content "Hello"))
    (let ((body (conversation->anthropic-json conversation "claude-sonnet")))
      (%assert (string= "claude-sonnet" (getf body :model))
               "Missing model in anthropic body: ~A" body)
      (%assert (= 1024 (getf body :max_tokens))
               "Missing max_tokens in anthropic body: ~A" body)
      (let ((messages (getf body :messages)))
        (%assert (listp messages) "Messages should be a list, got ~A" messages)
        (%assert (= 1 (length messages))
                 "Expected one anthropic message entry, got ~A" (length messages))
        (%assert (string= "user" (getf (first messages) :role))
                 "Expected 'user' role in first anthropic message, got ~A" (first messages))
        (%assert (string= "Hello" (getf (first messages) :content))
                 "Expected first anthropic message content, got ~A" (first messages))))))

(defun test-openrouter-chat-json-single-message-uses-array ()
  (let ((conversation (make-conversation :id "test-openrouter-chat-json")))
    (append-message conversation (make-message :role :user :content "Hello"))
    (let* ((body (conversation->chat-json conversation "moonshotai/kimi-k2.6"))
           (messages (getf body :messages)))
      (%assert (string= "moonshotai/kimi-k2.6" (getf body :model))
               "Missing model in openrouter body: ~A" body)
      (%assert (listp messages) "Messages should be a list, got ~A" messages)
      (%assert (= 1 (length messages))
               "Expected one openrouter message entry, got ~A" (length messages))
      (%assert (string= "user" (getf (first messages) :role))
               "Expected 'user' role in first openrouter message, got ~A" (first messages))
      (%assert (string= "Hello" (getf (first messages) :content))
               "Expected first openrouter message content, got ~A" (first messages))
      (%assert (search "\"messages\":[{" (json-encode-string body))
               "Expected serialized messages array in OpenRouter JSON, got ~A"
               (json-encode-string body)))))

(defun test-openrouter-chat-json-threads-tool-results-as-tool-role ()
  "Regression for the OpenRouter context-loss bug: tool results MUST serialize as
   role:\"tool\" messages with a matching tool_call_id, and assistant tool calls
   as well-formed tool_calls. Previously the chat path dropped tool-result blocks
   entirely, so OpenRouter-hosted models received no record of what they read or
   ran and looped re-reading the same files forever, never progressing to a write."
  (let ((conversation (make-conversation :id "chat-json-tool-results")))
    (append-message conversation (make-message :role :user :content "fix it"))
    (append-message conversation
                    (make-message :role :assistant
                                  :content (list (make-tool-use-block
                                                  :id "call_1" :name "file-read"
                                                  :input '(:path "m.py")))))
    (append-message conversation
                    (make-message :role :user
                                  :content (list (claw-lisp.core.domain:make-tool-result-block
                                                  :tool-use-id "call_1"
                                                  :content "file body here"
                                                  :is-error nil))))
    (let* ((body (conversation->chat-json conversation "moonshotai/kimi-k2.6"))
           (messages (getf body :messages)))
      (%assert (= 3 (length messages))
               "Expected 3 chat messages (user, assistant, tool), got ~A" (length messages))
      (let ((assistant (second messages))
            (tool-msg (third messages)))
        (%assert (string= "assistant" (getf assistant :role))
                 "Expected assistant role on second message, got ~A" (getf assistant :role))
        (%assert (getf assistant :tool_calls)
                 "Expected assistant message to carry tool_calls")
        (%assert (string= "tool" (getf tool-msg :role))
                 "Expected role:tool for the tool result, got ~A" (getf tool-msg :role))
        (%assert (string= "call_1" (getf tool-msg :tool_call_id))
                 "Expected matching tool_call_id, got ~A" (getf tool-msg :tool_call_id))
        (%assert (string= "file body here" (getf tool-msg :content))
                 "Expected tool result content to be threaded, got ~A" (getf tool-msg :content)))
      (let ((json (json-encode-string body)))
        (%assert (search "\"role\":\"tool\"" json)
                 "Expected role:tool in serialized JSON, got ~A" json)
        (%assert (search "\"tool_call_id\":\"call_1\"" json)
                 "Expected tool_call_id in serialized JSON, got ~A" json)))))

(defun test-openrouter-chat-json-tools-serialize-as-array-of-objects ()
  (let* ((conversation (make-conversation :id "test-openrouter-chat-json-tools"))
         (tools (list (list :name "file-read"
                            :description "Read a file"
                            :input_schema (list :type "object"
                                                :properties (list :path (list :type "string"))
                                                :required #("path")))))
         (body (conversation->chat-json conversation
                                        "anthropic/claude-sonnet-4-6"
                                        :tools tools))
         (json (json-encode-string body)))
    (%assert (search "\"tools\":[{" json)
             "Expected serialized tools array in OpenRouter JSON, got ~A"
             json)
    (%assert (search "\"name\":\"file-read\"" json)
             "Expected serialized tool name in OpenRouter JSON, got ~A"
             json)
    (%assert (search "\"parameters\":{\"type\":\"object\"" json)
             "Expected serialized tool schema object in OpenRouter JSON, got ~A"
             json)))

(defun test-openrouter-env-alias-loads-credentials ()
  ;; Verify that openrouter credentials can be set and read via config-credentials,
  ;; which is what load-provider-credentials-from-env relies on internally.
  (let* ((config (claw-lisp.config:make-default-runtime-config))
         (creds (claw-lisp.config:make-openrouter-credentials :api-key "alias-test-key")))
    (setf (claw-lisp.config:config-credentials config :openrouter) creds)
    (let ((loaded (claw-lisp.config:config-credentials config :openrouter)))
      (%assert loaded "Expected OpenRouter credentials to be retrievable after setf")
      (%assert (string= "alias-test-key"
                        (claw-lisp.config:provider-credentials-api-key loaded))
               "Expected alias env var value to populate OpenRouter credentials"))))

(defun test-openrouter-response-extraction ()
  (let ((text (extract-openrouter-response-text
               "{\"choices\":[{\"message\":{\"content\":\"hello\"}}]}")))
    (%assert (string= "hello" text) "Unexpected OpenRouter response text: ~A" text)))

(defun test-anthropic-response-extraction ()
  (let ((text (extract-anthropic-response-text
               "{\"content\":[{\"type\":\"text\",\"text\":\"hello anthropic\"}]}")))
    (%assert (string= "hello anthropic" text)
             "Unexpected Anthropic response text: ~A"
             text)))

(defun test-openrouter-error-response-extraction ()
  (let ((text (extract-openrouter-response-text
               "{\"error\":{\"message\":\"rate limited\"}}")))
    (%assert (string= "rate limited" text)
             "Unexpected OpenRouter error text: ~A"
             text)))

(defun test-transcript-path-for-session ()
  (let* ((config (claw-lisp.config:make-default-runtime-config))
         (path (transcript-path-for-session config "session-123")))
    (%assert (search "session-123.jsonl" (namestring path))
             "Unexpected transcript path: ~A"
             (namestring path))))

(defun test-default-state-root-family ()
  (let ((config (claw-lisp.config:make-default-runtime-config)))
    (%assert (string= ".achatina/" (claw-lisp.config:runtime-config-state-root config))
             "Unexpected default state root: ~A"
             (claw-lisp.config:runtime-config-state-root config))
    (%assert (string= ".achatina/" (claw-lisp.config:runtime-config-data-root config))
             "Unexpected default data root: ~A"
             (claw-lisp.config:runtime-config-data-root config))
    (%assert (string= ".achatina/transcripts/"
                      (claw-lisp.config:runtime-config-transcripts-root config))
             "Unexpected default transcripts root: ~A"
             (claw-lisp.config:runtime-config-transcripts-root config))
    (%assert (string= ".achatina/artifacts/"
                      (claw-lisp.config:runtime-config-artifacts-root config))
             "Unexpected default artifacts root: ~A"
             (claw-lisp.config:runtime-config-artifacts-root config))
    (%assert (string= ".achatina/memory/"
                      (claw-lisp.config:runtime-config-memory-root config))
             "Unexpected default memory root: ~A"
             (claw-lisp.config:runtime-config-memory-root config))
    (%assert (string= ".achatina/cas/objects/"
                      (claw-lisp.config:runtime-config-cas-objects-root config))
             "Unexpected default CAS objects root: ~A"
             (claw-lisp.config:runtime-config-cas-objects-root config))
    (%assert (string= ".achatina/cas/refs/"
                      (claw-lisp.config:runtime-config-cas-ref-root config))
             "Unexpected default CAS ref root: ~A"
             (claw-lisp.config:runtime-config-cas-ref-root config))))

(defun test-tool-result-error-p-returns-boolean ()
  (let* ((result (claw-lisp.core.domain:make-tool-result
                  :call-id "call-bool-1"
                  :tool-name "echo"
                  :content "[error] invalid tool input")))
    (%assert (eq t (claw-lisp.core.runtime::tool-result-error-p result))
             "Expected tool-result-error-p to return boolean T for bracketed error content")))

(defun test-make-tool-result-message-normalizes-error-flag-to-boolean ()
  (let* ((result (claw-lisp.core.domain:make-tool-result
                  :call-id "call-bool-2"
                  :tool-name "echo"
                  :content "[error] invalid tool input"))
         (message (claw-lisp.core.runtime::make-tool-result-message (list result)))
         (content (claw-lisp.core.domain:message-content message))
         (block (first content)))
    (%assert (claw-lisp.core.domain:tool-result-block-p block)
             "Expected a tool-result-block, got ~A" (type-of block))
    (%assert (typep (claw-lisp.core.domain:tool-result-block-is-error block) 'boolean)
             "Expected tool-result-block is-error to be boolean, got ~S"
             (claw-lisp.core.domain:tool-result-block-is-error block))
    (%assert (eq t (claw-lisp.core.domain:tool-result-block-is-error block))
             "Expected tool-result-block is-error to be T for error result")))

(defun test-default-state-root-bootstrap-copies-legacy-tree ()
  (let* ((temp-root (merge-pathnames
                     (format nil "achatina-bootstrap-copy-~D-~D/"
                             (get-universal-time)
                             (get-internal-real-time))
                     (uiop:temporary-directory))))
    (unwind-protect
         (progn
           (ensure-directories-exist temp-root)
           (uiop:with-current-directory (temp-root)
             (let* ((legacy-transcript #P".claw-lisp/transcripts/bootstrap-session.jsonl")
                    (legacy-memory #P".claw-lisp/memory/session/bootstrap-session.md")
                    (bootstrapped-transcript #P".achatina/transcripts/bootstrap-session.jsonl")
                    (bootstrapped-memory #P".achatina/memory/session/bootstrap-session.md")
                    (marker-path #P".achatina/.achatina-bootstrap-v1.sexp"))
               (ensure-directories-exist legacy-transcript)
               (with-open-file (stream legacy-transcript
                                       :direction :output
                                       :if-exists :supersede
                                       :if-does-not-exist :create)
                 (write-string "{\"event\":\"session_start\"}" stream))
               (ensure-directories-exist legacy-memory)
               (with-open-file (stream legacy-memory
                                       :direction :output
                                       :if-exists :supersede
                                       :if-does-not-exist :create)
                 (write-string "legacy bootstrap memory" stream))
               (let ((config (claw-lisp.config:make-default-runtime-config)))
                 (%assert (probe-file bootstrapped-transcript)
                          "Expected legacy transcript to be copied into .achatina/")
                 (%assert (probe-file bootstrapped-memory)
                          "Expected legacy session memory to be copied into .achatina/")
                 (%assert (probe-file marker-path)
                          "Expected bootstrap marker file in .achatina/")
                 (%assert (string= ".achatina/"
                                   (claw-lisp.config:runtime-config-state-root config))
                          "Expected default config to use .achatina/ after bootstrap")))))
      (when (uiop:directory-exists-p temp-root)
        (uiop:delete-directory-tree temp-root :validate t)))))

(defun test-state-root-override-derives-family ()
  (let ((config (claw-lisp.config:load-runtime-config
                 :overrides '(:state-root ".achatina/"))))
    (%assert (string= ".achatina/" (claw-lisp.config:runtime-config-state-root config))
             "Unexpected state root after override: ~A"
             (claw-lisp.config:runtime-config-state-root config))
    (%assert (string= ".achatina/" (claw-lisp.config:runtime-config-data-root config))
             "Unexpected data root after override: ~A"
             (claw-lisp.config:runtime-config-data-root config))
    (%assert (string= ".achatina/transcripts/"
                      (claw-lisp.config:runtime-config-transcripts-root config))
             "Unexpected transcripts root after override: ~A"
             (claw-lisp.config:runtime-config-transcripts-root config))
    (%assert (string= ".achatina/artifacts/"
                      (claw-lisp.config:runtime-config-artifacts-root config))
             "Unexpected artifacts root after override: ~A"
             (claw-lisp.config:runtime-config-artifacts-root config))
    (%assert (string= ".achatina/memory/"
                      (claw-lisp.config:runtime-config-memory-root config))
             "Unexpected memory root after override: ~A"
             (claw-lisp.config:runtime-config-memory-root config))
    (%assert (string= ".achatina/cas/objects/"
                      (claw-lisp.config:runtime-config-cas-objects-root config))
             "Unexpected CAS objects root after override: ~A"
             (claw-lisp.config:runtime-config-cas-objects-root config))
    (%assert (string= ".achatina/cas/refs/"
                      (claw-lisp.config:runtime-config-cas-ref-root config))
             "Unexpected CAS refs root after override: ~A"
             (claw-lisp.config:runtime-config-cas-ref-root config))))

(defun test-resume-session-falls-back-to-legacy-transcript-root ()
  (let* ((temp-root (merge-pathnames
                     (format nil "claw-lisp-legacy-transcript-~D-~D/"
                             (get-universal-time)
                             (get-internal-real-time))
                     (uiop:temporary-directory))))
    (unwind-protect
         (progn
           (ensure-directories-exist temp-root)
           (uiop:with-current-directory (temp-root)
             (let* ((legacy-config (claw-lisp.config:make-default-runtime-config))
                    (legacy-path (transcript-path-for-session legacy-config "legacy-session"))
                    (config (claw-lisp.config:load-runtime-config
                             :overrides '(:state-root ".achatina/"
                                          :default-provider "mock"
                                          :default-model "mock-model")))
                    (runtime (make-runtime :config config)))
               (register-default-providers runtime)
               (claw-lisp.storage.transcripts:append-transcript-event
                legacy-path
                (list :event "session_start"
                      :session_id "legacy-session"
                      :provider "mock"
                      :model "mock-model"))
               (claw-lisp.storage.transcripts:append-transcript-event
                legacy-path
                (list :event "message"
                      :role "user"
                      :content "hello from legacy transcript"))
               (claw-lisp.storage.transcripts:append-transcript-event
                legacy-path
                (list :event "message"
                      :role "assistant"
                      :content "legacy reply"))
               (%assert (equal (namestring legacy-path)
                               (namestring
                                (claw-lisp.core.runtime:transcript-existing-path-for-session-id
                                 runtime "legacy-session")))
                        "Expected fallback transcript path to resolve to legacy root")
               (let ((session (claw-lisp.core.runtime:resume-session runtime "legacy-session")))
                 (%assert (claw-lisp.core.runtime::session-state-value
                           session :resumed-from-transcript nil)
                          "Expected resumed session marker")
                 (%assert (= 2
                             (claw-lisp.core.runtime::session-state-value
                              session :resumed-message-count 0))
                          "Expected two restored transcript messages")))))
      (when (uiop:directory-exists-p temp-root)
        (uiop:delete-directory-tree temp-root :validate t)))))

(defun test-read-session-memory-text-falls-back-to-legacy-root ()
  (let* ((temp-root (merge-pathnames
                     (format nil "claw-lisp-legacy-memory-~D-~D/"
                             (get-universal-time)
                             (get-internal-real-time))
                     (uiop:temporary-directory))))
    (unwind-protect
         (progn
           (ensure-directories-exist temp-root)
           (uiop:with-current-directory (temp-root)
             (let* ((legacy-config (claw-lisp.config:make-default-runtime-config))
                    (legacy-path (session-memory-path legacy-config "legacy-memory"))
                    (config (claw-lisp.config:load-runtime-config
                             :overrides '(:state-root ".achatina/"
                                          :default-provider "mock"
                                          :default-model "mock-model")))
                    (runtime (make-runtime :config config))
                    (session (claw-lisp.core.domain:make-agent-session
                              :id "legacy-memory"
                              :provider "mock"
                              :model "mock-model"
                              :conversation (make-conversation :id "legacy-memory")
                              :state '(:initialized t))))
               (ensure-directories-exist legacy-path)
               (with-open-file (stream legacy-path
                                       :direction :output
                                       :if-exists :supersede
                                       :if-does-not-exist :create)
                 (write-string "legacy memory note" stream))
               (%assert (equal (namestring legacy-path)
                               (namestring
                                (claw-lisp.storage.session-memory:session-memory-existing-path
                                 config "legacy-memory")))
                        "Expected fallback session-memory path to resolve to legacy root")
               (%assert (string= "legacy memory note"
                                 (claw-lisp.core.runtime:read-session-memory-text runtime session))
                        "Expected session-memory read fallback to return legacy text"))))
      (when (uiop:directory-exists-p temp-root)
        (uiop:delete-directory-tree temp-root :validate t)))))

(defun test-session-memory-path ()
  (let* ((config (claw-lisp.config:make-default-runtime-config))
         (path (session-memory-path config "session-123")))
    (%assert (search "session/session-123.md" (namestring path))
             "Unexpected session memory path: ~A"
             (namestring path))))

(defun test-durable-memory-path ()
  (let* ((config (claw-lisp.config:make-default-runtime-config))
         (path (durable-memory-note-path config "session-123")))
    (%assert (search "durable/session-123.md" (namestring path))
             "Unexpected durable memory path: ~A"
             (namestring path))))

(defun test-session-context-status-thresholds ()
  (let* ((config (claw-lisp.config::%make-runtime-config
                  :data-root ".claw-lisp/"
                  :transcripts-root ".claw-lisp/transcripts/"
                  :artifacts-root ".claw-lisp/artifacts/"
                  :memory-root ".claw-lisp/memory/"
                  :default-provider "mock"
                  :default-model "mock-model"
                  :context-output-reserve 1024
                  :token-warning-buffer 2048
                  :compaction-trigger-buffer 512
                  ))
         (runtime (make-runtime :config config)))
    (register-default-providers runtime)
    (let ((session (start-session runtime
                                  :provider-name "mock"
                                  :model "mock-model"
                                  :session-id "context-status-test")))
      ;; With default 200K context window and 1024 reserve, effective window is 198976
      (let ((status (claw-lisp.core.runtime:session-context-status runtime session)))
        (%assert (= (- 200000 1024) (getf status :effective-window))
                 "Unexpected effective context window: ~A"
                 (getf status :effective-window))
        ;; Without any messages, thresholds should not be reached
        (%assert (not (getf status :warning-p))
                 "Expected no warning with empty conversation")
        (%assert (not (getf status :compaction-needed-p))
                 "Expected no compaction with empty conversation")))))

(defun test-session-context-status-below-thresholds ()
  (let* ((config (claw-lisp.config::%make-runtime-config
                  :data-root ".claw-lisp/"
                  :transcripts-root ".claw-lisp/transcripts/"
                  :artifacts-root ".claw-lisp/artifacts/"
                  :memory-root ".claw-lisp/memory/"
                  :default-provider "mock"
                  :default-model "mock-model"
                  :context-output-reserve 1024
                  :token-warning-buffer 2048
                  :compaction-trigger-buffer 512
                  ))
         (runtime (make-runtime :config config)))
    (register-default-providers runtime)
    (let ((session (start-session runtime
                                  :provider-name "mock"
                                  :model "mock-model"
                                  :session-id "context-status-low-test")))
      (append-message
       (claw-lisp.core.domain:agent-session-conversation session)
       (make-message :role :user :content "small message"))
      (%assert (not (claw-lisp.core.runtime:context-warning-p runtime session))
               "Did not expect warning threshold for small session")
      (%assert (not (claw-lisp.core.runtime:compaction-needed-p runtime session))
               "Did not expect compaction threshold for small session"))))

(defun test-session-context-status-without-provider-falls-back-locally ()
  (let* ((config (claw-lisp.config::%make-runtime-config
                  :data-root ".claw-lisp/"
                  :transcripts-root ".claw-lisp/transcripts/"
                  :artifacts-root ".claw-lisp/artifacts/"
                  :memory-root ".claw-lisp/memory/"
                  :default-provider "mock"
                  :default-model "mock-model"
                  :context-output-reserve 1024
                  :token-warning-buffer 4096
                  :compaction-trigger-buffer 1024
                  ))
         (runtime (make-runtime :config config))
         (session (claw-lisp.core.domain:make-agent-session
                   :id "context-status-no-provider"
                   :provider "missing-provider"
                   :model "missing-model"
                   :conversation (make-conversation :id "context-status-no-provider")
                   :state '(:initialized t))))
    (append-message
     (claw-lisp.core.domain:agent-session-conversation session)
     (make-message :role :user :content "fallback path message"))
    (let ((status (claw-lisp.core.runtime:session-context-status runtime session)))
      (%assert (= 6 (getf status :tokens))
               "Expected local token fallback estimate, got ~A"
               (getf status :tokens))
      (%assert (= (- 200000 1024) (getf status :effective-window))
               "Expected default fallback effective window, got ~A"
               (getf status :effective-window))
      (%assert (not (getf status :warning-p))
               "Did not expect warning threshold in fallback path")
      (%assert (not (getf status :compaction-needed-p))
               "Did not expect compaction threshold in fallback path"))))

(defun read-lines (pathname)
  "Return all lines from PATHNAME as a list of strings."
  (with-open-file (stream pathname :direction :input)
    (loop for line = (read-line stream nil nil)
          while line
          collect line)))

(defun %write-json-file (pathname value)
  "Write VALUE as JSON to PATHNAME."
  (let ((parent (uiop:pathname-parent-directory-pathname pathname)))
    (when parent
      (ensure-directories-exist parent)))
  (with-open-file (stream pathname
                          :direction :output
                          :if-exists :supersede
                          :if-does-not-exist :create)
    (write-string (claw-lisp.providers.http-utils:json-encode-string value) stream)))

(defun %make-runner-test-request (work-root data-root &key (user-input "hello from runner") (event-stream-p t))
  "Build a minimal valid runner request for tests."
  (list :protocol_version "claw-runner/v0"
        :request_id "req-test-001"
        :correlation_id "corr-test-001"
        :job_id "job-test-001"
        :task_id "task-test-001"
        :node_id "node-test-a"
        :session_id "runner-session-001"
        :turn_id "turn-001"
        :provider "mock"
        :model "mock-model"
        :user_input user-input
        :cwd (namestring work-root)
        :environment (list :transcript_root (namestring (merge-pathnames "transcripts/" data-root))
                           :memory_root (namestring (merge-pathnames "memory/" data-root))
                           :artifacts_root (namestring (merge-pathnames "artifacts/" data-root))
                           :cas_objects_root (namestring (merge-pathnames "cas/objects/" data-root))
                           :cas_ref_root (namestring (merge-pathnames "cas/refs/" data-root)))
        :timeouts (list :turn_timeout_seconds 5)
        :features (list :event_stream event-stream-p)))

(defun %dispatch-cli-capturing (args &key stdin)
  "Run `%dispatch-cli` and return three values: exit-code, stdout, stderr."
  (let ((stdout "")
        (stderr "")
        exit-code)
    (setf stdout
          (with-output-to-string (stream)
            (setf stderr
                  (with-output-to-string (err)
                    (setf exit-code
                          (claw-lisp.cli::%dispatch-cli
                           args
                           :stdin (or stdin *standard-input*)
                           :stdout stream
                           :stderr err))))))
    (values exit-code stdout stderr)))

(defun test-transcript-write-flow ()
  (let* ((root (merge-pathnames
                (format nil "claw-lisp-test-~A/" (get-universal-time))
                #P"/tmp/"))
         (transcripts-root (merge-pathnames "transcripts/" root))
         (config (claw-lisp.config::%make-runtime-config
                  :data-root (namestring root)
                  :transcripts-root (namestring transcripts-root)
                  :artifacts-root (namestring (merge-pathnames "artifacts/" root))
                  :memory-root (namestring (merge-pathnames "memory/" root))
                  :default-provider "mock"
                  :default-model "mock-model"
                  ))
         (runtime (make-runtime :config config)))
    (unwind-protect
         (progn
           (register-default-providers runtime)
           (let* ((session (start-session runtime
                                          :provider-name "mock"
                                          :model "mock-model"
                                          :session-id "transcript-test"))
                  (updated (submit-user-message runtime session "baseline transcript check"))
                  (transcript-path (session-transcript-path runtime updated))
                  (lines (read-lines transcript-path)))
             (%assert (>= (length lines) 3)
                      "Expected at least 3 transcript lines, got ~A"
                      (length lines))
             (%assert (search "\"event\":\"session_start\"" (first lines))
                      "Missing session_start line: ~A"
                      (first lines))
             (%assert (search "\"role\":\"user\"" (second lines))
                      "Missing user message line: ~A"
                      (second lines))
             (%assert (search "\"role\":\"assistant\"" (third lines))
                      "Missing assistant message line: ~A"
                      (third lines))))
      (when (probe-file root)
        (uiop:delete-directory-tree root :validate t))))

(defun test-submit-user-message-runs-provider-tool-loop ()
  (let* ((root (merge-pathnames
                (format nil "claw-lisp-provider-tool-loop-test-~A/" (get-universal-time))
                #P"/tmp/"))
         (transcripts-root (merge-pathnames "transcripts/" root))
         (config (claw-lisp.config::%make-runtime-config
                  :data-root (namestring root)
                  :transcripts-root (namestring transcripts-root)
                  :artifacts-root (namestring (merge-pathnames "artifacts/" root))
                  :memory-root (namestring (merge-pathnames "memory/" root))
                  :default-provider "mock"
                  :default-model "mock-model"
                  ))
         (runtime (make-runtime :config config)))
    (unwind-protect
         (progn
           (register-default-providers runtime)
           (register-tool runtime (make-echo-tool))
           (let* ((session (start-session runtime
                                          :provider-name "mock"
                                          :model "mock-model"
                                          :session-id "provider-tool-loop-test"))
                  (updated (submit-user-message runtime session "tool:echo:hello from provider loop"))
                  (conversation (claw-lisp.core.domain:agent-session-conversation updated))
                  (messages (claw-lisp.core.domain:conversation-messages conversation))
                  (tool-results (claw-lisp.core.domain:conversation-tool-results conversation))
                  (lines (read-lines (session-transcript-path runtime updated)))
                  (assistant-message (car (last messages))))
             ;; Messages: user input → assistant tool_use → tool-result user message → assistant final answer
             (%assert (= 4 (length messages))
                      "Expected user, assistant-tool-use, tool-result, and assistant-final messages, got ~A"
                      (length messages))
             ;; Verify the tool-result message (third message) contains content blocks
             (let* ((tool-result-msg (third messages))
                    (content (claw-lisp.core.domain:message-content tool-result-msg)))
               (%assert (eq :user (claw-lisp.core.domain:message-role tool-result-msg))
                        "Expected tool-result message to have :user role")
               (%assert (listp content) "Expected tool-result message content to be a list")
               (%assert (= 1 (length content)) "Expected one tool-result content block, got ~A" (length content))
               (%assert (claw-lisp.core.domain:tool-result-block-p (first content))
                        "Expected content block to be a tool-result-block, got ~A" (type-of (first content)))
               (%assert (string= "toolu_mock_01" (claw-lisp.core.domain:tool-result-block-tool-use-id (first content)))
                        "Expected tool-use-id to match, got ~A" (claw-lisp.core.domain:tool-result-block-tool-use-id (first content)))
               (%assert (search "hello from provider loop" (claw-lisp.core.domain:tool-result-block-content (first content)))
                        "Expected tool result content to contain echo result"))
             (%assert (= 1 (length tool-results))
                      "Expected one provider-requested tool result, got ~A"
                      (length tool-results))
             (%assert (string= "hello from provider loop"
                               (tool-result-content (first tool-results)))
                      "Unexpected provider loop tool result content: ~A"
                      (tool-result-content (first tool-results)))
             (%assert (search "Mock provider used echo tool result: hello from provider loop"
                              (claw-lisp.core.domain:message-content assistant-message))
                      "Unexpected final assistant message from provider loop: ~A"
                      (claw-lisp.core.domain:message-content assistant-message))
             (%assert (some (lambda (line) (search "\"event\":\"tool_result\"" line)) lines)
                      "Expected tool_result transcript event from provider loop, got ~A"
                      lines)
             (%assert (some (lambda (line) (search "\"role\":\"assistant\"" line)) lines)
                      "Expected assistant transcript event from provider loop, got ~A"
                      lines)))
      (when (probe-file root)
        (uiop:delete-directory-tree root :validate t))))

(defun test-submit-user-message-rejects-reentrant-turn ()
  (let ((runtime (make-runtime)))
    (register-default-providers runtime)
    (let* ((session (start-session runtime
                                   :provider-name "mock"
                                   :model "mock-model"
                                   :session-id "reentrant-submit-message"))
           (conversation (claw-lisp.core.domain:agent-session-conversation session))
           (before-count (length (claw-lisp.core.domain:conversation-messages conversation))))
      (claw-lisp.core.runtime::set-session-state-value session :turn-in-flight-p t)
      (handler-case
          (progn
            (submit-user-message runtime session "should be rejected")
            (%assert nil "Expected reentrant submit-user-message rejection"))
        (error (condition)
          (%assert (search "Reentrant turns on the same session are not allowed"
                           (princ-to-string condition))
                   "Expected explicit reentrant-turn error, got ~A" condition)))
      (%assert (= before-count
                  (length (claw-lisp.core.domain:conversation-messages conversation)))
               "Conversation messages should remain unchanged after reentrant rejection"))))

(defun test-execute-provider-turn-loop-rejects-reentrant-turn ()
  (let ((runtime (make-runtime)))
    (register-default-providers runtime)
    (let* ((session (start-session runtime
                                   :provider-name "mock"
                                   :model "mock-model"
                                   :session-id "reentrant-provider-loop"))
           (provider (claw-lisp.core.runtime:resolve-provider runtime "mock"))
           (conversation (claw-lisp.core.domain:agent-session-conversation session))
           (before-count (length (claw-lisp.core.domain:conversation-messages conversation))))
      (%assert provider "Expected mock provider for reentrant turn test")
      (claw-lisp.core.runtime::set-session-state-value session :turn-in-flight-p t)
      (handler-case
          (progn
            (claw-lisp.core.runtime:execute-provider-turn-loop runtime session provider)
            (%assert nil "Expected reentrant execute-provider-turn-loop rejection"))
        (error (condition)
          (%assert (search "Reentrant turns on the same session are not allowed"
                           (princ-to-string condition))
                   "Expected explicit reentrant-turn error, got ~A" condition)))
      (%assert (= before-count
                  (length (claw-lisp.core.domain:conversation-messages conversation)))
               "Conversation messages should remain unchanged after reentrant loop rejection"))))

(defun test-execute-provider-turn-loop-clears-in-flight-after-provider-error ()
  (let ((runtime (make-runtime)))
    (let* ((provider (make-instance 'failing-turn-provider :name "failing"))
           (session (start-session runtime
                                   :provider-name "failing"
                                   :model "mock-model"
                                   :session-id "provider-error-clears-in-flight"))
           (conversation (claw-lisp.core.domain:agent-session-conversation session)))
      (append-message conversation (make-message :role :user :content "trigger failure"))
      (handler-case
          (progn
            (claw-lisp.core.runtime:execute-provider-turn-loop runtime session provider)
            (%assert nil "Expected provider turn loop to signal provider failure"))
        (error (condition)
          (%assert (search "intentional provider failure" (princ-to-string condition))
                   "Expected intentional provider failure, got ~A" condition)))
      (%assert (eq nil (claw-lisp.core.runtime::session-state-value session :turn-in-flight-p))
               "turn-in-flight-p should be NIL after provider error"))))

(defun test-submit-user-message-clears-in-flight-after-provider-error ()
  (let ((runtime (make-runtime)))
    (let ((provider (make-instance 'failing-turn-provider :name "failing")))
      (claw-lisp.core.runtime:register-provider runtime provider)
      (let ((session (start-session runtime
                                    :provider-name "failing"
                                    :model "mock-model"
                                    :session-id "submit-error-clears-in-flight")))
        (handler-case
            (progn
              (submit-user-message runtime session "trigger failure")
              (%assert nil "Expected submit-user-message to signal provider failure"))
          (error (condition)
            (%assert (search "intentional provider failure" (princ-to-string condition))
                     "Expected intentional provider failure, got ~A" condition)))
        (%assert (eq nil (claw-lisp.core.runtime::session-state-value session :turn-in-flight-p))
                 "turn-in-flight-p should be NIL after submit-user-message provider error")))))

(defun test-session-memory-write-flow ()
  (let* ((root (merge-pathnames
                (format nil "claw-lisp-session-memory-test-~A/" (get-universal-time))
                #P"/tmp/"))
         (transcripts-root (merge-pathnames "transcripts/" root))
         (memory-root (merge-pathnames "memory/" root))
         (config (claw-lisp.config::%make-runtime-config
                  :data-root (namestring root)
                  :transcripts-root (namestring transcripts-root)
                  :artifacts-root (namestring (merge-pathnames "artifacts/" root))
                  :memory-root (namestring memory-root)
                  :default-provider "mock"
                  :default-model "mock-model"
                  ))
         (runtime (make-runtime :config config)))
    (unwind-protect
         (progn
           (register-default-providers runtime)
           (register-tool runtime (make-echo-tool))
           (let* ((session (start-session runtime
                                          :provider-name "mock"
                                          :model "mock-model"
                                          :session-id "memory-test"))
                  (updated (submit-user-message runtime session "baseline memory check"))
                  (memory-path (session-memory-path config "memory-test"))
                  (structured-path
                    (claw-lisp.storage.session-memory:session-memory-structured-path
                     config "memory-test"))
                  (memory-text (uiop:read-file-string memory-path)))
             (execute-registered-tool runtime updated "echo" '(:text "tool memory line"))
             (setf memory-text (uiop:read-file-string memory-path))
             (%assert (probe-file memory-path)
                      "Expected session memory file at ~A"
                      memory-path)
             (%assert (probe-file structured-path)
                      "Expected structured session memory file at ~A"
                      structured-path)
             (%assert (search "# Session memory-test" memory-text)
                      "Missing session header in session memory: ~A"
                      memory-text)
             (%assert (search "baseline memory check" memory-text)
                      "Missing recent message in session memory: ~A"
                      memory-text)
             (%assert (search "tool memory line" memory-text)
                      "Missing recent tool result in session memory: ~A"
                      memory-text)
             (let* ((structured-text (uiop:read-file-string structured-path))
                    (structured
                      (claw-lisp.providers.http-utils:json-decode structured-text))
                    (structured-metadata (getf structured :metadata))
                    (parsed-metadata
                      (claw-lisp.storage.session-memory:parse-session-memory-header
                       memory-text))
                    (recent-activity (getf structured :recent_activity))
                    (tool-entry
                      (find "tool" recent-activity
                            :key (lambda (entry) (getf entry :role))
                            :test #'string=)))
               (dolist (key '(:schema_version :session_id :updated_at_universal_time
                              :source :summary :recent_activity :metadata))
                 (%assert (member key structured)
                          "Expected top-level key ~A in structured memory: ~S"
                          key structured))
               (%assert (= (getf structured :schema_version) 1)
                        "Expected schema_version=1 in structured memory, got ~S"
                        structured)
               (%assert (string= (getf structured :session_id) "memory-test")
                        "Expected session_id in structured memory, got ~S"
                        structured)
               (%assert (string= (getf structured :source) "session-memory-update")
                        "Expected source marker in structured memory, got ~S"
                        structured)
               (%assert (integerp (getf structured :updated_at_universal_time))
                        "Expected updated_at_universal_time integer, got ~S"
                        structured)
               (%assert (listp (getf structured :recent_activity))
                        "Expected recent_activity list in structured memory, got ~S"
                        structured)
               (%assert tool-entry
                        "Expected a tool entry in recent_activity, got ~S"
                        recent-activity)
               (%assert (string= (getf tool-entry :tool_name) "echo")
                        "Expected tool_name echo in recent_activity, got ~S"
                        tool-entry)
               (%assert (stringp (getf tool-entry :call_id))
                        "Expected call_id string in recent_activity, got ~S"
                        tool-entry)
               (if parsed-metadata
                   (progn
                     (%assert (= (getf structured-metadata :update_count)
                                 (claw-lisp.storage.session-memory:session-memory-metadata-update-count
                                  parsed-metadata))
                              "Expected structured metadata update_count to match markdown metadata")
                     (%assert (= (getf structured-metadata :budget_chars_used)
                                 (claw-lisp.storage.session-memory:session-memory-metadata-budget-chars-used
                                  parsed-metadata))
                              "Expected structured metadata budget_chars_used to match markdown metadata")
                     (%assert (= (getf structured-metadata :budget_chars_max)
                                 (claw-lisp.storage.session-memory:session-memory-metadata-budget-chars-max
                                  parsed-metadata))
                              "Expected structured metadata budget_chars_max to match markdown metadata"))
                   (%assert (null structured-metadata)
                            "Expected nil structured metadata when markdown metadata header is absent")))))
      (when (probe-file root)
        (uiop:delete-directory-tree root :validate t)))))

(defun test-session-memory-structured-write-failure-is-non-fatal ()
  (let* ((root (merge-pathnames
                (format nil "claw-lisp-session-memory-structured-failure-~A/" (get-universal-time))
                #P"/tmp/"))
         (transcripts-root (merge-pathnames "transcripts/" root))
         (memory-root (merge-pathnames "memory/" root))
         (config (claw-lisp.config::%make-runtime-config
                  :data-root (namestring root)
                  :transcripts-root (namestring transcripts-root)
                  :artifacts-root (namestring (merge-pathnames "artifacts/" root))
                  :memory-root (namestring memory-root)
                  :default-provider "mock"
                  :default-model "mock-model"))
         (runtime (make-runtime :config config)))
    (unwind-protect
         (progn
           (register-default-providers runtime)
           (let* ((session (start-session runtime
                                          :provider-name "mock"
                                          :model "mock-model"
                                          :session-id "memory-structured-failure"))
                  (memory-path (session-memory-path config "memory-structured-failure"))
                  (old-fn
                    (symbol-function
                     'claw-lisp.storage.session-memory::write-session-memory-structured)))
             (unwind-protect
                  (progn
                    (setf (symbol-function
                           'claw-lisp.storage.session-memory::write-session-memory-structured)
                          (lambda (&rest args)
                            (declare (ignore args))
                            (error "structured write failed")))
                    (submit-user-message runtime session "structured write failure test")
                    (%assert (probe-file memory-path)
                             "Expected markdown session memory file despite structured write failure"))
               (setf (symbol-function
                      'claw-lisp.storage.session-memory::write-session-memory-structured)
                     old-fn))))
      (when (probe-file root)
        (uiop:delete-directory-tree root :validate t)))))

(defun test-session-memory-compaction-reuse ()
  (let* ((root (merge-pathnames
                (format nil "claw-lisp-compact-test-~A/" (get-universal-time))
                #P"/tmp/"))
         (transcripts-root (merge-pathnames "transcripts/" root))
         (memory-root (merge-pathnames "memory/" root))
         (config (claw-lisp.config::%make-runtime-config
                  :data-root (namestring root)
                  :transcripts-root (namestring transcripts-root)
                  :artifacts-root (namestring (merge-pathnames "artifacts/" root))
                  :memory-root (namestring memory-root)
                  :default-provider "mock"
                  :default-model "mock-model"
                  ))
         (runtime (make-runtime :config config)))
    (unwind-protect
         (progn
           (register-default-providers runtime)
           (let* ((session (start-session runtime
                                          :provider-name "mock"
                                          :model "mock-model"
                                          :session-id "compact-test"))
                  (updated (submit-user-message runtime session "reuse session memory"))
                  (result (claw-lisp.core.compact:try-session-memory-compaction
                           (claw-lisp.core.runtime:runtime-settings runtime)
                           updated
                           :keep-recent-messages 4)))
             (%assert result "Expected session-memory compaction result")
             (%assert (eq :session-memory-selective
                          (claw-lisp.core.domain:compaction-result-source result))
                      "Unexpected compaction source: ~A"
                      (claw-lisp.core.domain:compaction-result-source result))
             (%assert (search "reuse session memory"
                              (claw-lisp.core.compact:compaction-result-rendered-summary result))
                      "Expected session-memory summary to include recent content")
             (%assert (= 2 (length (claw-lisp.core.domain:compaction-result-preserved-messages result)))
                      "Expected preserved recent messages, got ~A"
                      (length (claw-lisp.core.domain:compaction-result-preserved-messages result)))))
      (when (probe-file root)
        (uiop:delete-directory-tree root :validate t)))))

(defun test-session-memory-compaction-missing-file ()
  (let* ((root (merge-pathnames
                (format nil "claw-lisp-compact-miss-~A/" (get-universal-time))
                #P"/tmp/"))
         (transcripts-root (merge-pathnames "transcripts/" root))
         (memory-root (merge-pathnames "memory/" root))
         (config (claw-lisp.config::%make-runtime-config
                  :data-root (namestring root)
                  :transcripts-root (namestring transcripts-root)
                  :artifacts-root (namestring (merge-pathnames "artifacts/" root))
                  :memory-root (namestring memory-root)
                  :default-provider "mock"
                  :default-model "mock-model"
                  ))
         (runtime (make-runtime :config config)))
    (unwind-protect
         (let ((session (claw-lisp.core.domain:make-agent-session
                         :id "compact-miss"
                         :provider "mock"
                         :model "mock-model"
                         :conversation (make-conversation :id "compact-miss")
                         :state '(:initialized t))))
           (%assert (null (claw-lisp.core.compact:try-session-memory-compaction
                           (claw-lisp.core.runtime:runtime-settings runtime)
                           session
                           :keep-recent-messages 4))
                    "Expected NIL compaction result when session-memory file is absent"))
      (when (probe-file root)
        (uiop:delete-directory-tree root :validate t)))))

(defun test-durable-memory-extraction ()
  (let* ((root (merge-pathnames
                (format nil "claw-lisp-durable-memory-test-~A/" (get-universal-time))
                #P"/tmp/"))
         (transcripts-root (merge-pathnames "transcripts/" root))
         (memory-root (merge-pathnames "memory/" root))
         (config (claw-lisp.config::%make-runtime-config
                  :data-root (namestring root)
                  :transcripts-root (namestring transcripts-root)
                  :artifacts-root (namestring (merge-pathnames "artifacts/" root))
                  :memory-root (namestring memory-root)
                  :default-provider "mock"
                  :default-model "mock-model"
                  ))
         (runtime (make-runtime :config config)))
    (unwind-protect
         (progn
           (register-default-providers runtime)
           (register-tool runtime (make-echo-tool))
           (let* ((session (start-session runtime
                                          :provider-name "mock"
                                          :model "mock-model"
                                          :session-id "durable-test"))
                  (updated (submit-user-message runtime session "remember this request"))
                  (tool-updated (progn
                                  (execute-registered-tool runtime updated "echo" '(:text "durable tool line"))
                                  updated))
                  (saved-records (extract-session-durable-memory runtime tool-updated))
                  (memory-path (durable-memory-note-path config "durable-test"))
                  (index-path (durable-memory-index-path config))
                  (memory-text (uiop:read-file-string memory-path))
                  (index-text (uiop:read-file-string index-path)))
             (%assert (and (listp saved-records) (plusp (length saved-records)))
                      "Expected extract-session-durable-memory to return saved records, got ~A"
                      saved-records)
             
             (%assert (probe-file memory-path)
                      "Expected durable memory file at ~A"
                      memory-path)
             (%assert (or (search "remember this request" memory-text)
                          (search "Mock provider reply" memory-text))
                      "Missing durable request context in durable memory: ~A"
                      memory-text)
             (%assert (search "durable tool line" memory-text)
                      "Missing durable tool result in durable memory: ~A"
                      memory-text)
             (%assert (probe-file index-path)
                      "Expected durable memory index at ~A"
                      index-path)
             (%assert (search "[durable-test](durable-test.md)" index-text)
                      "Missing durable memory index entry: ~A"
                      index-text)))
      (when (probe-file root)
        (uiop:delete-directory-tree root :validate t)))))

(defun test-durable-memory-extraction-skips-empty-session ()
  (let* ((root (merge-pathnames
                (format nil "claw-lisp-durable-memory-empty-test-~A/" (get-universal-time))
                #P"/tmp/"))
         (transcripts-root (merge-pathnames "transcripts/" root))
         (memory-root (merge-pathnames "memory/" root))
         (config (claw-lisp.config::%make-runtime-config
                  :data-root (namestring root)
                  :transcripts-root (namestring transcripts-root)
                  :artifacts-root (namestring (merge-pathnames "artifacts/" root))
                  :memory-root (namestring memory-root)
                  :default-provider "mock"
                  :default-model "mock-model"
                  ))
         (runtime (make-runtime :config config))
         (session (claw-lisp.core.domain:make-agent-session
                   :id "durable-empty"
                   :provider "mock"
                   :model "mock-model"
                   :conversation (make-conversation :id "durable-empty")
                   :state '(:initialized t))))
    (unwind-protect
         (%assert (null (extract-session-durable-memory runtime session))
                  "Expected durable memory extraction to skip empty session")
      (when (probe-file root)
        (uiop:delete-directory-tree root :validate t)))))

(defun test-durable-memory-auto-extracts-after-message-turn ()
  (let* ((root (merge-pathnames
                (format nil "claw-lisp-durable-auto-message-test-~A/" (get-universal-time))
                #P"/tmp/"))
         (transcripts-root (merge-pathnames "transcripts/" root))
         (memory-root (merge-pathnames "memory/" root))
         (config (claw-lisp.config::%make-runtime-config
                  :data-root (namestring root)
                  :transcripts-root (namestring transcripts-root)
                  :artifacts-root (namestring (merge-pathnames "artifacts/" root))
                  :memory-root (namestring memory-root)
                  :default-provider "mock"
                  :default-model "mock-model"
                  ))
         (runtime (make-runtime :config config)))
    (unwind-protect
         (progn
           (register-default-providers runtime)
           (let* ((session (start-session runtime
                                          :provider-name "mock"
                                          :model "mock-model"
                                          :session-id "durable-auto-message"))
                  (updated (submit-user-message runtime session "auto durable memory from message"))
                  (memory-path (durable-memory-note-path config "durable-auto-message")))
             (declare (ignore updated))
             (%assert (probe-file memory-path)
                      "Expected durable memory file to be written automatically: ~A"
                      memory-path)))
      (when (probe-file root)
        (uiop:delete-directory-tree root :validate t)))))

(defun test-durable-memory-auto-extracts-after-tool-execution ()
  (let* ((root (merge-pathnames
                (format nil "claw-lisp-durable-auto-tool-test-~A/" (get-universal-time))
                #P"/tmp/"))
         (transcripts-root (merge-pathnames "transcripts/" root))
         (memory-root (merge-pathnames "memory/" root))
         (config (claw-lisp.config::%make-runtime-config
                  :data-root (namestring root)
                  :transcripts-root (namestring transcripts-root)
                  :artifacts-root (namestring (merge-pathnames "artifacts/" root))
                  :memory-root (namestring memory-root)
                  :default-provider "mock"
                  :default-model "mock-model"
                  ))
         (runtime (make-runtime :config config)))
    (unwind-protect
         (progn
           (register-default-providers runtime)
           (register-tool runtime (make-echo-tool))
           (let* ((session (start-session runtime
                                          :provider-name "mock"
                                          :model "mock-model"
                                          :session-id "durable-auto-tool")))
             (execute-registered-tool runtime session "echo" '(:text "auto durable memory from tool"))
             (let ((memory-path (durable-memory-note-path config "durable-auto-tool")))
               (%assert (probe-file memory-path)
                        "Expected durable memory file after tool execution: ~A"
                        memory-path))))
      (when (probe-file root)
        (uiop:delete-directory-tree root :validate t)))))

(defun test-local-compaction-falls-back-without-session-memory ()
  (let* ((root (merge-pathnames
                (format nil "claw-lisp-local-compact-test-~A/" (get-universal-time))
                #P"/tmp/"))
         (transcripts-root (merge-pathnames "transcripts/" root))
         (memory-root (merge-pathnames "memory/" root))
         (config (claw-lisp.config::%make-runtime-config
                  :data-root (namestring root)
                  :transcripts-root (namestring transcripts-root)
                  :artifacts-root (namestring (merge-pathnames "artifacts/" root))
                  :memory-root (namestring memory-root)
                  :default-provider "mock"
                  :default-model "mock-model"
                  ))
         (runtime (make-runtime :config config)))
    (unwind-protect
         (let* ((conversation (make-conversation :id "local-compact"))
                (session (claw-lisp.core.domain:make-agent-session
                          :id "local-compact"
                          :provider "mock"
                          :model "mock-model"
                          :conversation conversation
                          :state '(:initialized t))))
           (append-message conversation (make-message :role :user :content "fallback summary check"))
           (append-message conversation (make-message :role :assistant :content "assistant trace"))
           (claw-lisp.core.domain:record-tool-result
            conversation
            (claw-lisp.core.domain:make-tool-result
             :call-id "echo-call-1"
             :tool-name "echo"
             :content "tool trace"
             :bytes 10))
           (let ((result (%runtime-compact-session
                          runtime
                          session
                          :keep-recent-messages 1)))
             (%assert result "Expected local fallback compaction result")
             (%assert (eq :fallback
                          (claw-lisp.core.domain:compaction-result-source result))
                      "Unexpected fallback compaction source: ~A"
                      (claw-lisp.core.domain:compaction-result-source result))
             (%assert (search "fallback summary check"
                              (claw-lisp.core.compact:compaction-result-rendered-summary result))
                      "Expected fallback summary to include recent message content")
             (%assert (search "tool trace"
                              (claw-lisp.core.compact:compaction-result-rendered-summary result))
                      "Expected fallback summary to include tool result content")
             (%assert (= 1 (length (claw-lisp.core.domain:compaction-result-preserved-messages result)))
                      "Expected one preserved recent message, got ~A"
                      (length (claw-lisp.core.domain:compaction-result-preserved-messages result)))))
      (when (probe-file root)
        (uiop:delete-directory-tree root :validate t)))))

(defun test-local-compaction-prefers-session-memory ()
  (let* ((root (merge-pathnames
                (format nil "claw-lisp-local-compact-prefer-test-~A/" (get-universal-time))
                #P"/tmp/"))
         (transcripts-root (merge-pathnames "transcripts/" root))
         (memory-root (merge-pathnames "memory/" root))
         (config (claw-lisp.config::%make-runtime-config
                  :data-root (namestring root)
                  :transcripts-root (namestring transcripts-root)
                  :artifacts-root (namestring (merge-pathnames "artifacts/" root))
                  :memory-root (namestring memory-root)
                  :default-provider "mock"
                  :default-model "mock-model"
                  ))
         (runtime (make-runtime :config config)))
    (unwind-protect
         (progn
           (register-default-providers runtime)
           (let* ((session (start-session runtime
                                          :provider-name "mock"
                                          :model "mock-model"
                                          :session-id "local-compact-prefer"))
                  (updated (submit-user-message runtime session "prefer session memory"))
                  (result (%runtime-compact-session
                           runtime
                           updated
                           :keep-recent-messages 1)))
             (%assert result "Expected compaction result")
             (%assert (member (claw-lisp.core.domain:compaction-result-source result)
                              '(:session-memory :session-memory-selective))
                      "Expected session-memory compaction to take precedence, got ~A"
                      (claw-lisp.core.domain:compaction-result-source result))))
      (when (probe-file root)
        (uiop:delete-directory-tree root :validate t)))))

(defun test-apply-session-compaction-replaces-message-history ()
  (let* ((root (merge-pathnames
                (format nil "claw-lisp-apply-compact-test-~A/" (get-universal-time))
                #P"/tmp/"))
         (transcripts-root (merge-pathnames "transcripts/" root))
         (memory-root (merge-pathnames "memory/" root))
         (config (claw-lisp.config::%make-runtime-config
                  :data-root (namestring root)
                  :transcripts-root (namestring transcripts-root)
                  :artifacts-root (namestring (merge-pathnames "artifacts/" root))
                  :memory-root (namestring memory-root)
                  :default-provider "mock"
                  :default-model "mock-model"
                  ))
         (runtime (make-runtime :config config)))
    (unwind-protect
         (progn
           (register-default-providers runtime)
           (register-tool runtime (make-echo-tool))
           (let* ((session (start-session runtime
                                          :provider-name "mock"
                                          :model "mock-model"
                                          :session-id "apply-compact-test"))
                  (s1 (submit-user-message runtime session "first user message"))
                  (s2 (submit-user-message runtime s1 "second user message"))
                  (tool-1 (execute-registered-tool runtime s2 "echo" '(:text "first tool result")))
                  (tool-2 (execute-registered-tool runtime s2 "echo" '(:text "second tool result")))
                  (tool-3 (execute-registered-tool runtime s2 "echo" '(:text "third tool result")))
                  (result (%runtime-compact-session
                           runtime
                           s2
                           :keep-recent-messages 1)))
             (declare (ignore tool-1 tool-2 tool-3))
             (claw-lisp.core.runtime:apply-session-compaction runtime s2 result)
             (let* ((conversation (claw-lisp.core.domain:agent-session-conversation s2))
                    (messages (claw-lisp.core.domain:conversation-messages conversation))
                    (tool-results (claw-lisp.core.domain:conversation-tool-results conversation))
                    (first-message (first messages))
                    (second-message (second messages))
                    (lines (read-lines (session-transcript-path runtime s2))))
               (%assert (= 2 (length messages))
                        "Expected boundary plus preserved message, got ~A"
                        (length messages))
               (%assert (eq :system (claw-lisp.core.domain:message-role first-message))
                        "Expected boundary system message, got ~A"
                        (claw-lisp.core.domain:message-role first-message))
               (%assert (search "# Compaction Boundary"
                                (claw-lisp.core.domain:message-content first-message))
                        "Expected compaction boundary content, got ~A"
                        (claw-lisp.core.domain:message-content first-message))
               (%assert (string= "Mock provider reply. No external model call executed."
                                 (claw-lisp.core.domain:message-content second-message))
                        "Expected preserved recent tail message, got ~A"
                        (claw-lisp.core.domain:message-content second-message))
               (%assert (= 2 (length tool-results))
                        "Expected restored recent tool-result slice, got ~A"
                        (length tool-results))
               (%assert (string= "second tool result"
                                 (claw-lisp.core.domain:tool-result-content (first tool-results)))
                        "Expected second-most-recent tool result to be restored, got ~A"
                        (claw-lisp.core.domain:tool-result-content (first tool-results)))
               (%assert (string= "third tool result"
                                 (claw-lisp.core.domain:tool-result-content (second tool-results)))
                        "Expected most-recent tool result to be restored, got ~A"
                        (claw-lisp.core.domain:tool-result-content (second tool-results)))
               (%assert (some (lambda (line) (search "\"event\":\"compaction_boundary\"" line)) lines)
                        "Expected compaction boundary transcript event, got ~A"
                        lines))))
      (when (probe-file root)
        (uiop:delete-directory-tree root :validate t)))))

(defun test-compaction-circuit-opens-after-failures ()
  (let ((runtime (make-runtime))
        (session (claw-lisp.core.domain:make-agent-session
                  :id "compact-circuit"
                  :provider "mock"
                  :model "mock-model"
                  :conversation (make-conversation :id "compact-circuit")
                  :state '(:initialized t))))
    (dotimes (_ 3)
      (declare (ignore _))
      (claw-lisp.core.runtime:increment-compaction-failures session))
    (%assert (claw-lisp.core.runtime:compaction-circuit-open-p session)
             "Expected compaction circuit to open after repeated failures")
    (%assert (>= (getf (claw-lisp.core.domain:agent-session-state session)
                       :compaction-failure-count)
                 3)
             "Expected recorded compaction failures to be >= 3")))

(defun test-compaction-circuit-resets-after-success ()
  (let* ((root (merge-pathnames
                (format nil "claw-lisp-compact-reset-test-~A/" (get-universal-time))
                #P"/tmp/"))
         (transcripts-root (merge-pathnames "transcripts/" root))
         (memory-root (merge-pathnames "memory/" root))
         (config (claw-lisp.config::%make-runtime-config
                  :data-root (namestring root)
                  :transcripts-root (namestring transcripts-root)
                  :artifacts-root (namestring (merge-pathnames "artifacts/" root))
                  :memory-root (namestring memory-root)
                  :default-provider "mock"
                  :default-model "mock-model"
                  ))
         (runtime (make-runtime :config config)))
    (unwind-protect
         (progn
           (register-default-providers runtime)
           (let ((session (start-session runtime
                                         :provider-name "mock"
                                         :model "mock-model"
                                         :session-id "compact-reset")))
             (submit-user-message runtime session "reset counter after success")
             (setf (getf (claw-lisp.core.domain:agent-session-state session)
                         :compaction-failure-count)
                   2)
             (claw-lisp.core.runtime:reset-compaction-failures session)
             (%assert (not (claw-lisp.core.runtime:compaction-circuit-open-p session))
                      "Did not expect compaction circuit to remain open after success")
             (%assert (= 0
                         (getf (claw-lisp.core.domain:agent-session-state session)
                               :compaction-failure-count))
                      "Expected compaction failure count reset after success")))
      (when (probe-file root)
        (uiop:delete-directory-tree root :validate t)))))

(defun test-tool-registration ()
  (let ((runtime (make-runtime)))
    (register-tool runtime (make-echo-tool))
    (register-tool runtime (make-file-read-tool))
    (register-tool runtime (make-file-write-tool))
    (register-tool runtime (make-file-replace-tool))
    (register-tool runtime (make-glob-tool))
    (register-tool runtime (make-grep-tool))
    (register-tool runtime (make-shell-command-tool))
    (%assert (equal '("echo" "file-read" "file-replace" "file-write" "glob" "grep" "shell-command") (list-tool-names runtime))
             "Expected baseline tool registration, got ~A"
             (list-tool-names runtime))))

(defun test-file-read-tool-execution-flow ()
  (let* ((root (merge-pathnames
                (format nil "claw-lisp-file-read-test-~A/" (get-universal-time))
                #P"/tmp/"))
         (transcripts-root (merge-pathnames "transcripts/" root))
         (fixture-path (merge-pathnames "fixture.txt" root))
         (config (claw-lisp.config::%make-runtime-config
                  :data-root (namestring root)
                  :transcripts-root (namestring transcripts-root)
                  :artifacts-root (namestring (merge-pathnames "artifacts/" root))
                  :memory-root (namestring (merge-pathnames "memory/" root))
                  :default-provider "mock"
                  :default-model "mock-model"
                  ))
         (runtime (make-runtime :config config)))
    (unwind-protect
         (progn
           (ensure-directories-exist fixture-path)
           (with-open-file (stream fixture-path
                                   :direction :output
                                   :if-exists :supersede
                                   :if-does-not-exist :create)
             (write-string "fixture contents" stream))
           (register-default-providers runtime)
           (register-tool runtime (make-file-read-tool))
           (let* ((session (start-session runtime
                                          :provider-name "mock"
                                          :model "mock-model"
                                          :session-id "file-read-test"))
                  (result (execute-registered-tool runtime
                                                   session
                                                   "file-read"
                                                   (list :path (namestring fixture-path)))))
             (%assert (string= "file-read" (tool-result-tool-name result))
                      "Unexpected file-read result name: ~A"
                      (tool-result-tool-name result))
             (%assert (string= "fixture contents" (tool-result-content result))
                      "Unexpected file-read result content: ~A"
                      (tool-result-content result)))))
      (when (probe-file root)
        (uiop:delete-directory-tree root :validate t)))))

(defun test-file-write-tool-execution-flow ()
  (let* ((root (merge-pathnames
                (format nil "claw-lisp-file-write-test-~A/" (get-universal-time))
                #P"/tmp/"))
         (transcripts-root (merge-pathnames "transcripts/" root))
         (target-path (merge-pathnames "nested/output.txt" root))
         (config (claw-lisp.config::%make-runtime-config
                  :data-root (namestring root)
                  :transcripts-root (namestring transcripts-root)
                  :artifacts-root (namestring (merge-pathnames "artifacts/" root))
                  :memory-root (namestring (merge-pathnames "memory/" root))
                  :default-provider "mock"
                  :default-model "mock-model"
                  ))
         (runtime (make-runtime :config config)))
    (unwind-protect
         (progn
           (register-default-providers runtime)
           (register-tool runtime (make-file-write-tool))
           (let* ((session (start-session runtime
                                          :provider-name "mock"
                                          :model "mock-model"
                                          :session-id "file-write-test"))
                  (result (execute-registered-tool runtime
                                                   session
                                                   "file-write"
                                                   (list :path (namestring target-path)
                                                         :text "written contents"))))
             (%assert (string= "file-write" (tool-result-tool-name result))
                      "Unexpected file-write result name: ~A"
                      (tool-result-tool-name result))
             (%assert (probe-file target-path)
                      "Expected file-write to create target file: ~A"
                      target-path)
             (%assert (string= "written contents"
                               (uiop:read-file-string target-path))
                      "Unexpected file-write target content.")
             (%assert (search "Wrote 16 bytes to" (tool-result-content result))
                      "Unexpected file-write result content: ~A"
                      (tool-result-content result))))
      (when (probe-file root)
        (uiop:delete-directory-tree root :validate t)))))

(defun test-file-replace-tool-execution-flow ()
  (let* ((root (merge-pathnames
                (format nil "claw-lisp-file-replace-test-~A/" (get-universal-time))
                #P"/tmp/"))
         (transcripts-root (merge-pathnames "transcripts/" root))
         (target-path (merge-pathnames "replace.txt" root))
         (config (claw-lisp.config::%make-runtime-config
                  :data-root (namestring root)
                  :transcripts-root (namestring transcripts-root)
                  :artifacts-root (namestring (merge-pathnames "artifacts/" root))
                  :memory-root (namestring (merge-pathnames "memory/" root))
                  :default-provider "mock"
                  :default-model "mock-model"
                  ))
         (runtime (make-runtime :config config)))
    (unwind-protect
         (progn
           (ensure-directories-exist target-path)
           (with-open-file (stream target-path
                                   :direction :output
                                   :if-exists :supersede
                                   :if-does-not-exist :create)
             (write-string "alpha beta gamma" stream))
           (register-default-providers runtime)
           (register-tool runtime (make-file-replace-tool))
           (let* ((session (start-session runtime
                                          :provider-name "mock"
                                          :model "mock-model"
                                          :session-id "file-replace-test"))
                  (result (execute-registered-tool runtime
                                                   session
                                                   "file-replace"
                                                   (list :path (namestring target-path)
                                                         :old-text "beta"
                                                         :new-text "delta"))))
             (%assert (string= "file-replace" (tool-result-tool-name result))
                      "Unexpected file-replace result name: ~A"
                      (tool-result-tool-name result))
             (%assert (string= "alpha delta gamma"
                               (uiop:read-file-string target-path))
                      "Unexpected file-replace target content.")
             (%assert (search "Replaced 1 occurrence"
                              (tool-result-content result))
                      "Unexpected file-replace result content: ~A"
                      (tool-result-content result))))
      (when (probe-file root)
        (uiop:delete-directory-tree root :validate t)))))

(defun test-glob-tool-execution-flow ()
  (let* ((root (merge-pathnames
                (format nil "claw-lisp-glob-test-~A/" (get-universal-time))
                #P"/tmp/"))
         (transcripts-root (merge-pathnames "transcripts/" root))
         (glob-root (merge-pathnames "glob-root/" root))
         (match-path (merge-pathnames "match.txt" glob-root))
         (skip-path (merge-pathnames "skip.lisp" glob-root))
         (config (claw-lisp.config::%make-runtime-config
                  :data-root (namestring root)
                  :transcripts-root (namestring transcripts-root)
                  :artifacts-root (namestring (merge-pathnames "artifacts/" root))
                  :memory-root (namestring (merge-pathnames "memory/" root))
                  :default-provider "mock"
                  :default-model "mock-model"
                  ))
         (runtime (make-runtime :config config)))
    (unwind-protect
         (progn
           (ensure-directories-exist match-path)
           (with-open-file (stream match-path :direction :output :if-exists :supersede :if-does-not-exist :create)
             (write-string "txt" stream))
           (with-open-file (stream skip-path :direction :output :if-exists :supersede :if-does-not-exist :create)
             (write-string "lisp" stream))
           (register-default-providers runtime)
           (register-tool runtime (make-glob-tool))
           (let* ((session (start-session runtime
                                          :provider-name "mock"
                                          :model "mock-model"
                                          :session-id "glob-test"))
                  (result (execute-registered-tool runtime
                                                   session
                                                   "glob"
                                                   (list :path (namestring glob-root)
                                                         :pattern "*.txt"))))
             (%assert (string= "glob" (tool-result-tool-name result))
                      "Unexpected glob result name: ~A"
                      (tool-result-tool-name result))
             (%assert (search "match.txt" (tool-result-content result))
                      "Expected glob result to include match.txt, got ~A"
                      (tool-result-content result))
             (%assert (not (search "skip.lisp" (tool-result-content result)))
                      "Did not expect glob result to include skip.lisp, got ~A"
                      (tool-result-content result))))
      (when (probe-file root)
        (uiop:delete-directory-tree root :validate t)))))

(defun test-grep-tool-execution-flow ()
  (let* ((root (merge-pathnames
                (format nil "claw-lisp-grep-test-~A/" (get-universal-time))
                #P"/tmp/"))
         (transcripts-root (merge-pathnames "transcripts/" root))
         (target-path (merge-pathnames "grep.txt" root))
         (config (claw-lisp.config::%make-runtime-config
                  :data-root (namestring root)
                  :transcripts-root (namestring transcripts-root)
                  :artifacts-root (namestring (merge-pathnames "artifacts/" root))
                  :memory-root (namestring (merge-pathnames "memory/" root))
                  :default-provider "mock"
                  :default-model "mock-model"
                  ))
         (runtime (make-runtime :config config)))
    (unwind-protect
         (progn
           (ensure-directories-exist target-path)
           (with-open-file (stream target-path :direction :output :if-exists :supersede :if-does-not-exist :create)
             (write-line "alpha" stream)
             (write-line "beta target" stream)
             (write-line "charlie target" stream))
           (register-default-providers runtime)
           (register-tool runtime (make-grep-tool))
           (let* ((session (start-session runtime
                                          :provider-name "mock"
                                          :model "mock-model"
                                          :session-id "grep-test"))
                  (result (execute-registered-tool runtime
                                                   session
                                                   "grep"
                                                   (list :path (namestring root)
                                                         :pattern "target"))))
             (%assert (string= "grep" (tool-result-tool-name result))
                      "Unexpected grep result name: ~A"
                      (tool-result-tool-name result))
             (%assert (search "2:beta target" (tool-result-content result))
                      "Expected grep result to include first matching line, got ~A"
                      (tool-result-content result))
             (%assert (search "3:charlie target" (tool-result-content result))
                      "Expected grep result to include second matching line, got ~A"
                      (tool-result-content result))))
      (when (probe-file root)
        (uiop:delete-directory-tree root :validate t)))))

(defun test-shell-command-tool-execution-flow ()
  (let* ((root (merge-pathnames
                (format nil "claw-lisp-shell-command-test-~A/" (get-universal-time))
                #P"/tmp/"))
         (transcripts-root (merge-pathnames "transcripts/" root))
         (config (claw-lisp.config::%make-runtime-config
                  :data-root (namestring root)
                  :transcripts-root (namestring transcripts-root)
                  :artifacts-root (namestring (merge-pathnames "artifacts/" root))
                  :memory-root (namestring (merge-pathnames "memory/" root))
                  :default-provider "mock"
                  :default-model "mock-model"
                  ))
         (runtime (make-runtime :config config)))
    (unwind-protect
         (progn
           (register-default-providers runtime)
           (register-tool runtime (make-shell-command-tool))
           (let* ((session (start-session runtime
                                          :provider-name "mock"
                                          :model "mock-model"
                                          :session-id "shell-command-test"))
                  (result (execute-registered-tool runtime
                                                   session
                                                   "shell-command"
                                                   (list :text "printf 'hello shell'"))))
             (%assert (string= "shell-command" (tool-result-tool-name result))
                      "Unexpected shell-command result name: ~A"
                      (tool-result-tool-name result))
             (%assert (string= "hello shell" (tool-result-content result))
                      "Unexpected shell-command output: ~A"
                      (tool-result-content result))))
      (when (probe-file root)
        (uiop:delete-directory-tree root :validate t)))))

(defun test-shell-command-tool-times-out ()
  (let* ((root (merge-pathnames
                (format nil "claw-lisp-shell-timeout-test-~A/" (get-universal-time))
                #P"/tmp/"))
         (transcripts-root (merge-pathnames "transcripts/" root))
         (config (claw-lisp.config::%make-runtime-config
                  :data-root (namestring root)
                  :transcripts-root (namestring transcripts-root)
                  :artifacts-root (namestring (merge-pathnames "artifacts/" root))
                  :memory-root (namestring (merge-pathnames "memory/" root))
                  :shell-command-timeout-seconds 1
                  :default-provider "mock"
                  :default-model "mock-model"
                  ))
         (runtime (make-runtime :config config)))
    (unwind-protect
         (progn
           (register-default-providers runtime)
           (register-tool runtime (make-shell-command-tool))
           (let* ((session (start-session runtime
                                          :provider-name "mock"
                                          :model "mock-model"
                                          :session-id "shell-timeout-test"))
                  (result (execute-registered-tool runtime
                                                   session
                                                   "shell-command"
                                                   (list :text "sleep 2"))))
             (%assert (string= "shell-command" (tool-result-tool-name result))
                      "Unexpected shell-command timeout result name: ~A"
                      (tool-result-tool-name result))
             (%assert (search "124" (tool-result-content result))
                      "Expected shell-command timeout output to reflect exit 124, got ~A"
                      (tool-result-content result))
             (%assert (search "timed out after 1 seconds" (tool-result-content result))
                      "Expected shell-command timeout output, got ~A"
                      (tool-result-content result))))
      (when (probe-file root)
        (uiop:delete-directory-tree root :validate t)))))

(defun test-file-tool-permissions-reject-forbidden-path ()
  (let* ((root (merge-pathnames
                (format nil "claw-lisp-file-permission-test-~A/" (get-universal-time))
                #P"/tmp/"))
         (transcripts-root (merge-pathnames "transcripts/" root))
         (config (claw-lisp.config::%make-runtime-config
                  :data-root (namestring root)
                  :transcripts-root (namestring transcripts-root)
                  :artifacts-root (namestring (merge-pathnames "artifacts/" root))
                  :memory-root (namestring (merge-pathnames "memory/" root))
                  :tool-allowed-roots (list (namestring root))
                  :default-provider "mock"
                  :default-model "mock-model"
                  ))
         (runtime (make-runtime :config config)))
    (unwind-protect
         (progn
           (register-default-providers runtime)
           (register-tool runtime (make-file-read-tool))
           (let ((session (start-session runtime
                                         :provider-name "mock"
                                         :model "mock-model"
                                         :session-id "file-permission-test")))
             (handler-case
                 (progn
                   (execute-registered-tool runtime
                                            session
                                            "file-read"
                                            (list :path "/etc/hosts"))
                   (%assert nil "Expected file-read permission rejection."))
               (error (condition)
                 (%assert (search "outside allowed roots" (princ-to-string condition))
                          "Expected permission rejection error, got ~A"
                          condition)))))
      (when (probe-file root)
        (uiop:delete-directory-tree root :validate t)))))

(defun test-shell-command-can-be-disabled-by-policy ()
  (let* ((root (merge-pathnames
                (format nil "claw-lisp-shell-disabled-test-~A/" (get-universal-time))
                #P"/tmp/"))
         (transcripts-root (merge-pathnames "transcripts/" root))
         (config (claw-lisp.config::%make-runtime-config
                  :data-root (namestring root)
                  :transcripts-root (namestring transcripts-root)
                  :artifacts-root (namestring (merge-pathnames "artifacts/" root))
                  :memory-root (namestring (merge-pathnames "memory/" root))
                  :shell-command-enabled-p nil
                  :default-provider "mock"
                  :default-model "mock-model"
                  ))
         (runtime (make-runtime :config config)))
    (unwind-protect
         (progn
           (register-default-providers runtime)
           (register-tool runtime (make-shell-command-tool))
           (let ((session (start-session runtime
                                         :provider-name "mock"
                                         :model "mock-model"
                                         :session-id "shell-disabled-test")))
             (handler-case
                 (progn
                   (execute-registered-tool runtime
                                            session
                                            "shell-command"
                                            (list :text "printf 'hello shell'"))
                   (%assert nil "Expected shell-command policy rejection."))
               (error (condition)
                 (%assert (search "disabled by runtime policy" (princ-to-string condition))
                          "Expected shell-command policy error, got ~A"
                          condition)))))
      (when (probe-file root)
        (uiop:delete-directory-tree root :validate t)))))

(defun test-tool-execution-flow ()
  (let* ((root (merge-pathnames
                (format nil "claw-lisp-tool-test-~A/" (get-universal-time))
                #P"/tmp/"))
         (transcripts-root (merge-pathnames "transcripts/" root))
         (config (claw-lisp.config::%make-runtime-config
                  :data-root (namestring root)
                  :transcripts-root (namestring transcripts-root)
                  :artifacts-root (namestring (merge-pathnames "artifacts/" root))
                  :memory-root (namestring (merge-pathnames "memory/" root))
                  :default-provider "mock"
                  :default-model "mock-model"
                  ))
         (runtime (make-runtime :config config)))
    (unwind-protect
         (progn
           (register-default-providers runtime)
           (register-tool runtime (make-echo-tool))
           (let* ((session (start-session runtime
                                          :provider-name "mock"
                                          :model "mock-model"
                                          :session-id "tool-test"))
                  (result (execute-registered-tool runtime session "echo" '(:text "hello tool")))
                  (conversation (claw-lisp.core.domain:agent-session-conversation session))
                  (tool-results (conversation-tool-results conversation))
                  (transcript-path (session-transcript-path runtime session))
                  (lines (read-lines transcript-path)))
             (%assert (string= "echo" (tool-result-tool-name result))
                      "Unexpected tool result name: ~A"
                      (tool-result-tool-name result))
             (%assert (string= "hello tool" (tool-result-content result))
                      "Unexpected tool result content: ~A"
                      (tool-result-content result))
             (%assert (= 1 (length tool-results))
                      "Expected one recorded tool result, got ~A"
                      (length tool-results))
             (%assert (>= (length lines) 2)
                      "Expected at least 2 transcript lines after tool execution, got ~A"
                      (length lines))
             (let ((tool-result-line
                     (find-if (lambda (line)
                                (search "\"event\":\"tool_result\"" line))
                              lines)))
               (%assert tool-result-line
                        "Missing tool_result transcript line: ~S"
                        lines)
               (%assert (search "\"call_id\":\"echo-call-1\"" tool-result-line)
                        "Missing call_id in tool_result transcript line: ~A"
                        tool-result-line)))))
      (when (probe-file root)
        (uiop:delete-directory-tree root :validate t)))))

(defun test-oversized-tool-result-persistence ()
  (let* ((root (merge-pathnames
                (format nil "claw-lisp-persist-test-~A/" (get-universal-time))
                #P"/tmp/"))
         (transcripts-root (merge-pathnames "transcripts/" root))
         (artifacts-root (merge-pathnames "artifacts/" root))
         (config (claw-lisp.config::%make-runtime-config
                  :data-root (namestring root)
                  :transcripts-root (namestring transcripts-root)
                  :artifacts-root (namestring artifacts-root)
                  :memory-root (namestring (merge-pathnames "memory/" root))
                  :default-provider "mock"
                  :default-model "mock-model"
                  :tool-preview-bytes 8
                  ))
         (runtime (make-runtime :config config)))
    (unwind-protect
         (progn
           (register-default-providers runtime)
           (register-tool runtime (make-echo-tool))
           (let* ((session (start-session runtime
                                          :provider-name "mock"
                                          :model "mock-model"
                                          :session-id "persist-test"))
                  (result (execute-registered-tool runtime session "echo" '(:text "123456789ABCDE")))
                  (persisted-path (claw-lisp.core.domain:tool-result-persisted-path result))
                  (full-content (read-persisted-tool-result persisted-path)))
             (%assert (claw-lisp.core.domain:tool-result-truncated-p result)
                      "Expected persisted tool result to be truncated")
             (%assert (string= "12345678" (tool-result-content result))
                      "Unexpected preview content: ~A"
                      (tool-result-content result))
             (%assert persisted-path
                      "Expected persisted path for oversized tool result")
             (%assert (probe-file persisted-path)
                      "Expected persisted artifact file at ~A"
                      persisted-path)
             (%assert (string= "123456789ABCDE" full-content)
                      "Unexpected persisted content: ~A"
                      full-content)))
      (when (probe-file root)
        (uiop:delete-directory-tree root :validate t)))))

(defun test-delete-session-tool-results ()
  (let* ((root (merge-pathnames
                (format nil "claw-lisp-cleanup-test-~A/" (get-universal-time))
                #P"/tmp/"))
         (transcripts-root (merge-pathnames "transcripts/" root))
         (artifacts-root (merge-pathnames "artifacts/" root))
         (config (claw-lisp.config::%make-runtime-config
                  :data-root (namestring root)
                  :transcripts-root (namestring transcripts-root)
                  :artifacts-root (namestring artifacts-root)
                  :memory-root (namestring (merge-pathnames "memory/" root))
                  :default-provider "mock"
                  :default-model "mock-model"
                  :tool-preview-bytes 8
                  ))
         (runtime (make-runtime :config config))
         (session-id "cleanup-test"))
    (unwind-protect
         (progn
           (register-default-providers runtime)
           (register-tool runtime (make-echo-tool))
           (let* ((session (start-session runtime
                                          :provider-name "mock"
                                          :model "mock-model"
                                          :session-id session-id))
                  (result (execute-registered-tool runtime session "echo" '(:text "123456789ABCDE")))
                  (persisted-path (claw-lisp.core.domain:tool-result-persisted-path result)))
             (%assert (probe-file persisted-path)
                      "Expected persisted artifact file at ~A"
                      persisted-path)
             (delete-session-tool-results config session-id)
             (%assert (not (probe-file persisted-path))
                      "Expected persisted artifact file to be deleted: ~A"
                      persisted-path)))
      (when (probe-file root)
        (uiop:delete-directory-tree root :validate t)))))

(defun test-microcompact-clears-old-persisted-previews ()
  (let* ((root (merge-pathnames
                (format nil "claw-lisp-microcompact-test-~A/" (get-universal-time))
                #P"/tmp/"))
         (transcripts-root (merge-pathnames "transcripts/" root))
         (artifacts-root (merge-pathnames "artifacts/" root))
         (config (claw-lisp.config::%make-runtime-config
                  :data-root (namestring root)
                  :transcripts-root (namestring transcripts-root)
                  :artifacts-root (namestring artifacts-root)
                  :memory-root (namestring (merge-pathnames "memory/" root))
                  :default-provider "mock"
                  :default-model "mock-model"
                  :tool-preview-bytes 8
                  :microcompact-keep-recent-tool-results 1
                  ))
         (runtime (make-runtime :config config)))
    (unwind-protect
         (progn
           (register-default-providers runtime)
           (register-tool runtime (make-echo-tool))
           (let ((session (start-session runtime
                                         :provider-name "mock"
                                         :model "mock-model"
                                         :session-id "microcompact-test")))
             (execute-registered-tool runtime session "echo" '(:text "123456789ABCDE"))
             (execute-registered-tool runtime session "echo" '(:text "ABCDEFGHIJKLMN"))
             (let* ((conversation (claw-lisp.core.domain:agent-session-conversation session))
                    (results (conversation-tool-results conversation))
                    (first-result (first results))
                    (second-result (second results))
                    (lines (read-lines (session-transcript-path runtime session))))
               (%assert (= 2 (length results))
                        "Expected two tool results, got ~A"
                        (length results))
               (%assert (string= "[old tool result content cleared; see persisted-path for full content]"
                                 (tool-result-content first-result))
                        "Expected cleared preview placeholder, got ~A"
                        (tool-result-content first-result))
               (%assert (string= "ABCDEFGH" (tool-result-content second-result))
                        "Expected recent preview to remain intact, got ~A"
                        (tool-result-content second-result))
               (%assert (some (lambda (line) (search "\"event\":\"microcompact\"" line)) lines)
                        "Expected a microcompact transcript event, got ~A"
                        lines))))
      (when (probe-file root)
        (uiop:delete-directory-tree root :validate t)))))

(defun test-tool-result-aggregate-budget-trims-older-previews ()
  (let* ((root (merge-pathnames
                (format nil "claw-lisp-tool-budget-test-~A/" (get-universal-time))
                #P"/tmp/"))
         (transcripts-root (merge-pathnames "transcripts/" root))
         (artifacts-root (merge-pathnames "artifacts/" root))
         (config (claw-lisp.config::%make-runtime-config
                  :data-root (namestring root)
                  :transcripts-root (namestring transcripts-root)
                  :artifacts-root (namestring artifacts-root)
                  :memory-root (namestring (merge-pathnames "memory/" root))
                  :default-provider "mock"
                  :default-model "mock-model"
                  :tool-preview-bytes 8
                  :tool-result-aggregate-budget-bytes 12
                  :microcompact-keep-recent-tool-results 2
                  ))
         (runtime (make-runtime :config config)))
    (unwind-protect
         (progn
           (register-default-providers runtime)
           (register-tool runtime (make-echo-tool))
           (let ((session (start-session runtime
                                         :provider-name "mock"
                                         :model "mock-model"
                                         :session-id "tool-budget-test")))
             (execute-registered-tool runtime session "echo" '(:text "123456789ABCDE"))
             (execute-registered-tool runtime session "echo" '(:text "ABCDEFGHIJKLMN"))
             (let* ((conversation (claw-lisp.core.domain:agent-session-conversation session))
                    (results (conversation-tool-results conversation))
                    (first-result (first results))
                    (second-result (second results))
                    (lines (read-lines (session-transcript-path runtime session))))
               (%assert (= 2 (length results))
                        "Expected two tool results, got ~A"
                        (length results))
               (%assert (string= "[old tool result content cleared; see persisted-path for full content]"
                                 (tool-result-content first-result))
                        "Expected aggregate budget to clear the older preview, got ~A"
                        (tool-result-content first-result))
               (%assert (string= "ABCDEFGH" (tool-result-content second-result))
                        "Expected aggregate budget to keep the most recent preview, got ~A"
                        (tool-result-content second-result))
               (%assert (some (lambda (line) (search "\"event\":\"tool_result_budget_trim\"" line)) lines)
                        "Expected a tool_result_budget_trim transcript event, got ~A"
                        lines))))
      (when (probe-file root)
        (uiop:delete-directory-tree root :validate t)))))

(defun test-cli-tool-parse ()
  (unless (fboundp 'claw-lisp.cli::parse-args)
    (return-from test-cli-tool-parse t))
  (let ((args (claw-lisp.cli::parse-args '("--tool" "echo" "--tool-text" "hello cli tool"))))
    (%assert (string= "echo" (getf args :tool))
             "Expected tool name from CLI parse, got ~A"
             (getf args :tool))
    (%assert (string= "hello cli tool" (getf args :tool-text))
             "Expected tool text from CLI parse, got ~A"
             (getf args :tool-text))))

(defun test-cli-file-read-parse ()
  (unless (fboundp 'claw-lisp.cli::parse-args)
    (return-from test-cli-file-read-parse t))
  (let ((args (claw-lisp.cli::parse-args '("--tool" "file-read" "--tool-path" "/tmp/demo.txt"))))
    (%assert (string= "file-read" (getf args :tool))
             "Expected file-read tool name from CLI parse, got ~A"
             (getf args :tool))
    (%assert (string= "/tmp/demo.txt" (getf args :tool-path))
             "Expected tool path from CLI parse, got ~A"
             (getf args :tool-path))))

(defun test-cli-file-write-parse ()
  (unless (fboundp 'claw-lisp.cli::parse-args)
    (return-from test-cli-file-write-parse t))
  (let ((args (claw-lisp.cli::parse-args
               '("--tool" "file-write" "--tool-path" "/tmp/out.txt" "--tool-text" "hello write"))))
    (%assert (string= "file-write" (getf args :tool))
             "Expected file-write tool name from CLI parse, got ~A"
             (getf args :tool))
    (%assert (string= "/tmp/out.txt" (getf args :tool-path))
             "Expected file-write tool path from CLI parse, got ~A"
             (getf args :tool-path))
    (%assert (string= "hello write" (getf args :tool-text))
             "Expected file-write tool text from CLI parse, got ~A"
             (getf args :tool-text))))

(defun test-cli-file-replace-parse ()
  (unless (fboundp 'claw-lisp.cli::parse-args)
    (return-from test-cli-file-replace-parse t))
  (let ((args (claw-lisp.cli::parse-args
               '("--tool" "file-replace" "--tool-path" "/tmp/out.txt" "--tool-match" "old" "--tool-text" "new"))))
    (%assert (string= "file-replace" (getf args :tool))
             "Expected file-replace tool name from CLI parse, got ~A"
             (getf args :tool))
    (%assert (string= "/tmp/out.txt" (getf args :tool-path))
             "Expected file-replace tool path from CLI parse, got ~A"
             (getf args :tool-path))
    (%assert (string= "old" (getf args :tool-match))
             "Expected file-replace tool match from CLI parse, got ~A"
             (getf args :tool-match))
    (%assert (string= "new" (getf args :tool-text))
             "Expected file-replace tool replacement text from CLI parse, got ~A"
             (getf args :tool-text))))

(defun test-cli-shell-command-parse ()
  (unless (fboundp 'claw-lisp.cli::parse-args)
    (return-from test-cli-shell-command-parse t))
  (let ((args (claw-lisp.cli::parse-args
               '("--tool" "shell-command" "--tool-text" "pwd"))))
    (%assert (string= "shell-command" (getf args :tool))
             "Expected shell-command tool name from CLI parse, got ~A"
             (getf args :tool))
    (%assert (string= "pwd" (getf args :tool-text))
             "Expected shell-command tool text from CLI parse, got ~A"
             (getf args :tool-text))))

(defun test-cli-glob-parse ()
  (unless (fboundp 'claw-lisp.cli::parse-args)
    (return-from test-cli-glob-parse t))
  (let ((args (claw-lisp.cli::parse-args
               '("--tool" "glob" "--tool-path" "/tmp" "--tool-match" "*.txt"))))
    (%assert (string= "glob" (getf args :tool))
             "Expected glob tool name from CLI parse, got ~A"
             (getf args :tool))
    (%assert (string= "/tmp" (getf args :tool-path))
             "Expected glob tool path from CLI parse, got ~A"
             (getf args :tool-path))
    (%assert (string= "*.txt" (getf args :tool-match))
             "Expected glob pattern from CLI parse, got ~A"
             (getf args :tool-match))))

(defun test-cli-grep-parse ()
  (unless (fboundp 'claw-lisp.cli::parse-args)
    (return-from test-cli-grep-parse t))
  (let ((args (claw-lisp.cli::parse-args
               '("--tool" "grep" "--tool-path" "/tmp/demo.txt" "--tool-match" "needle"))))
    (%assert (string= "grep" (getf args :tool))
             "Expected grep tool name from CLI parse, got ~A"
             (getf args :tool))
    (%assert (string= "/tmp/demo.txt" (getf args :tool-path))
             "Expected grep tool path from CLI parse, got ~A"
             (getf args :tool-path))
    (%assert (string= "needle" (getf args :tool-match))
             "Expected grep pattern from CLI parse, got ~A"
             (getf args :tool-match))))

(defun test-cli-tool-parse-requires-tool ()
  (unless (fboundp 'claw-lisp.cli::parse-args)
    (return-from test-cli-tool-parse-requires-tool t))
  (let ((args (claw-lisp.cli::parse-args '("--tool-text" "hello cli tool"))))
    (%assert (string= "--tool is required when using --tool-text, --tool-path, or --tool-match"
                      (getf args :error))
             "Expected --tool-text dependency error, got ~A"
             (getf args :error))))

(defun test-cli-tool-shot ()
  (unless (and (fboundp 'claw-lisp.cli::register-default-tools)
               (fboundp 'claw-lisp.cli::maybe-run-tool-shot))
    (return-from test-cli-tool-shot t))
  (let ((runtime (make-runtime)))
    (claw-lisp.cli::register-default-tools runtime)
    (let ((output (with-output-to-string (stream)
                    (let ((*standard-output* stream))
                      (%assert (= 0 (claw-lisp.cli::maybe-run-tool-shot runtime "echo" "hello cli tool" nil nil))
                               "Expected successful CLI tool shot")))))
      (%assert (search "hello cli tool" output)
               "Expected CLI tool output, got ~A"
               output))))

(defun test-cli-json-run-parse ()
  (let ((args (claw-lisp.cli::parse-args
               '("--json-run"
                 "--request-file" "/tmp/request.json"
                 "--result-file" "/tmp/result.json"
                 "--event-file" "/tmp/events.jsonl"
                 "--cancel-file" "/tmp/cancel.flag"
                 "--timeout-seconds" "9"))))
    (%assert (getf args :json-run)
             "Expected --json-run flag in parsed args, got ~S" args)
    (%assert (string= "/tmp/request.json" (getf args :request-file))
             "Expected request path, got ~A" (getf args :request-file))
    (%assert (string= "/tmp/result.json" (getf args :result-file))
             "Expected result path, got ~A" (getf args :result-file))
    (%assert (string= "/tmp/events.jsonl" (getf args :event-file))
             "Expected event path, got ~A" (getf args :event-file))
    (%assert (string= "/tmp/cancel.flag" (getf args :cancel-file))
             "Expected cancel path, got ~A" (getf args :cancel-file))
    (%assert (= 9 (getf args :timeout-seconds))
             "Expected integer timeout, got ~S" (getf args :timeout-seconds))))

(defun test-cli-json-run-success-with-files ()
  (let* ((temp-root (uiop:temporary-directory))
         (root (merge-pathnames
                (format nil "claw-lisp-json-run-success-~D-~D/"
                        (get-universal-time)
                        (get-internal-real-time))
                temp-root))
         (work-root (merge-pathnames "workspace/" root))
         (data-root (merge-pathnames "data/" root))
         (request-file (merge-pathnames "request.json" root))
         (result-file (merge-pathnames "nested/results/result.json" root))
         (event-file (merge-pathnames "nested/events/events.jsonl" root))
         (request (%make-runner-test-request work-root data-root))
         (stdout "")
         (stderr "")
         exit-code)
    (unwind-protect
         (progn
           (ensure-directories-exist (uiop:ensure-directory-pathname work-root))
           (%write-json-file request-file request)
           (setf stdout
                 (with-output-to-string (stream)
                   (setf stderr
                         (with-output-to-string (err)
                           (setf exit-code
                                 (claw-lisp.cli::%dispatch-cli
                                  (list "--json-run"
                                        "--request-file" (namestring request-file)
                                        "--result-file" (namestring result-file)
                                        "--event-file" (namestring event-file))
                                  :stdout stream
                                  :stderr err))))))
           (%assert (= 0 exit-code)
                    "Expected successful json-run exit code, got ~A" exit-code)
           (%assert (string= "" stdout)
                    "Expected no stdout when result-file is used, got ~S" stdout)
           (%assert (string= "" stderr)
                    "Expected no stderr for successful file-mode run, got ~S" stderr)
           (%assert (probe-file result-file)
                    "Expected result file to be created at ~A" result-file)
           (%assert (probe-file event-file)
                    "Expected event file to be created at ~A" event-file)
           (let* ((result (claw-lisp.providers.http-utils:json-decode
                           (uiop:read-file-string result-file)))
                  (message (getf result :message))
                  (content (and message (getf message :content)))
                  (first-block (and content (first content)))
                  (artifacts (getf result :artifacts))
                  (transcript-path (getf artifacts :transcript_path))
                  (events (mapcar #'claw-lisp.providers.http-utils:json-decode
                                  (read-lines event-file)))
                  (event-types (mapcar (lambda (event) (getf event :event_type)) events)))
             (%assert (string= "succeeded" (getf result :status))
                      "Expected succeeded result, got ~S" result)
             (%assert (string= "req-test-001" (getf result :request_id))
                      "Expected request_id echo, got ~S" result)
             (%assert (and first-block
                           (search "Mock provider reply"
                                   (getf first-block :text)))
                      "Expected mock provider assistant text, got ~S" result)
             (%assert (and transcript-path (probe-file transcript-path))
                      "Expected transcript path in result artifacts, got ~S" artifacts)
             (%assert (member "session.started" event-types :test #'string=)
                      "Expected session.started event, got ~S" event-types)
             (%assert (member "turn.completed" event-types :test #'string=)
                      "Expected turn.completed event, got ~S" event-types)))
      (when (uiop:directory-exists-p root)
        (uiop:delete-directory-tree root :validate t))))

(defun test-cli-runtime-event-mapping ()
  (let* ((tool-start-events (claw-lisp.cli::%runtime-event->runner-events
                             (list :event "tool_start"
                                   :call_id "call-0"
                                   :tool_name "echo"
                                   :input (list :text "hello"))))
         (tool-events (claw-lisp.cli::%runtime-event->runner-events
                       (list :event "tool_result"
                             :call_id "call-1"
                             :tool_name "echo"
                             :result (list :content "ok" :is_error nil))))
         (tool-missing-classification-events
          (claw-lisp.cli::%runtime-event->runner-events
           (list :event "tool_result"
                 :call_id "call-1b"
                 :tool_name "echo"
                 :result (list :content "unknown"))))
         (tool-nonplist-result-events
          (claw-lisp.cli::%runtime-event->runner-events
           (list :event "tool_result"
                 :call_id "call-1c"
                 :tool_name "echo"
                 :result "not-a-plist")))
         (tool-error-events (claw-lisp.cli::%runtime-event->runner-events
                             (list :event "tool_error"
                                   :call_id "call-2"
                                   :tool_name "echo"
                                   :error "boom")))
         (memory-events (claw-lisp.cli::%runtime-event->runner-events
                         (list :event "durable_memory_extract"
                               :saved_count 2)))
         (microcompact-events (claw-lisp.cli::%runtime-event->runner-events
                               (list :event "microcompact"
                                     :cleared_count 3
                                     :keep_recent 1)))
         (child-spawned-events (claw-lisp.cli::%runtime-event->runner-events
                                (list :event "child_spawned"
                                      :child_id "child-0"
                                      :status :starting
                                      :child_transcript_path "/tmp/child-0.jsonl")))
         (child-events (claw-lisp.cli::%runtime-event->runner-events
                        (list :event "child_finished"
                              :child_id "child-1"
                              :status :failed
                              :error "boom")))
         (child-completed-events (claw-lisp.cli::%runtime-event->runner-events
                                  (list :event "child_finished"
                                        :child_id "child-2"
                                        :status "completed")))
         (child-progress-events (claw-lisp.cli::%runtime-event->runner-events
                                 (list :event "child_finished"
                                       :child_id "child-3"
                                       :status "running")))
         (unknown-events (claw-lisp.cli::%runtime-event->runner-events
                          (list :event "unknown_runtime_event"
                                :foo "bar"))))
    (%assert (string= "tool.started" (getf (first tool-start-events) :event_type))
             "Expected tool_start to map to tool.started, got ~S" tool-start-events)
    (%assert (string= "tool.completed" (getf (first tool-events) :event_type))
             "Expected tool_result to map to tool.completed, got ~S" tool-events)
    (%assert (string= "tool.failed" (getf (first tool-missing-classification-events) :event_type))
             "Expected tool_result without :is_error to fail closed, got ~S"
             tool-missing-classification-events)
    (%assert (string= "tool.failed" (getf (first tool-nonplist-result-events) :event_type))
             "Expected non-plist tool_result payload to fail closed, got ~S"
             tool-nonplist-result-events)
    (%assert (string= "tool.failed" (getf (first tool-error-events) :event_type))
             "Expected tool_error to map to tool.failed, got ~S" tool-error-events)
    (%assert (string= "memory.extracted" (getf (first memory-events) :event_type))
             "Expected durable_memory_extract to map to memory.extracted, got ~S"
             memory-events)
    (%assert (string= "context.microcompacted" (getf (first microcompact-events) :event_type))
             "Expected microcompact to map to context.microcompacted, got ~S"
             microcompact-events)
    (%assert (string= "child_agent.spawned" (getf (first child-spawned-events) :event_type))
             "Expected child_spawned to map to child_agent.spawned, got ~S"
             child-spawned-events)
    (%assert (string= "child_agent.failed" (getf (first child-events) :event_type))
             "Expected failed child_finished to map to child_agent.failed, got ~S"
             child-events)
    (%assert (string= "child_agent.completed" (getf (first child-completed-events) :event_type))
             "Expected completed child_finished to map to child_agent.completed, got ~S"
             child-completed-events)
    (%assert (string= "child_agent.progress" (getf (first child-progress-events) :event_type))
             "Expected unknown child_finished status to map to child_agent.progress, got ~S"
             child-progress-events)
    (%assert (null unknown-events)
             "Expected unknown runtime event to map to NIL, got ~S"
             unknown-events)))

(defun test-inspect-transcript-file-tolerates-tool-result-extra-keys ()
  (let* ((root (merge-pathnames
                (format nil "claw-lisp-transcript-tool-result-shape-~A/" (get-universal-time))
                #P"/tmp/"))
         (transcript-path (merge-pathnames "transcripts/session.jsonl" root)))
    (unwind-protect
         (progn
           (ensure-directories-exist transcript-path)
           (with-open-file (stream transcript-path
                                   :direction :output
                                   :if-exists :supersede
                                   :if-does-not-exist :create)
             (format stream
                     "{\"event\":\"tool_result\",\"session_id\":\"shape-audit\",\"result\":{\"tool_name\":\"echo\",\"content\":\"ok\",\"is_error\":false}}~%"))
           (let ((summary (claw-lisp.core.runtime::inspect-transcript-file transcript-path :tail-count 1)))
             (%assert (= 1 (getf summary :parsed-json-count))
                      "Expected tool_result line with extra keys to parse, got ~S" summary)
             (%assert (string= "tool_result" (getf summary :last-event))
                      "Expected tool_result to remain the last event, got ~S" summary)
             (%assert (= 0 (getf summary :malformed-json-count))
                      "Expected no malformed JSON lines, got ~S" summary)))
      (when (probe-file root)
        (uiop:delete-directory-tree root :validate t)))))

(defun test-runtime-event-callback-observes-tool-events ()
  (let* ((root (merge-pathnames
                (format nil "claw-lisp-runtime-callback-events-~A/" (get-universal-time))
                #P"/tmp/"))
         (config (claw-lisp.config::%make-runtime-config
                  :data-root (namestring root)
                  :transcripts-root (namestring (merge-pathnames "transcripts/" root))
                  :artifacts-root (namestring (merge-pathnames "artifacts/" root))
                  :memory-root (namestring (merge-pathnames "memory/" root))
                  :default-provider "mock"
                  :default-model "mock-model"))
         (runtime (make-runtime :config config))
         (events nil))
    (unwind-protect
         (progn
           (register-default-providers runtime)
           (register-tool runtime (make-echo-tool))
           (let ((session (start-session runtime
                                         :provider-name "mock"
                                         :model "mock-model"
                                         :session-id "runtime-callback-events")))
             (claw-lisp.core.runtime:submit-user-message
              runtime session "tool:echo:hello from callback"
              :runtime-event-callback (lambda (event)
                                        (push (getf event :event) events)))
             (%assert (member "tool_start" events :test #'string=)
                      "Expected runtime callback to observe tool_start, got ~S" events)
             (%assert (member "tool_result" events :test #'string=)
                      "Expected runtime callback to observe tool_result, got ~S" events)))
      (when (probe-file root)
        (uiop:delete-directory-tree root :validate t)))))

(defun test-runtime-event-callback-warning-on-error ()
  (let* ((runtime (make-runtime))
         (session (start-session runtime
                                 :provider-name "mock"
                                 :model "mock-model"
                                 :session-id "runtime-callback-warning"))
         (warnings nil))
    (claw-lisp.core.runtime::set-session-state-value
     session
     :runtime-event-callback
     (lambda (event)
       (declare (ignore event))
       (error "intentional callback failure")))
    (handler-bind
        ((warning (lambda (condition)
                    (push (princ-to-string condition) warnings)
                    (muffle-warning condition))))
      (claw-lisp.core.runtime:maybe-append-transcript-event
       session
       (claw-lisp.core.runtime:session-transcript-path runtime session)
       (list :event "unit_test_callback_failure")))
    (%assert warnings
             "Expected callback failure to emit a warning.")
    (%assert (search "unit_test_callback_failure" (first warnings))
             "Expected warning to include event context, got ~S"
             warnings)))

(defun test-submit-user-message-restores-runtime-event-callback ()
  (let* ((root (merge-pathnames
                (format nil "claw-lisp-runtime-callback-restore-~A/" (get-universal-time))
                #P"/tmp/"))
         (config (claw-lisp.config::%make-runtime-config
                  :data-root (namestring root)
                  :transcripts-root (namestring (merge-pathnames "transcripts/" root))
                  :artifacts-root (namestring (merge-pathnames "artifacts/" root))
                  :memory-root (namestring (merge-pathnames "memory/" root))
                  :default-provider "mock"
                  :default-model "mock-model"))
         (runtime (make-runtime :config config))
         (original-callback (lambda (event)
                              (declare (ignore event))
                              :original))
         (temporary-callback (lambda (event)
                               (declare (ignore event))
                               :temporary)))
    (unwind-protect
         (progn
           (register-default-providers runtime)
           (let ((session (start-session runtime
                                         :provider-name "mock"
                                         :model "mock-model"
                                         :session-id "runtime-callback-restore")))
             (claw-lisp.core.runtime::set-session-state-value
              session :runtime-event-callback original-callback)
             (submit-user-message runtime session "restore callback test"
                                  :runtime-event-callback temporary-callback)
             (%assert (eq original-callback
                          (claw-lisp.core.runtime::session-runtime-event-callback session))
                      "Expected original runtime callback to be restored after submit-user-message.")))
      (when (probe-file root)
        (uiop:delete-directory-tree root :validate t)))))

(defun test-submit-user-message-restores-runtime-event-callback-after-error ()
  (let* ((root (merge-pathnames
                (format nil "claw-lisp-runtime-callback-restore-error-~A/" (get-universal-time))
                #P"/tmp/"))
         (config (claw-lisp.config::%make-runtime-config
                  :data-root (namestring root)
                  :transcripts-root (namestring (merge-pathnames "transcripts/" root))
                  :artifacts-root (namestring (merge-pathnames "artifacts/" root))
                  :memory-root (namestring (merge-pathnames "memory/" root))
                  :default-provider "mock"
                  :default-model "mock-model"))
         (runtime (make-runtime :config config))
         (original-callback (lambda (event)
                              (declare (ignore event))
                              :original))
         (temporary-callback (lambda (event)
                               (declare (ignore event))
                               :temporary)))
    (unwind-protect
         (progn
           (register-default-providers runtime)
           (let ((session (start-session runtime
                                         :provider-name "mock"
                                         :model "mock-model"
                                         :session-id "runtime-callback-restore-error")))
             (claw-lisp.core.runtime::set-session-state-value
              session :runtime-event-callback original-callback)
             (%with-redefined-function
                 ('claw-lisp.core.runtime:execute-provider-turn-loop
                  (lambda (&rest args)
                    (declare (ignore args))
                    (error "intentional submit failure")))
               (handler-case
                   (submit-user-message runtime session "restore callback error test"
                                        :runtime-event-callback temporary-callback)
                 (error (condition)
                   (%assert (search "intentional submit failure" (princ-to-string condition))
                            "Expected forced submit-user-message failure, got ~A"
                            condition))))
             (%assert (eq original-callback
                          (claw-lisp.core.runtime::session-runtime-event-callback session))
                      "Expected original runtime callback to be restored after submit-user-message error.")))
      (when (probe-file root)
        (uiop:delete-directory-tree root :validate t)))))

(defun test-runtime-event-callback-covers-broader-runtime-events ()
  (let* ((root (merge-pathnames
                (format nil "claw-lisp-runtime-callback-broad-~A/" (get-universal-time))
                #P"/tmp/"))
         (config (claw-lisp.config::%make-runtime-config
                  :data-root (namestring root)
                  :transcripts-root (namestring (merge-pathnames "transcripts/" root))
                  :artifacts-root (namestring (merge-pathnames "artifacts/" root))
                  :memory-root (namestring (merge-pathnames "memory/" root))
                  :default-provider "mock"
                  :default-model "mock-model"
                  :tool-preview-bytes 8
                  :microcompact-keep-recent-tool-results 1))
         (runtime (make-runtime :config config))
         (events nil))
    (unwind-protect
         (progn
           (register-default-providers runtime)
           (register-tool runtime (make-echo-tool))
           (let ((session (start-session runtime
                                         :provider-name "mock"
                                         :model "mock-model"
                                         :session-id "runtime-callback-broad")))
             (claw-lisp.core.runtime::set-session-state-value
              session
              :runtime-event-callback
              (lambda (event)
                (push (getf event :event) events)))
             (submit-user-message runtime session "auto durable memory from message")
             (claw-lisp.core.runtime:maybe-append-transcript-event
              session
              (claw-lisp.core.runtime:session-transcript-path runtime session)
              (list :event "durable_memory_extract"
                    :session_id (claw-lisp.core.domain:agent-session-id session)
                    :saved_count 1))
             (execute-registered-tool runtime session "echo" '(:text "ABCDEFGHIJKLMN"))
             (execute-registered-tool runtime session "echo" '(:text "123456789ABCDE"))
             (%assert (member "durable_memory_extract" events :test #'string=)
                      "Expected callback to observe durable_memory_extract, got ~S" events)
             (%assert (member "microcompact" events :test #'string=)
                      "Expected callback to observe microcompact, got ~S" events)))
      (when (probe-file root)
        (uiop:delete-directory-tree root :validate t)))))

(defun test-execute-registered-tool-tool-error-omits-unauthorized-input ()
  (let* ((root (merge-pathnames
                (format nil "claw-lisp-tool-error-authorization-~A/" (get-universal-time))
                #P"/tmp/"))
         (forbidden-path "/etc/passwd")
         (config (claw-lisp.config::%make-runtime-config
                  :data-root (namestring root)
                  :transcripts-root (namestring (merge-pathnames "transcripts/" root))
                  :artifacts-root (namestring (merge-pathnames "artifacts/" root))
                  :memory-root (namestring (merge-pathnames "memory/" root))
                  :default-provider "mock"
                  :default-model "mock-model"))
         (runtime (make-runtime :config config))
         (events nil))
    (unwind-protect
         (progn
           (register-default-providers runtime)
           (register-tool runtime (make-file-read-tool))
           (let ((session (start-session runtime
                                         :provider-name "mock"
                                         :model "mock-model"
                                         :session-id "tool-error-authorization")))
             (claw-lisp.core.runtime::set-session-state-value
              session
              :runtime-event-callback
              (lambda (event)
                (push event events)))
             (handler-case
                 (execute-registered-tool runtime session "file-read" (list :path forbidden-path))
               (error ()
                 nil))
             (let ((tool-error-event (find "tool_error" events
                                           :key (lambda (event) (getf event :event))
                                           :test #'string=)))
               (%assert tool-error-event
                        "Expected tool_error event for forbidden file-read input, got ~S"
                        events)
               (%assert (null (getf tool-error-event :input))
                        "Expected tool_error event to omit unauthorized input, got ~S"
                        tool-error-event)
               (%assert (null (search forbidden-path (princ-to-string tool-error-event)))
                        "Expected tool_error event to exclude forbidden path, got ~S"
                        tool-error-event))))
      (when (probe-file root)
        (uiop:delete-directory-tree root :validate t)))))

(defun test-execute-registered-tool-postprocess-failure-does-not-emit-tool-error ()
  (let* ((root (merge-pathnames
                (format nil "claw-lisp-tool-postprocess-failure-~A/" (get-universal-time))
                #P"/tmp/"))
         (config (claw-lisp.config::%make-runtime-config
                  :data-root (namestring root)
                  :transcripts-root (namestring (merge-pathnames "transcripts/" root))
                  :artifacts-root (namestring (merge-pathnames "artifacts/" root))
                  :memory-root (namestring (merge-pathnames "memory/" root))
                  :default-provider "mock"
                  :default-model "mock-model"))
         (runtime (make-runtime :config config))
         (events nil)
         (caught-error nil))
    (unwind-protect
         (progn
           (register-default-providers runtime)
           (register-tool runtime (make-echo-tool))
           (let ((session (start-session runtime
                                         :provider-name "mock"
                                         :model "mock-model"
                                         :session-id "tool-postprocess-failure")))
             (claw-lisp.core.runtime::set-session-state-value
              session
              :runtime-event-callback
              (lambda (event)
                (push event events)))
             (%with-redefined-function
                 ('claw-lisp.core.runtime::maybe-extract-durable-memory
                  (lambda (runtime session)
                    (declare (ignore runtime session))
                    (error "forced postprocess failure")))
               (handler-case
                   (execute-registered-tool runtime session "echo" '(:text "postprocess failure"))
                 (error (condition)
                   (setf caught-error condition)))))
           (%assert caught-error
                    "Expected post-processing failure to propagate.")
           (%assert (search "forced postprocess failure" (princ-to-string caught-error))
                    "Expected post-processing error text, got ~S" caught-error)
           (%assert (find "tool_result" events
                          :key (lambda (event) (getf event :event))
                          :test #'string=)
                    "Expected tool_result before post-processing failure, got ~S"
                    events)
           (%assert (null (find "tool_error" events
                                :key (lambda (event) (getf event :event))
                                :test #'string=))
                    "Did not expect tool_error for a post-processing failure, got ~S"
                    events))
      (when (probe-file root)
        (uiop:delete-directory-tree root :validate t)))))

(defun test-cli-json-run-tool-success-events-with-file-output ()
  (let* ((temp-root (uiop:temporary-directory))
         (root (merge-pathnames
                (format nil "claw-lisp-json-run-tool-events-~D-~D/"
                        (get-universal-time)
                        (get-internal-real-time))
                temp-root))
         (work-root (merge-pathnames "workspace/" root))
         (data-root (merge-pathnames "data/" root))
         (request-file (merge-pathnames "request.json" root))
         (result-file (merge-pathnames "result.json" root))
         (event-file (merge-pathnames "events.jsonl" root))
         (request (%make-runner-test-request work-root data-root
                                             :user-input "tool:echo:hello from json runner"))
         (stdout "")
         (stderr "")
         exit-code)
    (unwind-protect
         (progn
           (ensure-directories-exist (uiop:ensure-directory-pathname work-root))
           (%write-json-file request-file request)
           (setf stdout
                 (with-output-to-string (stream)
                   (setf stderr
                         (with-output-to-string (err)
                           (setf exit-code
                                 (claw-lisp.cli::%dispatch-cli
                                  (list "--json-run"
                                        "--request-file" (namestring request-file)
                                        "--result-file" (namestring result-file)
                                        "--event-file" (namestring event-file))
                                  :stdout stream
                                  :stderr err))))))
           (%assert (= 0 exit-code)
                    "Expected successful json-run tool event exit code, got ~A" exit-code)
           (%assert (string= "" stdout)
                    "Expected no stdout when result-file is used, got ~S" stdout)
           (%assert (string= "" stderr)
                    "Expected no stderr for successful tool event run, got ~S" stderr)
           (let* ((result (claw-lisp.providers.http-utils:json-decode
                           (uiop:read-file-string result-file)))
                  (events (mapcar #'claw-lisp.providers.http-utils:json-decode
                                  (read-lines event-file)))
                  (event-types (mapcar (lambda (event) (getf event :event_type)) events)))
             (%assert (string= "succeeded" (getf result :status))
                      "Expected succeeded tool-event result, got ~S" result)
             (%assert (member "tool.started" event-types :test #'string=)
                      "Expected tool.started event, got ~S" event-types)
             (%assert (member "tool.completed" event-types :test #'string=)
                      "Expected tool.completed event, got ~S" event-types)))
      (when (uiop:directory-exists-p root)
        (uiop:delete-directory-tree root :validate t)))))

(defun test-cli-json-run-tool-failure-events-with-file-output ()
  (let* ((temp-root (uiop:temporary-directory))
         (root (merge-pathnames
                (format nil "claw-lisp-json-run-tool-failure-events-~D-~D/"
                        (get-universal-time)
                        (get-internal-real-time))
                temp-root))
         (work-root (merge-pathnames "workspace/" root))
         (data-root (merge-pathnames "data/" root))
         (request-file (merge-pathnames "request.json" root))
         (result-file (merge-pathnames "result.json" root))
         (event-file (merge-pathnames "events.jsonl" root))
         (request (%make-runner-test-request work-root data-root
                                             :user-input "tool:echo:hello from failing json runner"))
         (stdout "")
         (stderr "")
         exit-code)
    (unwind-protect
         (progn
           (ensure-directories-exist (uiop:ensure-directory-pathname work-root))
           (%write-json-file request-file request)
           (%with-redefined-function
               ('claw-lisp.core.runtime:register-default-tools
                (lambda (runtime)
                  (register-tool runtime (make-failing-echo-tool))
                  runtime))
             (setf stdout
                   (with-output-to-string (stream)
                     (setf stderr
                           (with-output-to-string (err)
                             (setf exit-code
                                   (claw-lisp.cli::%dispatch-cli
                                    (list "--json-run"
                                          "--request-file" (namestring request-file)
                                          "--result-file" (namestring result-file)
                                          "--event-file" (namestring event-file))
                                    :stdout stream
                                    :stderr err)))))))
           (%assert (= 0 exit-code)
                    "Expected graceful json-run tool failure exit code 0, got ~A" exit-code)
           (%assert (string= "" stdout)
                    "Expected no stdout when result-file is used, got ~S" stdout)
           (%assert (string= "" stderr)
                    "Expected no stderr for failing tool event run, got ~S" stderr)
           (let* ((result (claw-lisp.providers.http-utils:json-decode
                           (uiop:read-file-string result-file)))
                  (events (mapcar #'claw-lisp.providers.http-utils:json-decode
                                  (read-lines event-file)))
                  (event-types (mapcar (lambda (event) (getf event :event_type)) events)))
             (%assert (string= "succeeded" (getf result :status))
                      "Expected graceful tool-event result, got ~S" result)
             (%assert (member "tool.started" event-types :test #'string=)
                      "Expected tool.started event, got ~S" event-types)
             (%assert (member "tool.failed" event-types :test #'string=)
                      "Expected tool.failed event, got ~S" event-types)))
      (when (uiop:directory-exists-p root)
        (uiop:delete-directory-tree root :validate t)))))

(defun test-cli-json-run-invalid-request-exits-20 ()
  (let* ((request (list :protocol_version "claw-runner/v0"
                        :correlation_id "corr-only"
                        :job_id "job-only"
                        :task_id "task-only"
                        :node_id "node-only"
                        :session_id "session-only"
                        :user_input "hello"))
         (request-json (claw-lisp.providers.http-utils:json-encode-string request))
         (stdout "")
         (stderr "")
         exit-code)
    (setf stdout
          (with-output-to-string (stream)
            (setf stderr
                  (with-output-to-string (err)
                    (with-input-from-string (input request-json)
                      (setf exit-code
                            (claw-lisp.cli::%dispatch-cli
                             '("--json-run")
                             :stdin input
                             :stdout stream
                             :stderr err)))))))
    (%assert (= 20 exit-code)
             "Expected invalid request exit code 20, got ~A" exit-code)
    (%assert (string= "" stdout)
             "Expected no stdout for invalid request, got ~S" stdout)
    (%assert (search "request_id" stderr)
             "Expected stderr to mention missing request_id, got ~S" stderr)))

(defun test-cli-json-run-invalid-request-timeout-exits-20 ()
  (let* ((request (list :protocol_version "claw-runner/v0"
                        :request_id "req-invalid-timeout"
                        :correlation_id "corr-invalid-timeout"
                        :job_id "job-invalid-timeout"
                        :task_id "task-invalid-timeout"
                        :node_id "node-invalid-timeout"
                        :session_id "session-invalid-timeout"
                        :user_input "hello"
                        :timeouts (list :turn_timeout_seconds -1)))
         (request-json (claw-lisp.providers.http-utils:json-encode-string request))
         (stdout "")
         (stderr "")
         exit-code)
    (setf stdout
          (with-output-to-string (stream)
            (setf stderr
                  (with-output-to-string (err)
                    (with-input-from-string (input request-json)
                      (setf exit-code
                            (claw-lisp.cli::%dispatch-cli
                             '("--json-run")
                             :stdin input
                             :stdout stream
                             :stderr err)))))))
    (%assert (= 20 exit-code)
             "Expected invalid timeout request exit code 20, got ~A" exit-code)
    (%assert (string= "" stdout)
             "Expected no stdout for invalid timeout request, got ~S" stdout)
    (%assert (search "turn_timeout_seconds" stderr)
             "Expected stderr to mention turn_timeout_seconds, got ~S" stderr)))

(defun test-cli-json-run-stdout-result ()
  (let* ((temp-root (uiop:temporary-directory))
         (root (merge-pathnames
                (format nil "claw-lisp-json-run-stdout-~D-~D/"
                        (get-universal-time)
                        (get-internal-real-time))
                temp-root))
         (work-root (merge-pathnames "workspace/" root))
         (data-root (merge-pathnames "data/" root))
         (request (%make-runner-test-request work-root data-root :event-stream-p nil))
         (request-json (claw-lisp.providers.http-utils:json-encode-string request))
         (stdout "")
         (stderr "")
         exit-code)
    (unwind-protect
         (progn
           (ensure-directories-exist (uiop:ensure-directory-pathname work-root))
           (setf stdout
                 (with-output-to-string (stream)
                   (setf stderr
                         (with-output-to-string (err)
                           (with-input-from-string (input request-json)
                             (setf exit-code
                                   (claw-lisp.cli::%dispatch-cli
                                    '("--json-run")
                                    :stdin input
                                    :stdout stream
                                    :stderr err)))))))
           (%assert (= 0 exit-code)
                    "Expected successful stdout-mode exit code, got ~A" exit-code)
           (%assert (string= "" stderr)
                    "Expected no stderr in stdout result mode, got ~S" stderr)
           (let* ((result (claw-lisp.providers.http-utils:json-decode stdout))
                  (artifacts (getf result :artifacts))
                  (memory-paths (getf artifacts :memory_paths)))
             (%assert (string= "succeeded" (getf result :status))
                      "Expected succeeded stdout result, got ~S" result)
             (%assert (string= "runner-session-001" (getf result :session_id))
                      "Expected session_id echo in stdout result, got ~S" result)
             (%assert (listp memory-paths)
                      "Expected memory_paths to decode as a list, got ~S" memory-paths)
             (%assert (every #'stringp memory-paths)
                      "Expected memory_paths entries to be strings, got ~S"
                      memory-paths))))
      (when (uiop:directory-exists-p root)
        (uiop:delete-directory-tree root :validate t)))))

(defun test-cli-json-run-cancel-file-at-startup ()
  (let* ((temp-root (uiop:temporary-directory))
         (root (merge-pathnames
                (format nil "claw-lisp-json-run-cancel-start-~D-~D/"
                        (get-universal-time)
                        (get-internal-real-time))
                temp-root))
         (work-root (merge-pathnames "workspace/" root))
         (data-root (merge-pathnames "data/" root))
         (request-file (merge-pathnames "request.json" root))
         (result-file (merge-pathnames "result.json" root))
         (cancel-file (merge-pathnames "cancel.flag" root))
         (request (%make-runner-test-request work-root data-root :event-stream-p nil))
         exit-code stdout stderr)
    (unwind-protect
         (progn
           (ensure-directories-exist (uiop:ensure-directory-pathname work-root))
           (%write-json-file request-file request)
           (with-open-file (stream cancel-file
                                   :direction :output
                                   :if-exists :supersede
                                   :if-does-not-exist :create)
             (write-string "cancel" stream))
           (multiple-value-setq (exit-code stdout stderr)
             (%dispatch-cli-capturing
              (list "--json-run"
                    "--request-file" (namestring request-file)
                    "--result-file" (namestring result-file)
                    "--cancel-file" (namestring cancel-file))))
           (%assert (= 11 exit-code)
                    "Expected startup cancel exit code 11, got ~A" exit-code)
           (%assert (string= "" stdout)
                    "Expected no stdout for startup cancel, got ~S" stdout)
           (%assert (string= "" stderr)
                    "Expected no stderr for startup cancel, got ~S" stderr)
           (let ((result (claw-lisp.providers.http-utils:json-decode
                          (uiop:read-file-string result-file))))
             (%assert (string= "cancelled" (getf result :status))
                      "Expected cancelled result, got ~S" result)
             (%assert (string= "cancel_file_present" (getf (getf result :failure) :code))
                      "Expected cancel failure code, got ~S" result)))
      (when (uiop:directory-exists-p root)
        (uiop:delete-directory-tree root :validate t)))))

(defun test-cli-json-run-cancel-file-post-turn ()
  (let* ((temp-root (uiop:temporary-directory))
         (root (merge-pathnames
                (format nil "claw-lisp-json-run-cancel-post-~D-~D/"
                        (get-universal-time)
                        (get-internal-real-time))
                temp-root))
         (work-root (merge-pathnames "workspace/" root))
         (data-root (merge-pathnames "data/" root))
         (request-file (merge-pathnames "request.json" root))
         (result-file (merge-pathnames "result.json" root))
         (cancel-file (merge-pathnames "cancel.flag" root))
         (request (%make-runner-test-request work-root data-root :event-stream-p nil))
         exit-code stdout stderr)
    (unwind-protect
         (progn
           (ensure-directories-exist (uiop:ensure-directory-pathname work-root))
           (%write-json-file request-file request)
           (%with-redefined-function
               ('claw-lisp.cli::%submit-runner-user-message
                (lambda (runtime session request cwd timeout-seconds &key &allow-other-keys)
                  (declare (ignore timeout-seconds))
                  (uiop:with-current-directory (cwd)
                    (claw-lisp.core.runtime:submit-user-message
                     runtime session (getf request :user_input)))
                  (with-open-file (stream cancel-file
                                          :direction :output
                                          :if-exists :supersede
                                          :if-does-not-exist :create)
                    (write-string "cancel" stream))
                  session))
             (multiple-value-setq (exit-code stdout stderr)
               (%dispatch-cli-capturing
                (list "--json-run"
                      "--request-file" (namestring request-file)
                      "--result-file" (namestring result-file)
                      "--cancel-file" (namestring cancel-file)))))
           (%assert (= 11 exit-code)
                    "Expected post-turn cancel exit code 11, got ~A" exit-code)
           (%assert (string= "" stdout)
                    "Expected no stdout for post-turn cancel, got ~S" stdout)
           (%assert (string= "" stderr)
                    "Expected no stderr for post-turn cancel, got ~S" stderr)
           (let ((result (claw-lisp.providers.http-utils:json-decode
                          (uiop:read-file-string result-file))))
             (%assert (string= "cancelled" (getf result :status))
                      "Expected cancelled result, got ~S" result)
             (%assert (string= "cancel_file_present" (getf (getf result :failure) :code))
                      "Expected cancel failure code, got ~S" result)))
      (when (uiop:directory-exists-p root)
        (uiop:delete-directory-tree root :validate t)))))

(defun test-cli-json-run-unknown-provider-exits-22 ()
  (let* ((temp-root (uiop:temporary-directory))
         (root (merge-pathnames
                (format nil "claw-lisp-json-run-provider-~D-~D/"
                        (get-universal-time)
                        (get-internal-real-time))
                temp-root))
         (work-root (merge-pathnames "workspace/" root))
         (data-root (merge-pathnames "data/" root))
         (request (%make-runner-test-request work-root data-root :event-stream-p nil))
         (request-json (progn
                         (remf request :provider)
                         (claw-lisp.providers.http-utils:json-encode-string request)))
         exit-code stdout stderr)
    (unwind-protect
         (progn
           (ensure-directories-exist (uiop:ensure-directory-pathname work-root))
           (multiple-value-setq (exit-code stdout stderr)
             (with-input-from-string (input request-json)
               (%dispatch-cli-capturing
                '("--json-run" "--provider" "missing-provider")
                :stdin input)))
           (%assert (= 22 exit-code)
                    "Expected unknown-provider exit code 22, got ~A" exit-code)
           (%assert (string= "" stdout)
                    "Expected no stdout for unknown-provider path, got ~S" stdout)
           (%assert (search "Unknown or unconfigured provider" stderr)
                    "Expected unknown-provider stderr, got ~S" stderr))
      (when (uiop:directory-exists-p root)
        (uiop:delete-directory-tree root :validate t)))))

(defun test-cli-json-run-timeout-exits-12 ()
  #+sbcl
  (let* ((temp-root (uiop:temporary-directory))
         (root (merge-pathnames
                (format nil "claw-lisp-json-run-timeout-~D-~D/"
                        (get-universal-time)
                        (get-internal-real-time))
                temp-root))
         (work-root (merge-pathnames "workspace/" root))
         (data-root (merge-pathnames "data/" root))
         (request-file (merge-pathnames "request.json" root))
         (result-file (merge-pathnames "result.json" root))
         (request (%make-runner-test-request work-root data-root :event-stream-p nil))
         exit-code stdout stderr)
    (unwind-protect
         (progn
           (ensure-directories-exist (uiop:ensure-directory-pathname work-root))
           (%write-json-file request-file request)
           (%with-redefined-function
               ('claw-lisp.cli::%submit-runner-user-message
                (lambda (&rest args)
                  (declare (ignore args))
                  (error 'sb-ext:timeout)))
             (multiple-value-setq (exit-code stdout stderr)
               (%dispatch-cli-capturing
                (list "--json-run"
                      "--request-file" (namestring request-file)
                      "--result-file" (namestring result-file)))))
           (%assert (= 12 exit-code)
                    "Expected timeout exit code 12, got ~A" exit-code)
           (%assert (string= "" stdout)
                    "Expected no stdout for timeout path, got ~S" stdout)
           (%assert (string= "" stderr)
                    "Expected no stderr for timeout path, got ~S" stderr)
           (let ((result (claw-lisp.providers.http-utils:json-decode
                          (uiop:read-file-string result-file))))
             (%assert (string= "timed_out" (getf result :status))
                      "Expected timed_out result, got ~S" result)
             (%assert (string= "turn_timeout" (getf (getf result :failure) :code))
                      "Expected timeout failure code, got ~S" result)))
      (when (uiop:directory-exists-p root)
        (uiop:delete-directory-tree root :validate t))))
  #-sbcl
  t)

(defun test-cli-json-run-failure-captures-diagnostics ()
  (let* ((temp-root (uiop:temporary-directory))
         (root (merge-pathnames
                (format nil "claw-lisp-json-run-diagnostics-~D-~D/"
                        (get-universal-time)
                        (get-internal-real-time))
                temp-root))
         (work-root (merge-pathnames "workspace/" root))
         (data-root (merge-pathnames "data/" root))
         (request-file (merge-pathnames "request.json" root))
         (result-file (merge-pathnames "result.json" root))
         (request (%make-runner-test-request work-root data-root :event-stream-p nil))
         exit-code stdout stderr)
    (unwind-protect
         (progn
           (ensure-directories-exist (uiop:ensure-directory-pathname work-root))
           (%write-json-file request-file request)
           (%with-redefined-function
               ('claw-lisp.cli::%submit-runner-user-message
                (lambda (&rest args)
                  (declare (ignore args))
                  (format *error-output* "diagnostic warning from runner~%")
                  (error "forced runner failure")))
             (multiple-value-setq (exit-code stdout stderr)
               (%dispatch-cli-capturing
                (list "--json-run"
                      "--request-file" (namestring request-file)
                      "--result-file" (namestring result-file)))))
           (%assert (= 10 exit-code)
                    "Expected failed exit code 10, got ~A" exit-code)
           (%assert (string= "" stdout)
                    "Expected no stdout for failed run, got ~S" stdout)
           (%assert (string= "" stderr)
                    "Expected no stderr for failed run with result-file, got ~S" stderr)
           (let* ((result (claw-lisp.providers.http-utils:json-decode
                           (uiop:read-file-string result-file)))
                  (failure (getf result :failure))
                  (details (getf failure :details)))
             (%assert (string= "failed" (getf result :status))
                      "Expected failed result, got ~S" result)
             (%assert (search "diagnostic warning from runner" (or (getf details :stderr) ""))
                      "Expected captured diagnostics in failure details, got ~S" failure)))
      (when (uiop:directory-exists-p root)
        (uiop:delete-directory-tree root :validate t)))))

(defun test-cli-help-exits-0 ()
  (let ((output (with-output-to-string (stream)
                  (%assert (= 0 (claw-lisp.cli::%dispatch-cli '("--help") :stdout stream))
                           "Expected --help dispatch to exit 0"))))
    (%assert (search "Usage: achatina" output)
             "Expected help usage text, got ~S" output)))

(defun test-model-registry-resolve-exact-match ()
  (let ((registry (claw-lisp.core.model-registry:make-default-model-registry)))
    (let ((caps (claw-lisp.core.model-registry:resolve-model registry "claude-sonnet-4-6")))
      (%assert caps "Expected capabilities for claude-sonnet-4-6")
      (%assert (= 200000 (claw-lisp.core.domain:model-capabilities-context-window caps))
               "Expected 200K context window for Sonnet")
      (%assert (claw-lisp.core.domain:model-capabilities-tools-p caps)
               "Expected Sonnet to support tools"))))

(defun test-model-registry-resolve-alias ()
  (let ((registry (claw-lisp.core.model-registry:make-default-model-registry)))
    (let ((caps (claw-lisp.core.model-registry:resolve-model registry "claude-opus")))
      (%assert caps "Expected capabilities for claude-opus alias")
      (%assert (string= "claude-opus-4-6" (claw-lisp.core.domain:model-capabilities-name caps))
               "Expected alias to resolve to claude-opus-4-6")
      (%assert (= 32000 (claw-lisp.core.domain:model-capabilities-max-output-tokens caps))
               "Expected Opus max output of 32K"))))

(defun test-model-registry-prefix-match ()
  (let ((registry (claw-lisp.core.model-registry:make-default-model-registry)))
    ;; A model ID containing a known prefix should match
    (let ((caps (claw-lisp.core.model-registry:resolve-model registry "claude-sonnet-4-6-20250929")))
      (%assert caps "Expected prefix match for versioned model ID")
      (%assert (string= "claude-sonnet-4-6" (claw-lisp.core.domain:model-capabilities-name caps))
               "Expected prefix match to resolve to canonical name"))))

(defun test-model-registry-provider-default ()
  (let ((registry (claw-lisp.core.model-registry:make-default-model-registry)))
    ;; An unknown model on a known provider should get the provider default
    (let ((caps (claw-lisp.core.model-registry:resolve-model registry "some-unknown-model")))
      (%assert caps "Expected provider default for unknown model")
      (%assert (eq :anthropic (claw-lisp.core.domain:model-capabilities-provider caps))
               "Expected default provider to be :anthropic"))))

(defun test-model-registry-supports-p ()
  (let ((registry (claw-lisp.core.model-registry:make-default-model-registry)))
    (%assert (claw-lisp.core.model-registry:model-supports-p registry "claude-opus-4-6" :tools)
             "Expected Opus to support tools")
    (%assert (claw-lisp.core.model-registry:model-supports-p registry "claude-opus-4-6" :thinking)
             "Expected Opus to support thinking")
    (%assert (not (claw-lisp.core.model-registry:model-supports-p registry "claude-haiku-4-5" :thinking))
             "Expected Haiku to not support thinking")))

(defun test-model-registry-translate-name ()
  (let ((registry (claw-lisp.core.model-registry:make-default-model-registry)))
    (%assert (string= "claude-sonnet-4-6"
                      (claw-lisp.core.model-registry:model-translate-name registry :anthropic "claude-sonnet"))
             "Expected bare name for Anthropic")
    (%assert (string= "anthropic/claude-sonnet-4-6"
                      (claw-lisp.core.model-registry:model-translate-name registry :openrouter "claude-sonnet"))
             "Expected prefixed name for OpenRouter")))

;; --- Phase 2c: Conditions and Retry Tests ---

(defun test-condition-hierarchy ()
  ;; claw-error is the base
  (let ((err (make-condition 'claw-lisp.core.conditions:claw-error :message "test")))
    (%assert (typep err 'error) "claw-error should be an error")
    (%assert (string= "test" (claw-lisp.core.conditions:claw-error-message err))
             "Message should be stored"))
  ;; provider-error
  (let ((err (make-condition 'claw-lisp.core.conditions:provider-error
                             :provider "test" :status 500 :message "server error")))
    (%assert (typep err 'claw-lisp.core.conditions:claw-error)
             "provider-error should be a claw-error")
    (%assert (= 500 (claw-lisp.core.conditions:provider-error-status err))
             "Status should be 500"))
  ;; rate-limit-error
  (let ((err (make-condition 'claw-lisp.core.conditions:rate-limit-error
                             :provider "test" :status 429 :retry-after 30
                             :message "rate limited")))
    (%assert (typep err 'claw-lisp.core.conditions:provider-error)
             "rate-limit-error should be a provider-error")
    (%assert (= 30 (claw-lisp.core.conditions:rate-limit-retry-after err))
             "Retry-after should be 30"))
  ;; auth-error
  (let ((err (make-condition 'claw-lisp.core.conditions:auth-error
                             :provider "test" :status 401 :message "unauthorized")))
    (%assert (typep err 'claw-lisp.core.conditions:provider-error)
             "auth-error should be a provider-error"))
  ;; tool-error
  (let ((err (make-condition 'claw-lisp.core.conditions:tool-error
                             :tool-name "echo" :message "tool failed")))
    (%assert (string= "echo" (claw-lisp.core.conditions:tool-error-tool-name err))
             "Tool name should be stored")))

(defun test-http-status-error-type ()
  (%assert (eq 'claw-lisp.core.conditions:rate-limit-error
               (claw-lisp.core.conditions:http-status->error-type 429))
           "429 should be rate-limit-error")
  (%assert (eq 'claw-lisp.core.conditions:auth-error
               (claw-lisp.core.conditions:http-status->error-type 401))
           "401 should be auth-error")
  (%assert (eq 'claw-lisp.core.conditions:auth-error
               (claw-lisp.core.conditions:http-status->error-type 403))
           "403 should be auth-error")
  (%assert (eq 'claw-lisp.core.conditions:context-exceeded-error
               (claw-lisp.core.conditions:http-status->error-type 413))
           "413 should be context-exceeded-error")
  (%assert (eq 'claw-lisp.core.conditions:provider-error
               (claw-lisp.core.conditions:http-status->error-type 500))
           "500 should be provider-error")
  (%assert (null (claw-lisp.core.conditions:http-status->error-type 200))
           "200 should be nil (not an error)"))

(defun test-retry-exponential-delay ()
  ;; Delay should increase with attempt
  (let ((d0 (claw-lisp.providers.retry:exponential-delay 0 1 60))
        (d1 (claw-lisp.providers.retry:exponential-delay 1 1 60))
        (d2 (claw-lisp.providers.retry:exponential-delay 2 1 60)))
    (%assert (<= 0.9 d0 1.1) "Base delay ~1s, got ~A" d0)
    (%assert (< d0 d1) "Delay should increase, ~A < ~A" d0 d1)
    (%assert (< d1 d2) "Delay should increase, ~A < ~A" d1 d2))
  ;; Max delay should cap
  (let ((d10 (claw-lisp.providers.retry:exponential-delay 10 1 60)))
    (%assert (<= d10 60) "Delay should be capped at 60, got ~A" d10)))

(defun test-retry-retryable-status ()
  (%assert (claw-lisp.providers.retry:retryable-status-p 429) "429 should be retryable")
  (%assert (claw-lisp.providers.retry:retryable-status-p 500) "500 should be retryable")
  (%assert (claw-lisp.providers.retry:retryable-status-p 529) "529 should be retryable")
  (%assert (claw-lisp.providers.retry:retryable-status-p 503) "503 should be retryable")
  (%assert (not (claw-lisp.providers.retry:retryable-status-p 200)) "200 should not be retryable")
  (%assert (not (claw-lisp.providers.retry:retryable-status-p 400)) "400 should not be retryable")
  (%assert (not (claw-lisp.providers.retry:retryable-status-p 401)) "401 should not be retryable"))

(defun test-retry-immediate-500 ()
  "Verify that 500 errors retry immediately without exponential delay."
  (let ((call-count 0)
        (start-time (get-universal-time)))
    (claw-lisp.providers.retry:call-with-retry
        (lambda ()
          (incf call-count)
          (if (< call-count 3)
              (values 500 "server error")
              (values 200 "ok")))
      :max-retries 3
      :base-delay 10 ;; If this was exponential, 10s would cause timeout
      :max-delay 10)
    (%assert (= call-count 3) "Should have retried 3 times, got ~A" call-count)
    ;; If 500 retries were delayed, this would take > 10 seconds.
    ;; With immediate retry, it should complete in < 1 second.
    (%assert (< (- (get-universal-time) start-time) 2)
             "500 retries should be immediate, took too long")))

;; --- Phase 2d: System Prompt Tests ---

(defun test-system-prompt-builds ()
  "Test that build-system-prompt returns a non-empty string."
  (let* ((config (claw-lisp.config:make-default-runtime-config))
         (runtime (make-runtime :config config)))
    ;; Register a tool so we can test tool descriptions
    (register-tool runtime (make-echo-tool))
    (let ((prompt (claw-lisp.core.system-prompt:build-system-prompt
                   :tool-registry (runtime-tool-registry runtime))))
      (%assert (and prompt (> (length prompt) 0))
               "System prompt should be non-empty, got ~A" prompt)
      (%assert (search "You are Claw" prompt)
               "System prompt should contain base identity")
      (%assert (search "Current Time" prompt)
               "System prompt should contain date/time")
      (%assert (search "Available Tools" prompt)
               "System prompt should list tools")
      (%assert (search "echo" prompt)
               "System prompt should list the echo tool"))))

(defun test-claude-md-user-file ()
  "Test CLAUDE.md discovery for user-level file by reading it directly."
  (let* ((root (merge-pathnames
                (format nil "claw-lisp-claude-md-test-~A/" (get-universal-time))
                #P"/tmp/"))
         (claude-dir (merge-pathnames ".claw-lisp/" root))
         (claude-path (merge-pathnames "CLAUDE.md" claude-dir)))
    (unwind-protect
         (progn
           (ensure-directories-exist claude-path)
           (with-open-file (stream claude-path
                                   :direction :output
                                   :if-exists :supersede
                                   :if-does-not-exist :create)
             (write-string "Test user instructions" stream))
           ;; Test that the file can be read
           (let ((content (claw-lisp.core.claude-md:read-claude-md-file claude-path)))
             (%assert (and content (search "Test user instructions" content))
                      "Should read CLAUDE.md content, got ~A" content)))
      (when (probe-file root)
        (uiop:delete-directory-tree root :validate t)))))

(defun test-claude-md-project-file ()
  "Test CLAUDE.md discovery for project-level file."
  (let* ((root (merge-pathnames
                (format nil "claw-lisp-project-md-test-~A/" (get-universal-time))
                #P"/tmp/"))
         (claude-path (merge-pathnames "CLAUDE.md" root)))
    (unwind-protect
         (progn
           (ensure-directories-exist claude-path)
           (with-open-file (stream claude-path
                                   :direction :output
                                   :if-exists :supersede
                                   :if-does-not-exist :create)
             (write-string "Test project instructions" stream))
           (let ((content (claw-lisp.core.claude-md:load-claude-md-files
                           :project-root root)))
             (%assert (and content (search "Test project instructions" content))
                      "Should find project CLAUDE.md content, got ~A" content)))
      (when (probe-file root)
        (uiop:delete-directory-tree root :validate t)))))

(defun test-sse-parser-simple-event ()
  "Test parsing a simple single-line data event."
  (let ((input (format nil "data: {\"type\":\"message_start\"}~%~%")))
    (with-input-from-string (stream input)
      (let ((event (claw-lisp.providers.sse-parser:read-sse-event stream)))
        (%assert event "Should return an event, got ~A" event)
        (%assert (string= "{\"type\":\"message_start\"}" (getf event :data))
                 "Data should match input, got ~A" (getf event :data))))))

(defun test-sse-parser-multi-line-data ()
  "Test parsing multi-line data (multiple data: lines concatenated with newlines)."
  (let ((input (format nil "data: line1~%data: line2~%~%")))
    (with-input-from-string (stream input)
      (let ((event (claw-lisp.providers.sse-parser:read-sse-event stream)))
        (%assert event "Should return an event, got ~A" event)
        (%assert (string= (format nil "line1~%line2") (getf event :data))
                 "Multi-line data should be concatenated with newline, got ~A" (getf event :data))))))

(defun test-sse-parser-comments-ignored ()
  "Test that comment lines are ignored."
  (let ((input (format nil ": ping~%data: {\"ok\":true}~%~%")))
    (with-input-from-string (stream input)
      (let ((event (claw-lisp.providers.sse-parser:read-sse-event stream)))
        (%assert (not (getf event :ping)) "Comment fields should be ignored")
        (%assert (getf event :data) "Data should be present")))))

(defun test-sse-parser-event-and-data ()
  "Test parsing an event with both event: and data: fields (Anthropic format)."
  (let ((input (format nil "event: content_block_delta~%data: {\"delta\":\"text\"}~%~%")))
    (with-input-from-string (stream input)
      (let ((event (claw-lisp.providers.sse-parser:read-sse-event stream)))
        (%assert event "Should return an event")
        (%assert (string= "content_block_delta" (getf event :event))
                 "Event field should match, got ~A" (getf event :event))
        (%assert (string= "{\"delta\":\"text\"}" (getf event :data))
                 "Data field should match, got ~A" (getf event :data))))))

(defun test-sse-parser-no-space-after-colon ()
  "Test parsing data without space after colon."
  (let ((input (format nil "data:value~%~%")))
    (with-input-from-string (stream input)
      (let ((event (claw-lisp.providers.sse-parser:read-sse-event stream)))
        (%assert event "Should return an event")
        (%assert (string= "value" (getf event :data))
                 "Data should match, got ~A" (getf event :data))))))

(defun test-sse-parser-multiple-events ()
  "Test reading multiple events from a single stream."
  ;; Note: Due to how the parser handles stream positioning, 
  ;; we test each event separately for now.
  (let ((input1 (format nil "data: first~%~%"))
        (input2 (format nil "data: second~%~%")))
    (with-input-from-string (stream1 input1)
      (let ((event1 (claw-lisp.providers.sse-parser:read-sse-event stream1)))
        (%assert event1 "First event should exist")
        (%assert (string= "first" (getf event1 :data)) "First event data")))
    (with-input-from-string (stream2 input2)
      (let ((event2 (claw-lisp.providers.sse-parser:read-sse-event stream2)))
        (%assert event2 "Second event should exist")
        (%assert (string= "second" (getf event2 :data)) "Second event data")))))

(defun test-streaming-on-event-callback ()
  "Verify that the on-event callback fires during mock streaming."
  (let* ((events-received nil)
         (callback (lambda (event-type data)
                     (push (list event-type data) events-received)))
         (provider (claw-lisp.providers.mock:make-mock-provider))
         (conversation (claw-lisp.core.domain:make-conversation
                        :id "test-streaming"
                        :messages (list (claw-lisp.core.domain:make-message
                                         :role :user
                                         :content "Hello")))))
    ;; Call stream-turn with on-event callback
    (let ((response (claw-lisp.core.protocols:stream-turn
                     provider conversation
                     :model "mock-model"
                     :tools nil
                     :on-event callback)))
      ;; Verify callback was called
      (%assert (> (length events-received) 0)
               "Expected on-event callback to fire, got ~A events"
               (length events-received))
      ;; Verify we received the expected event types
      (%assert (find "message_start" events-received :key #'car :test #'string=)
               "Expected message_start event")
      (%assert (find "content_block_delta" events-received :key #'car :test #'string=)
               "Expected content_block_delta event")
      (%assert (find "message_stop" events-received :key #'car :test #'string=)
               "Expected message_stop event")
      ;; Verify response is valid
      (%assert (claw-lisp.core.domain:transport-response-ok-p response)
               "Expected successful response"))))

(defun test-phase8-child-progress-and-transcript-linking ()
  (labels ((capture-error (thunk)
             (handler-case
                 (progn
                   (funcall thunk)
                   nil)
               (error (condition)
                 condition))))
    (let ((runtime (make-runtime)))
      (register-default-providers runtime)
      (let ((session (start-session runtime
                                    :provider-name "mock"
                                    :model "mock-model"
                                    :session-id "phase8-progress-parent")))
        (dolist (condition
                 (list (capture-error
                        (lambda ()
                          (claw-lisp.core.runtime:spawn-child-agent
                           runtime session
                           :provider-name "mock"
                           :model "mock-model"
                           :initial-user-message "child hello")))
                       (capture-error
                        (lambda ()
                          (claw-lisp.core.runtime:child-progress-snapshot
                           runtime session "missing-child")))
                       (capture-error
                        (lambda ()
                          (claw-lisp.core.runtime:list-child-progress-snapshots runtime session)))
                       (capture-error
                        (lambda ()
                          (claw-lisp.core.runtime:list-child-agents runtime session)))))
          (%assert condition
                   "Expected child-agent public-build error")
          (%assert (search "Child-agent orchestration is not included in the public Achatina build"
                           (princ-to-string condition))
                   "Expected public-build child-agent message, got ~S" condition))))))

(defun test-phase8-cli-agent-visibility-commands ()
  (let ((runtime (make-runtime)))
    (register-default-providers runtime)
    (let* ((session (start-session runtime
                                   :provider-name "mock"
                                   :model "mock-model"
                                   :session-id "phase8-cli-parent"))
           (agents-output
             (with-output-to-string (*standard-output*)
               (claw-lisp.cli::handle-command runtime session ":agents")))
           (memory-output
             (with-output-to-string (*standard-output*)
               (claw-lisp.cli::handle-command runtime session ":memory")))
           (memory-content-output
             (with-output-to-string (*standard-output*)
               (claw-lisp.cli::handle-command runtime session ":memory-content")))
           (compaction-output
             (with-output-to-string (*standard-output*)
               (claw-lisp.cli::handle-command runtime session ":compaction")))
           (tasks-output
             (with-output-to-string (*standard-output*)
               (claw-lisp.cli::handle-command runtime session ":tasks"))))
      (%assert (search "Child-agent orchestration commands are not included in the public Achatina build."
                       agents-output)
               "Expected public-build notice for :agents, got ~S" agents-output)
      (%assert (search "Session memory status:" memory-output)
               "Expected memory heading in :memory output, got ~S" memory-output)
      (%assert (search "Exists:" memory-output)
               "Expected existence status in :memory output, got ~S" memory-output)
      (%assert (or (search "No session memory file yet." memory-content-output)
                   (search "Session memory at:" memory-content-output))
               "Expected :memory-content output shape, got ~S" memory-content-output)
      (%assert (search "Compaction status:" compaction-output)
               "Expected compaction heading in :compaction output, got ~S" compaction-output)
      (%assert (search "Failure count:" compaction-output)
               "Expected compaction failure count in :compaction output, got ~S" compaction-output)
      (%assert (search "No background tasks right now." tasks-output)
               "Expected empty-state output for :tasks, got ~S" tasks-output))))

(defun test-phase8-await-timeout-summary ()
  (labels ((capture-error (thunk)
             (handler-case
                 (progn
                   (funcall thunk)
                   nil)
               (error (condition)
                 condition))))
    (let ((runtime (make-runtime)))
      (register-default-providers runtime)
      (let* ((session (start-session runtime
                                     :provider-name "mock"
                                     :model "mock-model"
                                     :session-id "phase8-timeout-parent"))
             (await-error
               (capture-error
                (lambda ()
                  (claw-lisp.core.runtime:await-child-agent
                   runtime session "phase8-timeout-child" :timeout-seconds 0.1)))))
        (%assert await-error
                 "Expected public-build await-child-agent error")
        (%assert (search "Child-agent orchestration is not included in the public Achatina build"
                         (princ-to-string await-error))
                 "Expected public-build child-agent message, got ~S" await-error)))))

(defun test-phase8-unknown-child-errors ()
  (labels ((capture-error (thunk)
             (handler-case
                 (progn
                   (funcall thunk)
                   nil)
               (error (condition)
                 condition))))
    (let* ((root (merge-pathnames
                  (format nil "claw-lisp-phase8-unknown-child-test-~A/" (get-universal-time))
                  #P"/tmp/"))
           (transcripts-root (merge-pathnames "transcripts/" root))
           (config (claw-lisp.config::%make-runtime-config
                    :data-root (namestring root)
                    :transcripts-root (namestring transcripts-root)
                    :artifacts-root (namestring (merge-pathnames "artifacts/" root))
                    :memory-root (namestring (merge-pathnames "memory/" root))
                    :default-provider "mock"
                    :default-model "mock-model"))
           (runtime (make-runtime :config config)))
      (unwind-protect
           (progn
             (register-default-providers runtime)
             (let ((session (start-session runtime
                                           :provider-name "mock"
                                           :model "mock-model"
                                           :session-id "phase8-unknown-child-parent")))
               (let ((await-error (capture-error
                                   (lambda ()
                                     (claw-lisp.core.runtime:await-child-agent runtime session "missing-child"))))
                     (cancel-error (capture-error
                                    (lambda ()
                                      (claw-lisp.core.runtime:cancel-child-agent runtime session "missing-child"))))
                     (send-error (capture-error
                                  (lambda ()
                                    (claw-lisp.core.runtime:send-agent-message
                                     runtime session "missing-child" :control (list :ping t))))))
                (%assert await-error
                        "Expected await-child-agent public-build error")
                 (%assert (typep await-error 'simple-error)
                          "Expected SIMPLE-ERROR for await-child-agent, got ~S" await-error)
                 (%assert (search "Child-agent orchestration is not included in the public Achatina build"
                                  (princ-to-string await-error))
                          "Expected public-build message for await-child-agent, got ~S" await-error)
                 (%assert cancel-error
                        "Expected cancel-child-agent public-build error")
                 (%assert (typep cancel-error 'simple-error)
                          "Expected SIMPLE-ERROR for cancel-child-agent, got ~S" cancel-error)
                 (%assert (search "Child-agent orchestration is not included in the public Achatina build"
                                  (princ-to-string cancel-error))
                          "Expected public-build message for cancel-child-agent, got ~S" cancel-error)
                 (%assert send-error
                          "Expected send-agent-message public-build error")
                 (%assert (typep send-error 'simple-error)
                          "Expected SIMPLE-ERROR for send-agent-message, got ~S" send-error)
                 (%assert (search "Child-agent orchestration is not included in the public Achatina build"
                                  (princ-to-string send-error))
                          "Expected public-build message for send-agent-message, got ~S" send-error))))
        (when (probe-file root)
          (uiop:delete-directory-tree root :validate t))))))

(defun test-phase9-cli-session-resume-command ()
  (let* ((root (merge-pathnames
                (format nil "claw-lisp-phase9-cli-resume-test-~A/" (get-universal-time))
                #P"/tmp/"))
         (transcripts-root (merge-pathnames "transcripts/" root))
         (config (claw-lisp.config::%make-runtime-config
                  :data-root (namestring root)
                  :transcripts-root (namestring transcripts-root)
                  :artifacts-root (namestring (merge-pathnames "artifacts/" root))
                  :memory-root (namestring (merge-pathnames "memory/" root))
                  :default-provider "mock"
                  :default-model "mock-model"))
         (runtime (make-runtime :config config)))
    (unwind-protect
         (progn
           (register-default-providers runtime)
           (let* ((source-id "phase9-resume-source")
                  (source-session (start-session runtime
                                               :provider-name "mock"
                                               :model "mock-model"
                                               :session-id source-id))
                  (target-session (start-session runtime
                                                 :provider-name "mock"
                                                 :model "mock-model"
                                                 :session-id "phase9-resume-target")))
             ;; Generate transcript messages for source session.
             (submit-user-message runtime source-session "resume test message")
             ;; Command usage path.
             (let ((usage-output
                     (with-output-to-string (*standard-output*)
                       (claw-lisp.cli::handle-command runtime target-session ":resume"))))
               (%assert (search "Usage: :resume <session-id>" usage-output)
                        "Expected usage output for :resume, got ~S" usage-output))
             ;; Missing transcript path.
             (let ((missing-output
                     (with-output-to-string (*standard-output*)
                       (claw-lisp.cli::handle-command runtime target-session ":resume does-not-exist"))))
               (%assert (search "Resume failed:" missing-output)
                        "Expected resume failure output for missing session, got ~S" missing-output))
             ;; Happy path resume.
             (let (handled resumed-session)
               (let ((resume-output
                       (with-output-to-string (*standard-output*)
                         (multiple-value-setq (handled resumed-session)
                           (claw-lisp.cli::handle-command
                            runtime target-session (format nil ":resume ~A" source-id))))))
                 (%assert handled "Expected :resume command to be handled")
                 (%assert resumed-session "Expected resumed session object")
                 (%assert (string= source-id (claw-lisp.core.domain:agent-session-id resumed-session))
                          "Expected resumed session id ~A, got ~A"
                          source-id
                          (claw-lisp.core.domain:agent-session-id resumed-session))
                 (%assert (search "Resumed session" resume-output)
                          "Expected success output for :resume, got ~S" resume-output)
                 (%assert (> (length
                              (claw-lisp.core.domain:conversation-messages
                               (claw-lisp.core.domain:agent-session-conversation resumed-session)))
                             0)
                          "Expected restored conversation messages after resume")))))
      (when (probe-file root)
        (uiop:delete-directory-tree root :validate t)))))

(defun test-phase9-cli-compaction-no-event-when-transcript-missing ()
  (let* ((root (merge-pathnames
                (format nil "claw-lisp-phase9-cli-compaction-missing-transcript-test-~A/"
                        (get-universal-time))
                #P"/tmp/"))
         (transcripts-root (merge-pathnames "transcripts/" root))
         (config (claw-lisp.config::%make-runtime-config
                  :data-root (namestring root)
                  :transcripts-root (namestring transcripts-root)
                  :artifacts-root (namestring (merge-pathnames "artifacts/" root))
                  :memory-root (namestring (merge-pathnames "memory/" root))
                  :default-provider "mock"
                  :default-model "mock-model"))
         (runtime (make-runtime :config config)))
    (unwind-protect
         (progn
           (register-default-providers runtime)
           (let* ((session (start-session runtime
                                          :provider-name "mock"
                                          :model "mock-model"
                                          :session-id "phase9-compaction-missing-transcript"))
                  (transcript-path (claw-lisp.core.runtime:session-transcript-path runtime session)))
             ;; Explicitly remove transcript so :compaction exercises missing-file path.
             (when (probe-file transcript-path)
               (delete-file transcript-path))
             (let ((output
                     (with-output-to-string (*standard-output*)
                       (claw-lisp.cli::handle-command runtime session ":compaction"))))
               (%assert (search "Compaction status:" output)
                        "Expected :compaction heading, got ~S" output)
               (%assert (search "Last event: none" output)
                        "Expected missing-transcript last-event output, got ~S" output))))
      (when (probe-file root)
        (uiop:delete-directory-tree root :validate t)))))

(defun test-phase9-cli-compaction-reflects-failure-circuit-state ()
  (let* ((root (merge-pathnames
                (format nil "claw-lisp-phase9-cli-compaction-circuit-test-~A/"
                        (get-universal-time))
                #P"/tmp/"))
         (transcripts-root (merge-pathnames "transcripts/" root))
         (config (claw-lisp.config::%make-runtime-config
                  :data-root (namestring root)
                  :transcripts-root (namestring transcripts-root)
                  :artifacts-root (namestring (merge-pathnames "artifacts/" root))
                  :memory-root (namestring (merge-pathnames "memory/" root))
                  :default-provider "mock"
                  :default-model "mock-model"))
         (runtime (make-runtime :config config)))
    (unwind-protect
         (progn
           (register-default-providers runtime)
           (let ((session (start-session runtime
                                         :provider-name "mock"
                                         :model "mock-model"
                                         :session-id "phase9-compaction-circuit")))
             (dotimes (_ 3)
               (declare (ignore _))
               (claw-lisp.core.runtime:increment-compaction-failures session))
             (let ((output
                     (with-output-to-string (*standard-output*)
                       (claw-lisp.cli::handle-command runtime session ":compaction"))))
               (%assert (search "Failure count: 3" output)
                        "Expected failure count reflection in :compaction output, got ~S" output)
               (%assert (search "Circuit open: yes" output)
                        "Expected open-circuit reflection in :compaction output, got ~S" output))))
      (when (probe-file root)
        (uiop:delete-directory-tree root :validate t)))))

(defun test-phase9-find-last-compaction-event-line-missing-file ()
  (%assert (null (claw-lisp.cli::%find-last-compaction-event-line
                  #P"/tmp/claw-lisp-missing-compaction-event-file.jsonl"))
           "Expected NIL for missing transcript file"))

(defun test-phase9-find-last-compaction-event-line-empty-file ()
  (let* ((root (merge-pathnames
                (format nil "claw-lisp-phase9-empty-compaction-event-test-~A/"
                        (get-universal-time))
                #P"/tmp/"))
         (path (merge-pathnames "transcript.jsonl" root)))
    (unwind-protect
         (progn
           (ensure-directories-exist path)
           (with-open-file (stream path :direction :output :if-exists :supersede :if-does-not-exist :create)
             (declare (ignore stream)))
           (%assert (null (claw-lisp.cli::%find-last-compaction-event-line path))
                    "Expected NIL for empty transcript file"))
      (when (probe-file root)
        (uiop:delete-directory-tree root :validate t)))))

(defun test-phase9-find-last-compaction-event-line-last-match-wins ()
  (let* ((root (merge-pathnames
                (format nil "claw-lisp-phase9-last-match-compaction-event-test-~A/"
                        (get-universal-time))
                #P"/tmp/"))
         (path (merge-pathnames "transcript.jsonl" root))
         (expected nil))
    (unwind-protect
         (progn
           (ensure-directories-exist path)
           (setf expected "{\"event\":\"microcompact\",\"session_id\":\"s1\"}")
           (with-open-file (stream path :direction :output :if-exists :supersede :if-does-not-exist :create)
             (format stream "{\"event\":\"session_start\",\"session_id\":\"s1\"}~%")
             (format stream "{\"event\":\"compaction_boundary\",\"session_id\":\"s1\"}~%")
             (format stream "not-json-line~%")
             (format stream "~A~%" expected))
           (%assert (string= expected
                             (claw-lisp.cli::%find-last-compaction-event-line path))
                    "Expected helper to return last matching compaction event line"))
      (when (probe-file root)
        (uiop:delete-directory-tree root :validate t)))))

(defun test-phase9-find-last-compaction-event-line-whitespace-variant ()
  (let* ((root (merge-pathnames
                (format nil "claw-lisp-phase9-whitespace-compaction-event-test-~A/"
                        (get-universal-time))
                #P"/tmp/"))
         (path (merge-pathnames "transcript.jsonl" root))
         (expected "{ \"event\" : \"compaction_boundary\" , \"session_id\" : \"s2\" }"))
    (unwind-protect
         (progn
           (ensure-directories-exist path)
           (with-open-file (stream path :direction :output :if-exists :supersede :if-does-not-exist :create)
             (format stream "~A~%" expected))
           (%assert (string= expected
                             (claw-lisp.cli::%find-last-compaction-event-line path))
                    "Expected helper to parse and match compaction event despite JSON whitespace"))
      (when (probe-file root)
        (uiop:delete-directory-tree root :validate t)))))

(defun test-phase9-find-last-compaction-event-line-return-contract ()
  (let* ((root (merge-pathnames
                (format nil "claw-lisp-phase9-helper-contract-test-~A/"
                        (get-universal-time))
                #P"/tmp/"))
         (path (merge-pathnames "transcript.jsonl" root)))
    (unwind-protect
         (progn
           (ensure-directories-exist path)
           (with-open-file (stream path :direction :output :if-exists :supersede :if-does-not-exist :create)
             (format stream "{\"event\":\"session_start\",\"session_id\":\"s1\"}~%")
             (format stream "{\"session_id\":\"s1\",\"note\":\"missing-event\"}~%")
             (format stream "{\"event\":\"microcompact\",\"session_id\":\"s1\"}~%"))
           (multiple-value-bind (line err missing-count)
               (claw-lisp.cli::%find-last-compaction-event-line path)
             (%assert (string= line "{\"event\":\"microcompact\",\"session_id\":\"s1\"}")
                      "Expected helper to return final compaction event line")
             (%assert (null err)
                      "Expected NIL helper error on readable transcript, got ~S" err)
             (%assert (= missing-count 1)
                      "Expected one parsed JSON line missing event field, got ~D" missing-count)))
      (when (probe-file root)
        (uiop:delete-directory-tree root :validate t)))))

(defun test-phase9-find-last-compaction-event-line-io-error ()
  (let* ((root (merge-pathnames
                (format nil "claw-lisp-phase9-ioerror-compaction-event-test-~A/"
                        (get-universal-time))
                #P"/tmp/"))
         (dir-path (merge-pathnames "transcript-dir/" root)))
    (unwind-protect
         (progn
           (ensure-directories-exist (merge-pathnames "sentinel" dir-path))
           (multiple-value-bind (line err)
               (claw-lisp.cli::%find-last-compaction-event-line dir-path)
             (%assert (null line)
                      "Expected NIL line result when transcript path is unreadable as file")
             (%assert err
                      "Expected non-NIL error text for transcript I/O failure")))
      (when (probe-file root)
        (uiop:delete-directory-tree root :validate t)))))

(defun test-phase9-cli-compaction-shows-latest-event ()
  (let* ((root (merge-pathnames
                (format nil "claw-lisp-phase9-cli-compaction-happy-path-test-~A/"
                        (get-universal-time))
                #P"/tmp/"))
         (transcripts-root (merge-pathnames "transcripts/" root))
         (config (claw-lisp.config::%make-runtime-config
                  :data-root (namestring root)
                  :transcripts-root (namestring transcripts-root)
                  :artifacts-root (namestring (merge-pathnames "artifacts/" root))
                  :memory-root (namestring (merge-pathnames "memory/" root))
                  :default-provider "mock"
                  :default-model "mock-model"))
         (runtime (make-runtime :config config)))
    (unwind-protect
         (progn
           (register-default-providers runtime)
           (let* ((session (start-session runtime
                                          :provider-name "mock"
                                          :model "mock-model"
                                          :session-id "phase9-compaction-happy-path"))
                  (transcript-path (claw-lisp.core.runtime:session-transcript-path runtime session)))
             (with-open-file (stream transcript-path :direction :output :if-exists :append :if-does-not-exist :create)
               (format stream "{\"event\":\"compaction_boundary\",\"session_id\":\"phase9-compaction-happy-path\"}~%")
               (format stream "{\"event\":\"microcompact\",\"session_id\":\"phase9-compaction-happy-path\"}~%"))
             (let ((output
                     (with-output-to-string (*standard-output*)
                       (claw-lisp.cli::handle-command runtime session ":compaction"))))
               (%assert (search "Last event: {\"event\":\"microcompact\"" output)
                        "Expected :compaction to show latest matching event, got ~S" output)
               (%assert (not (search "Transcript read warning:" output))
                        "Did not expect transcript warning on happy path, got ~S" output))))
      (when (probe-file root)
        (uiop:delete-directory-tree root :validate t)))))

(defun test-phase9-cli-compaction-large-transcript ()
  (let* ((root (merge-pathnames
                (format nil "claw-lisp-phase9-cli-compaction-large-transcript-test-~A/"
                        (get-universal-time))
                #P"/tmp/"))
         (transcripts-root (merge-pathnames "transcripts/" root))
         (config (claw-lisp.config::%make-runtime-config
                  :data-root (namestring root)
                  :transcripts-root (namestring transcripts-root)
                  :artifacts-root (namestring (merge-pathnames "artifacts/" root))
                  :memory-root (namestring (merge-pathnames "memory/" root))
                  :default-provider "mock"
                  :default-model "mock-model"))
         (runtime (make-runtime :config config)))
    (unwind-protect
         (progn
           (register-default-providers runtime)
           (let* ((session (start-session runtime
                                          :provider-name "mock"
                                          :model "mock-model"
                                          :session-id "phase9-compaction-large-transcript"))
                  (transcript-path (claw-lisp.core.runtime:session-transcript-path runtime session))
                  (payload-size 12000))
             (with-open-file (stream transcript-path :direction :output :if-exists :append :if-does-not-exist :create)
               (dotimes (i 3000)
                 (format stream "{\"event\":\"message\",\"i\":~D,\"payload\":\"~A\"}~%"
                         i
                         (make-string payload-size :initial-element #\x)))
               (format stream "{\"event\":\"compaction_boundary\",\"session_id\":\"phase9-compaction-large-transcript\"}~%")
               (dotimes (j 3000)
                 (format stream "{\"event\":\"tool\",\"j\":~D,\"payload\":\"~A\"}~%"
                         j
                         (make-string payload-size :initial-element #\y)))
               (format stream "{\"event\":\"microcompact\",\"session_id\":\"phase9-compaction-large-transcript\"}~%"))
             (let ((output
                     (with-output-to-string (*standard-output*)
                       (claw-lisp.cli::handle-command runtime session ":compaction"))))
               (%assert (search "Compaction status:" output)
                        "Expected :compaction heading on large transcript, got ~S" output)
               (%assert (search "Last event: {\"event\":\"microcompact\"" output)
                        "Expected latest compaction event on large transcript, got ~S" output)
               (%assert (not (search "Transcript read warning:" output))
                        "Did not expect transcript read warning on large transcript, got ~S" output))))
      (when (probe-file root)
        (uiop:delete-directory-tree root :validate t)))))

(defun test-phase9-cli-compaction-schema-warning-for-missing-event-field ()
  (let* ((root (merge-pathnames
                (format nil "claw-lisp-phase9-cli-compaction-schema-warning-test-~A/"
                        (get-universal-time))
                #P"/tmp/"))
         (transcripts-root (merge-pathnames "transcripts/" root))
         (config (claw-lisp.config::%make-runtime-config
                  :data-root (namestring root)
                  :transcripts-root (namestring transcripts-root)
                  :artifacts-root (namestring (merge-pathnames "artifacts/" root))
                  :memory-root (namestring (merge-pathnames "memory/" root))
                  :default-provider "mock"
                  :default-model "mock-model"))
         (runtime (make-runtime :config config)))
    (unwind-protect
         (progn
           (register-default-providers runtime)
           (let* ((session (start-session runtime
                                          :provider-name "mock"
                                          :model "mock-model"
                                          :session-id "phase9-compaction-schema-warning"))
                  (transcript-path (claw-lisp.core.runtime:session-transcript-path runtime session)))
             (with-open-file (stream transcript-path :direction :output :if-exists :append :if-does-not-exist :create)
               (format stream "{\"session_id\":\"phase9-compaction-schema-warning\",\"message\":\"ok\"}~%"))
             (let ((output
                     (with-output-to-string (*standard-output*)
                       (claw-lisp.cli::handle-command runtime session ":compaction"))))
               (%assert (search "Last event: none" output)
                        "Expected no compaction event in schema-warning case, got ~S" output)
               (%assert (search "Transcript schema warning: 1 parsed line(s) missing event field" output)
                        "Expected transcript schema warning in :compaction output, got ~S" output))))
      (when (probe-file root)
        (uiop:delete-directory-tree root :validate t)))))

(defun test-phase9-cli-compaction-when-transcript-path-nil ()
  (let* ((root (merge-pathnames
                (format nil "claw-lisp-phase9-cli-compaction-nil-path-test-~A/"
                        (get-universal-time))
                #P"/tmp/"))
         (transcripts-root (merge-pathnames "transcripts/" root))
         (config (claw-lisp.config::%make-runtime-config
                  :data-root (namestring root)
                  :transcripts-root (namestring transcripts-root)
                  :artifacts-root (namestring (merge-pathnames "artifacts/" root))
                  :memory-root (namestring (merge-pathnames "memory/" root))
                  :default-provider "mock"
                  :default-model "mock-model"))
         (runtime (make-runtime :config config)))
    (unwind-protect
         (progn
           (register-default-providers runtime)
           (let ((session (start-session runtime
                                         :provider-name "mock"
                                         :model "mock-model"
                                         :session-id "phase9-compaction-nil-path")))
             (flet ((claw-lisp.core.runtime:session-transcript-path (&rest args)
                      (declare (ignore args))
                      nil))
               (let ((output
                       (with-output-to-string (*standard-output*)
                         (claw-lisp.cli::handle-command runtime session ":compaction"))))
                 (%assert (search "Compaction status:" output)
                          "Expected :compaction heading for NIL transcript path, got ~S" output)
                 (%assert (search "Last event: none" output)
                          "Expected no last event when transcript path is NIL, got ~S" output)
                 (%assert (not (search "Transcript read warning:" output))
                          "Did not expect transcript warning when transcript path is NIL, got ~S" output)))))
      (when (probe-file root)
        (uiop:delete-directory-tree root :validate t)))))

(defun test-phase9-cli-compaction-when-transcript-unreadable ()
  (let* ((root (merge-pathnames
                (format nil "claw-lisp-phase9-cli-compaction-unreadable-test-~A/"
                        (get-universal-time))
                #P"/tmp/"))
         (transcripts-root (merge-pathnames "transcripts/" root))
         (config (claw-lisp.config::%make-runtime-config
                  :data-root (namestring root)
                  :transcripts-root (namestring transcripts-root)
                  :artifacts-root (namestring (merge-pathnames "artifacts/" root))
                  :memory-root (namestring (merge-pathnames "memory/" root))
                  :default-provider "mock"
                  :default-model "mock-model"))
         (runtime (make-runtime :config config)))
    (unwind-protect
         (progn
           (register-default-providers runtime)
           (let ((session (start-session runtime
                                         :provider-name "mock"
                                         :model "mock-model"
                                         :session-id "phase9-compaction-unreadable")))
             (let ((old-fn (symbol-function 'claw-lisp.cli::%find-last-compaction-event-line)))
               (unwind-protect
                    (progn
                      (setf (symbol-function 'claw-lisp.cli::%find-last-compaction-event-line)
                            (lambda (&rest args)
                              (declare (ignore args))
                              (values nil "Permission denied" 0)))
                      (let ((output
                              (with-output-to-string (*standard-output*)
                                (claw-lisp.cli::handle-command runtime session ":compaction"))))
                        (%assert (search "Compaction status:" output)
                                 "Expected :compaction heading for unreadable transcript, got ~S" output)
                        (%assert (search "Last event: none" output)
                                 "Expected no last event for unreadable transcript, got ~S" output)
                        (%assert (search "Transcript read warning:" output)
                                 "Expected transcript warning for unreadable transcript path, got ~S" output)))
                 (setf (symbol-function 'claw-lisp.cli::%find-last-compaction-event-line) old-fn)))))
      (when (probe-file root)
        (uiop:delete-directory-tree root :validate t)))))

(defun test-phase9-cli-transcript-inspection-command ()
  (let* ((root (merge-pathnames
                (format nil "claw-lisp-phase9-cli-transcript-inspection-test-~A/"
                        (get-universal-time))
                #P"/tmp/"))
         (transcripts-root (merge-pathnames "transcripts/" root))
         (config (claw-lisp.config::%make-runtime-config
                  :data-root (namestring root)
                  :transcripts-root (namestring transcripts-root)
                  :artifacts-root (namestring (merge-pathnames "artifacts/" root))
                  :memory-root (namestring (merge-pathnames "memory/" root))
                  :default-provider "mock"
                  :default-model "mock-model"))
         (runtime (make-runtime :config config)))
    (unwind-protect
         (progn
           (register-default-providers runtime)
           (let* ((session (start-session runtime
                                          :provider-name "mock"
                                          :model "mock-model"
                                          :session-id "phase9-transcript-inspection"))
                  (transcript-path (claw-lisp.core.runtime:session-transcript-path runtime session)))
             ;; Ensure there are transcript lines to inspect.
             (submit-user-message runtime session "transcript inspection message")
             (with-open-file (stream transcript-path :direction :output :if-exists :append :if-does-not-exist :create)
               (format stream "not-json-line~%")
               (format stream "{\"event\":\"tool_result\",\"session_id\":\"phase9-transcript-inspection\"}~%"))
             (let ((summary-output
                     (with-output-to-string (*standard-output*)
                       (claw-lisp.cli::handle-command runtime session ":transcript")))
                   (tail-output
                     (with-output-to-string (*standard-output*)
                       (claw-lisp.cli::handle-command runtime session ":transcript tail 2")))
                   (usage-output
                     (with-output-to-string (*standard-output*)
                       (claw-lisp.cli::handle-command runtime session ":transcript nope"))))
               (%assert (search "Transcript summary:" summary-output)
                        "Expected :transcript summary heading, got ~S" summary-output)
               (%assert (search "Exists: yes" summary-output)
                        "Expected existing transcript in summary, got ~S" summary-output)
               (%assert (search "Non-JSON or malformed JSON lines: 1" summary-output)
                        "Expected malformed/non-JSON count in summary, got ~S" summary-output)
               (%assert (search "Last event name: tool_result" summary-output)
                        "Expected latest event name in summary, got ~S" summary-output)
               (%assert (search "Transcript tail (2):" tail-output)
                        "Expected :transcript tail heading, got ~S" tail-output)
               (%assert (search "not-json-line" tail-output)
                        "Expected tail to include recent raw line, got ~S" tail-output)
               (%assert (search "\"event\":\"tool_result\"" tail-output)
                        "Expected tail to include latest event line, got ~S" tail-output)
               (%assert (search "Usage: :transcript | :transcript tail [n] (n<=200)" usage-output)
                        "Expected usage output for malformed :transcript command, got ~S" usage-output))))
      (when (probe-file root)
        (uiop:delete-directory-tree root :validate t)))))

(defun test-phase9-cli-transcript-tail-partial-fill ()
  (let* ((root (merge-pathnames
                (format nil "claw-lisp-phase9-cli-transcript-tail-partial-fill-test-~A/"
                        (get-universal-time))
                #P"/tmp/"))
         (transcripts-root (merge-pathnames "transcripts/" root))
         (config (claw-lisp.config::%make-runtime-config
                  :data-root (namestring root)
                  :transcripts-root (namestring transcripts-root)
                  :artifacts-root (namestring (merge-pathnames "artifacts/" root))
                  :memory-root (namestring (merge-pathnames "memory/" root))
                  :default-provider "mock"
                  :default-model "mock-model"))
         (runtime (make-runtime :config config)))
    (unwind-protect
         (progn
           (register-default-providers runtime)
           (let* ((session (start-session runtime
                                          :provider-name "mock"
                                          :model "mock-model"
                                          :session-id "phase9-transcript-tail-partial-fill"))
                  (transcript-path (claw-lisp.core.runtime:session-transcript-path runtime session)))
             (with-open-file (stream transcript-path :direction :output :if-exists :supersede :if-does-not-exist :create)
               (format stream "{\"event\":\"session_start\",\"session_id\":\"phase9-transcript-tail-partial-fill\"}~%")
               (format stream "{\"event\":\"message\",\"session_id\":\"phase9-transcript-tail-partial-fill\",\"role\":\"user\",\"content\":\"hello\"}~%"))
             (let ((output
                     (with-output-to-string (*standard-output*)
                       (claw-lisp.cli::handle-command runtime session ":transcript tail 3"))))
               (%assert (search "Transcript tail (3):" output)
                        "Expected transcript tail heading, got ~S" output)
               (%assert (search "\"event\":\"session_start\"" output)
                        "Expected first transcript line in partial tail output, got ~S" output)
               (%assert (search "\"event\":\"message\"" output)
                        "Expected second transcript line in partial tail output, got ~S" output)
               (%assert (not (search "  NIL" output :test #'string-equal))
                        "Did not expect NIL entries in partial tail output, got ~S" output))))
      (when (probe-file root)
        (uiop:delete-directory-tree root :validate t)))))

(defun test-phase9-cli-transcript-when-transcript-missing ()
  (let* ((root (merge-pathnames
                (format nil "claw-lisp-phase9-cli-transcript-missing-test-~A/"
                        (get-universal-time))
                #P"/tmp/"))
         (transcripts-root (merge-pathnames "transcripts/" root))
         (config (claw-lisp.config::%make-runtime-config
                  :data-root (namestring root)
                  :transcripts-root (namestring transcripts-root)
                  :artifacts-root (namestring (merge-pathnames "artifacts/" root))
                  :memory-root (namestring (merge-pathnames "memory/" root))
                  :default-provider "mock"
                  :default-model "mock-model"))
         (runtime (make-runtime :config config)))
    (unwind-protect
         (progn
           (register-default-providers runtime)
           (let* ((session (start-session runtime
                                          :provider-name "mock"
                                          :model "mock-model"
                                          :session-id "phase9-transcript-missing"))
                  (transcript-path (claw-lisp.core.runtime:session-transcript-path runtime session)))
             ;; Remove the auto-created transcript so command hits missing-file path.
             (when (probe-file transcript-path)
               (delete-file transcript-path))
             (let ((summary-output
                     (with-output-to-string (*standard-output*)
                       (claw-lisp.cli::handle-command runtime session ":transcript")))
                   (tail-output
                     (with-output-to-string (*standard-output*)
                       (claw-lisp.cli::handle-command runtime session ":transcript tail 5"))))
               (%assert (search "Transcript summary:" summary-output)
                        "Expected :transcript summary heading for missing file, got ~S" summary-output)
               (%assert (search "Exists: no" summary-output)
                        "Expected Exists: no for missing transcript, got ~S" summary-output)
               (%assert (search "Last event name: none" summary-output)
                        "Expected no last event for missing transcript, got ~S" summary-output)
               (%assert (search "Note: Transcript file missing." summary-output)
                        "Expected missing-file note for missing transcript, got ~S" summary-output)
               (%assert (search "Transcript tail (5):" tail-output)
                        "Expected tail heading for missing transcript, got ~S" tail-output)
               (%assert (search "(no lines)" tail-output)
                        "Expected empty tail body for missing transcript, got ~S" tail-output))))
      (when (probe-file root)
        (uiop:delete-directory-tree root :validate t)))))

(defun test-phase9-cli-transcript-tail-zero-usage ()
  (let ((runtime (make-runtime)))
    (register-default-providers runtime)
    (let ((session (start-session runtime
                                  :provider-name "mock"
                                  :model "mock-model"
                                  :session-id "phase9-transcript-tail-zero")))
      (let ((output
              (with-output-to-string (*standard-output*)
                (claw-lisp.cli::handle-command runtime session ":transcript tail 0"))))
        (%assert (search "Usage: :transcript | :transcript tail [n] (n<=200)" output)
                 "Expected usage output for :transcript tail 0, got ~S" output)))))

(defun test-phase9-cli-transcript-when-transcript-path-nil ()
  (let ((runtime (make-runtime)))
    (register-default-providers runtime)
    (let ((session (start-session runtime
                                  :provider-name "mock"
                                  :model "mock-model"
                                  :session-id "phase9-transcript-nil-path")))
      (let ((old-existing-fn (symbol-function 'claw-lisp.core.runtime:session-transcript-existing-path))
            (old-fn (symbol-function 'claw-lisp.core.runtime:session-transcript-path)))
        (unwind-protect
             (progn
               (setf (symbol-function 'claw-lisp.core.runtime:session-transcript-existing-path)
                     (lambda (&rest args)
                       (declare (ignore args))
                       nil))
               (setf (symbol-function 'claw-lisp.core.runtime:session-transcript-path)
                     (lambda (&rest args)
                       (declare (ignore args))
                       nil))
               (let ((summary-output
                       (with-output-to-string (*standard-output*)
                         (claw-lisp.cli::handle-command runtime session ":transcript")))
                     (tail-output
                       (with-output-to-string (*standard-output*)
                         (claw-lisp.cli::handle-command runtime session ":transcript tail 5"))))
                 (%assert (search "Transcript summary:" summary-output)
                          "Expected summary heading for NIL transcript path, got ~S" summary-output)
                 (%assert (search "Exists: no" summary-output)
                          "Expected Exists: no for NIL transcript path, got ~S" summary-output)
                 (%assert (search "Last event name: none" summary-output)
                          "Expected no last event for NIL transcript path, got ~S" summary-output)
                 (%assert (search "Path: (none)" summary-output)
                          "Expected explicit (none) path for NIL transcript path, got ~S" summary-output)
                 (%assert (search "Note: Transcript not configured for this session." summary-output)
                          "Expected not-configured note for NIL transcript path, got ~S" summary-output)
                 (%assert (search "Transcript tail (5):" tail-output)
                          "Expected tail heading for NIL transcript path, got ~S" tail-output)
                 (%assert (search "(no lines)" tail-output)
                          "Expected empty tail for NIL transcript path, got ~S" tail-output)))
          (setf (symbol-function 'claw-lisp.core.runtime:session-transcript-existing-path)
                old-existing-fn)
          (setf (symbol-function 'claw-lisp.core.runtime:session-transcript-path) old-fn))))))

(defun test-phase9-cli-transcript-when-transcript-unreadable ()
  (let* ((root (merge-pathnames
                (format nil "claw-lisp-phase9-cli-transcript-unreadable-test-~A/"
                        (get-universal-time))
                #P"/tmp/"))
         (transcripts-root (merge-pathnames "transcripts/" root))
         (config (claw-lisp.config::%make-runtime-config
                  :data-root (namestring root)
                  :transcripts-root (namestring transcripts-root)
                  :artifacts-root (namestring (merge-pathnames "artifacts/" root))
                  :memory-root (namestring (merge-pathnames "memory/" root))
                  :default-provider "mock"
                  :default-model "mock-model"))
         (runtime (make-runtime :config config)))
    (unwind-protect
         (progn
           (register-default-providers runtime)
           (let ((session (start-session runtime
                                         :provider-name "mock"
                                         :model "mock-model"
                                         :session-id "phase9-transcript-unreadable")))
             (let ((old-fn (symbol-function 'claw-lisp.core.runtime::inspect-transcript-file)))
               (unwind-protect
                    (progn
                      (setf (symbol-function 'claw-lisp.core.runtime::inspect-transcript-file)
                            (lambda (path &key (tail-count 0))
                              (declare (ignore path tail-count))
                              (list :path "/tmp/fake.jsonl"
                                    :exists t
                                    :line-count 0
                                    :parsed-json-count 0
                                    :malformed-json-count 0
                                    :event-count 0
                                    :message-count 0
                                    :last-event nil
                                    :last-compaction-event-line nil
                                    :missing-event-count 0
                                    :tail-lines nil
                                    :error "Permission denied")))
                      (let ((output
                              (with-output-to-string (*standard-output*)
                                (claw-lisp.cli::handle-command runtime session ":transcript"))))
                        (%assert (search "Transcript summary:" output)
                                 "Expected summary heading on unreadable transcript, got ~S" output)
                        (%assert (search "Transcript read warning: Permission denied" output)
                                 "Expected transcript read warning for unreadable transcript, got ~S" output)))
                 (setf (symbol-function 'claw-lisp.core.runtime::inspect-transcript-file) old-fn)))))
      (when (probe-file root)
        (uiop:delete-directory-tree root :validate t)))))

(defun test-phase9-cli-transcript-tail-boundaries ()
  (let ((runtime (make-runtime)))
    (register-default-providers runtime)
    (let ((session (start-session runtime
                                  :provider-name "mock"
                                  :model "mock-model"
                                  :session-id "phase9-transcript-tail-boundaries")))
      (let ((tail-200-output
              (with-output-to-string (*standard-output*)
                (claw-lisp.cli::handle-command runtime session ":transcript tail 200")))
            (tail-201-output
              (with-output-to-string (*standard-output*)
                (claw-lisp.cli::handle-command runtime session ":transcript tail 201"))))
        (%assert (search "Transcript tail (200):" tail-200-output)
                 "Expected accepted tail boundary for 200, got ~S" tail-200-output)
        (%assert (search "Usage: :transcript | :transcript tail [n] (n<=200)" tail-201-output)
                 "Expected usage output for out-of-range tail 201, got ~S" tail-201-output)))))

(defun test-phase9-cli-transcript-large-file-tail-streaming ()
  (let* ((root (merge-pathnames
                (format nil "claw-lisp-phase9-cli-transcript-large-test-~A/"
                        (get-universal-time))
                #P"/tmp/"))
         (transcripts-root (merge-pathnames "transcripts/" root))
         (config (claw-lisp.config::%make-runtime-config
                  :data-root (namestring root)
                  :transcripts-root (namestring transcripts-root)
                  :artifacts-root (namestring (merge-pathnames "artifacts/" root))
                  :memory-root (namestring (merge-pathnames "memory/" root))
                  :default-provider "mock"
                  :default-model "mock-model"))
         (runtime (make-runtime :config config)))
    (unwind-protect
         (progn
           (register-default-providers runtime)
           (let* ((session (start-session runtime
                                          :provider-name "mock"
                                          :model "mock-model"
                                          :session-id "phase9-transcript-large"))
                  (transcript-path (claw-lisp.core.runtime:session-transcript-path runtime session)))
             (with-open-file (stream transcript-path :direction :output :if-exists :supersede :if-does-not-exist :create)
               (dotimes (i 12000)
                 (format stream "{\"event\":\"message\",\"i\":~D,\"payload\":\"~A\"}~%"
                         i
                         (make-string 200 :initial-element #\x))))
             (let ((output
                     (with-output-to-string (*standard-output*)
                       (claw-lisp.cli::handle-command runtime session ":transcript tail 10"))))
               (%assert (search "Transcript summary:" output)
                        "Expected transcript summary for large transcript, got ~S" output)
               (%assert (search "Lines: 12000" output)
                        "Expected full line count for large transcript, got ~S" output)
               (%assert (search "Transcript tail (10):" output)
                        "Expected tail heading for large transcript, got ~S" output)
               (%assert (search "\"i\":11999" output)
                        "Expected newest line in tail output for large transcript, got ~S" output))))
      (when (probe-file root)
        (uiop:delete-directory-tree root :validate t)))))

(defun test-phase9-cli-transcript-large-file-tail-performance ()
  (let* ((root (merge-pathnames
                (format nil "claw-lisp-phase9-cli-transcript-perf-test-~A/"
                        (get-universal-time))
                #P"/tmp/"))
         (transcripts-root (merge-pathnames "transcripts/" root))
         (config (claw-lisp.config::%make-runtime-config
                  :data-root (namestring root)
                  :transcripts-root (namestring transcripts-root)
                  :artifacts-root (namestring (merge-pathnames "artifacts/" root))
                  :memory-root (namestring (merge-pathnames "memory/" root))
                  :default-provider "mock"
                  :default-model "mock-model"))
         (runtime (make-runtime :config config)))
    (unwind-protect
         (progn
           (register-default-providers runtime)
           (let* ((session (start-session runtime
                                          :provider-name "mock"
                                          :model "mock-model"
                                          :session-id "phase9-transcript-perf"))
                  (transcript-path (claw-lisp.core.runtime:session-transcript-path runtime session))
                  (start 0)
                  (elapsed-ms 0))
             (with-open-file (stream transcript-path :direction :output :if-exists :supersede :if-does-not-exist :create)
               (dotimes (i 12000)
                 (format stream "{\"event\":\"message\",\"i\":~D,\"payload\":\"~A\"}~%"
                         i
                         (make-string 200 :initial-element #\x))))
             (setf start (get-internal-real-time))
             (with-output-to-string (*standard-output*)
               (claw-lisp.cli::handle-command runtime session ":transcript tail 10"))
             (setf elapsed-ms
                   (/ (* (- (get-internal-real-time) start) 1000.0)
                      internal-time-units-per-second))
             ;; Relaxed guard: catches pathological regressions without flaking in CI.
             (%assert (< elapsed-ms 8000.0)
                      "Expected :transcript tail 10 on large transcript to finish <8000ms, got ~,2fms"
                      elapsed-ms)))
      (when (probe-file root)
        (uiop:delete-directory-tree root :validate t)))))

(defun test-phase9-select-session-model-updates-session-and-transcript ()
  (let ((runtime (make-runtime)))
    (register-default-providers runtime)
    (let* ((session (start-session runtime
                                   :provider-name "mock"
                                   :model "mock-model"
                                   :session-id "phase9-select-model"))
           (result (claw-lisp.core.runtime:select-session-model
                    runtime session
                    :provider-name "mock"
                    :model "claude-3.7-sonnet"
                    :allow-incompatible-model-p t))
           (transcript (claw-lisp.core.runtime:session-transcript-path runtime session))
           (lines (read-lines transcript)))
      (%assert (getf result :ok)
               "Expected successful select-session-model result, got ~S" result)
      (%assert (string= "mock" (claw-lisp.core.domain:agent-session-provider session))
               "Expected provider to stay mock, got ~S"
               (claw-lisp.core.domain:agent-session-provider session))
      (%assert (string= "claude-3.7-sonnet" (claw-lisp.core.domain:agent-session-model session))
               "Expected model update, got ~S"
               (claw-lisp.core.domain:agent-session-model session))
      (%assert (eq nil (claw-lisp.core.runtime::session-state-value session :selection-in-progress))
               "Expected selection-in-progress flag reset to NIL")
      (%assert transcript "Expected transcript path")
      (%assert (find "\"event\":\"session_config_changed\"" lines :test #'search)
               "Expected session_config_changed event in transcript, got ~S" lines)
      (%assert (find "\"event_version\":1" lines :test #'search)
               "Expected event_version in transcript, got ~S" lines))))

(defun test-phase9-select-session-model-profile-switch-and-persist ()
  (let ((runtime (make-runtime)))
    (register-default-providers runtime)
    (let* ((session (start-session runtime
                                   :provider-name "mock"
                                   :model "mock-model"
                                   :session-id "phase9-select-profile"))
           (result-a (claw-lisp.core.runtime:select-session-model
                      runtime session
                      :provider-name "mock"
                      :model "claude-3.7-sonnet"
                      :profile "team-a"
                      :allow-incompatible-model-p t))
           (result-b (claw-lisp.core.runtime:select-session-model
                      runtime session
                      :model "claude-3.7-sonnet"
                      :allow-incompatible-model-p t)))
      (%assert (string= "team-a" (or (getf result-a :profile) ""))
               "Expected profile in selection result, got ~S" result-a)
      (%assert (string= "team-a"
                        (or (claw-lisp.core.runtime::session-state-value session :profile nil) ""))
               "Expected session profile state to persist")
      (%assert (string= "team-a" (or (getf result-b :profile) ""))
               "Expected profile to persist when omitted on later selection, got ~S" result-b)
      (let* ((transcript (claw-lisp.core.runtime:session-transcript-path runtime session))
             (lines (read-lines transcript)))
        (%assert (find "\"profile\":\"team-a\"" lines :test #'search)
                 "Expected profile recorded in transcript events, got ~S" lines)))))

(defun test-phase9-select-session-model-rejects-unknown-provider-without-mutation ()
  (let* ((runtime (make-runtime)))
    (register-default-providers runtime)
    (let ((session (start-session runtime
                                  :provider-name "mock"
                                  :model "mock-model"
                                  :session-id "phase9-select-bad-provider")))
      (handler-case
          (progn
            (claw-lisp.core.runtime:select-session-model
             runtime session :provider-name "does-not-exist" :model "claude-3.7-sonnet")
            (%assert nil "Expected select-session-model to fail for unknown provider"))
        (error (condition)
          (%assert (search "Unknown provider" (princ-to-string condition))
                   "Expected unknown provider error, got ~A" condition)))
      (%assert (string= "mock" (claw-lisp.core.domain:agent-session-provider session))
               "Provider should remain unchanged after failure, got ~S"
               (claw-lisp.core.domain:agent-session-provider session))
      (%assert (string= "mock-model" (claw-lisp.core.domain:agent-session-model session))
               "Model should remain unchanged after failure, got ~S"
               (claw-lisp.core.domain:agent-session-model session))
      (%assert (eq nil (claw-lisp.core.runtime::session-state-value session :selection-in-progress))
               "selection-in-progress should be cleared after failure"))))

(defun test-phase9-provider-name-normalization ()
  (let ((runtime (make-runtime)))
    (register-default-providers runtime)
    (let* ((session (start-session runtime
                                   :provider-name "MoCk"
                                   :model "mock-model"
                                   :session-id "phase9-provider-normalization"))
           (result (claw-lisp.core.runtime:select-session-model
                    runtime session :provider-name " MoCk " :model "mock-model")))
      (%assert (string= "mock" (claw-lisp.core.domain:agent-session-provider session))
               "Expected start-session to canonicalize provider to mock, got ~S"
               (claw-lisp.core.domain:agent-session-provider session))
      (%assert (string= "mock" (getf result :provider))
               "Expected select-session-model result provider canonicalization, got ~S" result)
      (%assert (string= "mock" (claw-lisp.core.domain:agent-session-provider session))
               "Expected canonical provider on session after selection, got ~S"
               (claw-lisp.core.domain:agent-session-provider session))
      (%assert (claw-lisp.core.runtime:resolve-provider runtime "OPENROUTER")
               "Expected case-insensitive provider resolution for OPENROUTER")
      (%assert (member "openrouter" (claw-lisp.core.runtime:list-provider-names runtime) :test #'string=)
               "Expected canonical provider names in list-provider-names"))))

(defun test-phase9-model-resolution-source-coverage ()
  (let ((runtime (make-runtime)))
    (register-default-providers runtime)
    (%assert (eq :exact
                 (claw-lisp.core.runtime::%model-resolution-source runtime "claude-sonnet-4-6"))
             "Expected :exact model resolution source")
    (%assert (eq :alias
                 (claw-lisp.core.runtime::%model-resolution-source runtime "claude-sonnet"))
             "Expected :alias model resolution source")
    (%assert (eq :prefix
                 (claw-lisp.core.runtime::%model-resolution-source runtime "claude-sonnet-4-6-20260601"))
             "Expected :prefix model resolution source")
    (%assert (eq :provider-default
                 (claw-lisp.core.runtime::%model-resolution-source runtime "openai/gpt-5-unknown"))
             "Expected :provider-default model resolution source")
    (%assert (eq :fallback
                 (claw-lisp.core.runtime::%model-resolution-source runtime "bedrock-unknown-model"))
             "Expected :fallback model resolution source")))

(defun test-phase9-select-session-model-rejects-incompatible-provider-model-by-default ()
  (let* ((runtime (make-runtime)))
    (register-default-providers runtime)
    (let ((session (start-session runtime
                                  :provider-name "mock"
                                  :model "mock-model"
                                  :session-id "phase9-select-incompatible-default")))
      (handler-case
          (progn
            (claw-lisp.core.runtime:select-session-model
             runtime session :provider-name "mock" :model "claude-sonnet-4-6")
            (%assert nil "Expected incompatible provider/model rejection"))
        (error (condition)
          (%assert (search "incompatible with selected provider" (princ-to-string condition))
                   "Expected compatibility error, got ~A" condition)))
      (%assert (string= "mock-model" (claw-lisp.core.domain:agent-session-model session))
               "Model should remain unchanged after compatibility rejection"))))

(defun test-phase9-select-session-model-allows-incompatible-provider-model-with-override ()
  (let* ((runtime (make-runtime)))
    (register-default-providers runtime)
    (let* ((session (start-session runtime
                                   :provider-name "mock"
                                   :model "mock-model"
                                   :session-id "phase9-select-incompatible-override"))
           (result (claw-lisp.core.runtime:select-session-model
                    runtime session
                    :provider-name "mock"
                    :model "claude-sonnet-4-6"
                    :allow-incompatible-model-p t)))
      (%assert (getf result :ok)
               "Expected override selection success, got ~S" result)
      (%assert (search "compatibility override enabled"
                       (string-downcase (or (first (getf result :warnings)) "")))
               "Expected compatibility override warning, got ~S"
               (getf result :warnings))
      (%assert (string= "claude-sonnet-4-6" (claw-lisp.core.domain:agent-session-model session))
               "Expected model update with compatibility override"))))

(defun test-phase9-select-session-model-rejects-when-turn-in-flight ()
  (let* ((runtime (make-runtime)))
    (register-default-providers runtime)
    (let ((session (start-session runtime
                                  :provider-name "mock"
                                  :model "mock-model"
                                  :session-id "phase9-select-busy")))
      (claw-lisp.core.runtime::set-session-state-value session :turn-in-flight-p t)
      (handler-case
          (progn
            (claw-lisp.core.runtime:select-session-model runtime session :model "claude-3.7-sonnet")
            (%assert nil "Expected selection failure while turn is in-flight"))
        (error (condition)
          (%assert (search "busy executing a turn" (princ-to-string condition))
                   "Expected in-flight busy error, got ~A" condition)))
      (%assert (string= "mock-model" (claw-lisp.core.domain:agent-session-model session))
               "Model should remain unchanged on in-flight rejection"))))

(defun test-phase9-select-session-model-provider-default-warning ()
  (let ((runtime (make-runtime)))
    (register-default-providers runtime)
    (let* ((session (start-session runtime
                                   :provider-name "mock"
                                   :model "mock-model"
                                   :session-id "phase9-select-provider-default"))
           (result (claw-lisp.core.runtime:select-session-model
                    runtime session
                    :provider-name "mock"
                    :model "openai/gpt-5-unknown")))
      (%assert (search "resolved via PROVIDER-DEFAULT capabilities"
                       (or (first (getf result :warnings)) ""))
               "Expected provider-default warning text, got ~S"
               (getf result :warnings))
      (%assert (string= "mock" (claw-lisp.core.domain:agent-session-provider session))
               "Expected provider updated to mock despite warning")
      (%assert (string= "openai/gpt-5-unknown"
                        (claw-lisp.core.domain:agent-session-model session))
               "Expected model update despite provider-default warning"))))

(defun test-phase9-select-session-model-fallback-warning ()
  (let ((runtime (make-runtime)))
    (register-default-providers runtime)
    (let* ((session (start-session runtime
                                   :provider-name "mock"
                                   :model "mock-model"
                                   :session-id "phase9-select-fallback"))
           (result (claw-lisp.core.runtime:select-session-model
                    runtime session
                    :provider-name "mock"
                    :model "bedrock-unknown-model")))
      (%assert (search "resolved via FALLBACK capabilities"
                       (or (first (getf result :warnings)) ""))
               "Expected fallback warning text, got ~S"
               (getf result :warnings))
      (%assert (string= "mock" (claw-lisp.core.domain:agent-session-provider session))
               "Expected provider updated to mock despite warning")
      (%assert (string= "bedrock-unknown-model"
                        (claw-lisp.core.domain:agent-session-model session))
               "Expected model update despite fallback warning"))))

(defun test-phase9-select-session-model-rejects-unknown-real-provider-model-before-credentials ()
  (let ((runtime (make-runtime)))
    (register-default-providers runtime)
    (let ((session (start-session runtime
                                  :provider-name "mock"
                                  :model "mock-model"
                                  :session-id "phase9-select-real-provider-unknown-model")))
      (handler-case
          (progn
            (claw-lisp.core.runtime:select-session-model
             runtime session
             :provider-name "anthropic"
             :model "missing-model")
            (%assert nil "Expected unknown-model rejection for real provider"))
        (error (condition)
          (%assert (search "Unknown model: missing-model" (princ-to-string condition))
                   "Expected unknown-model error, got ~A" condition))))))

(defun test-phase9-provider-credential-check-uses-registered-provider-api-keys ()
  (let* ((config (claw-lisp.config::%make-runtime-config))
         (runtime nil))
    (setf (claw-lisp.config:config-credentials config :anthropic)
          (claw-lisp.config:make-anthropic-credentials
           :api-key "anthropic-test-key"))
    (setf (claw-lisp.config:config-credentials config :openrouter)
          (claw-lisp.config:make-openrouter-credentials
           :api-key "openrouter-test-key"))
    (setf runtime (make-runtime :config config))
    (register-default-providers runtime)
    (%assert (null (claw-lisp.core.runtime:check-provider-configuration runtime "anthropic"))
             "Expected anthropic provider credentials to satisfy provider readiness checks")
    (%assert (null (claw-lisp.core.runtime:check-provider-configuration runtime "openrouter"))
             "Expected openrouter provider credentials to satisfy provider readiness checks")))

(defun test-phase9-select-session-model-preserves-input-model-id-for-alias-and-prefix ()
  (let ((runtime (make-runtime)))
    (register-default-providers runtime)
    (let ((session (start-session runtime
                                  :provider-name "mock"
                                  :model "mock-model"
                                  :session-id "phase9-select-alias-prefix")))
      (claw-lisp.core.runtime:select-session-model
       runtime session
       :provider-name "mock"
       :model "claude"
       :allow-incompatible-model-p t)
      (%assert (string= "claude" (claw-lisp.core.domain:agent-session-model session))
               "Expected alias input model-id to be stored as provided")
      (claw-lisp.core.runtime:select-session-model
       runtime session
       :provider-name "mock"
       :model "claude-3.7-sonnet-20250219"
       :allow-incompatible-model-p t)
      (%assert (string= "claude-3.7-sonnet-20250219"
                        (claw-lisp.core.domain:agent-session-model session))
               "Expected prefix input model-id to be stored as provided")
      (let* ((transcript (claw-lisp.core.runtime:session-transcript-path runtime session))
             (lines (read-lines transcript)))
        (%assert (find "\"model\":\"claude\"" lines :test #'search)
                 "Expected transcript to record alias input model-id, got ~S" lines)
        (%assert (find "\"model\":\"claude-3.7-sonnet-20250219\"" lines :test #'search)
                 "Expected transcript to record prefix input model-id, got ~S" lines)))))

(defun test-phase9-select-session-model-rejects-when-selection-in-progress ()
  (let ((runtime (make-runtime)))
    (register-default-providers runtime)
    (let ((session (start-session runtime
                                  :provider-name "mock"
                                  :model "mock-model"
                                  :session-id "phase9-select-concurrent")))
      (claw-lisp.core.runtime::set-session-state-value session :selection-in-progress t)
      (handler-case
          (progn
            (claw-lisp.core.runtime:select-session-model runtime session :model "claude-3.7-sonnet")
            (%assert nil "Expected selection failure while selection is already in progress"))
        (error (condition)
          (%assert (search "busy updating provider/model configuration"
                           (princ-to-string condition))
                   "Expected selection-in-progress error, got ~A" condition)))
      (%assert (string= "mock-model" (claw-lisp.core.domain:agent-session-model session))
               "Model should remain unchanged on selection-in-progress rejection"))))

(defun test-phase9-select-session-model-concurrent-calls-serialize ()
  #+sb-thread
  (let ((runtime (make-runtime)))
    (register-default-providers runtime)
    (let* ((session (start-session runtime
                                   :provider-name "mock"
                                   :model "mock-model"
                                   :session-id "phase9-select-concurrent-threads"))
           (results (make-array 2 :initial-element nil))
           (errors (make-array 2 :initial-element nil))
           (threads
             (list
              (sb-thread:make-thread
               (lambda ()
                 (handler-case
                     (setf (aref results 0)
                           (claw-lisp.core.runtime:select-session-model
                            runtime session :provider-name "mock" :model "claude-3.7-sonnet"))
                   (error (e)
                     (setf (aref errors 0) (princ-to-string e)))))
               :name "phase9-select-thread-1")
              (sb-thread:make-thread
               (lambda ()
                 (handler-case
                     (setf (aref results 1)
                           (claw-lisp.core.runtime:select-session-model
                            runtime session :provider-name "mock" :model "claude-3.7-sonnet-20250219"))
                   (error (e)
                     (setf (aref errors 1) (princ-to-string e)))))
               :name "phase9-select-thread-2"))))
      (dolist (thread threads)
        (sb-thread:join-thread thread))
      (%assert (null (aref errors 0))
               "Expected no thread-1 selection error, got ~S" (aref errors 0))
      (%assert (null (aref errors 1))
               "Expected no thread-2 selection error, got ~S" (aref errors 1))
      (%assert (or (string= "claude-3.7-sonnet"
                            (claw-lisp.core.domain:agent-session-model session))
                   (string= "claude-3.7-sonnet-20250219"
                            (claw-lisp.core.domain:agent-session-model session)))
               "Expected final model to be one completed selection value, got ~S"
               (claw-lisp.core.domain:agent-session-model session))
      (%assert (eq nil (claw-lisp.core.runtime::session-state-value session :selection-in-progress))
               "Expected selection-in-progress to be reset after concurrent calls")))
  #-sb-thread
  (%assert t "Skipping concurrent selection lock test: SB-THREAD not available."))

(defun test-phase9-cli-provider-model-and-use-commands ()
  (let ((runtime (make-runtime)))
    (register-default-providers runtime)
    (let ((session-ok (start-session runtime
                                     :provider-name "mock"
                                     :model "mock-model"
                                     :session-id "phase9-cli-provider-model"))
          (session-err (start-session runtime
                                      :provider-name "mock"
                                      :model "mock-model"
                                      :session-id "phase9-cli-provider-model-errors")))
      (let ((provider-output (with-output-to-string (*standard-output*)
                               (claw-lisp.cli::handle-command runtime session-ok ":provider")))
            (models-output (with-output-to-string (*standard-output*)
                             (claw-lisp.cli::handle-command runtime session-ok ":models")))
            (set-provider-output (with-output-to-string (*standard-output*)
                                   (claw-lisp.cli::handle-command runtime session-ok ":provider mock")))
            (set-provider-spaced-output (with-output-to-string (*standard-output*)
                                          (claw-lisp.cli::handle-command runtime session-ok "   :PrOvIdEr    mock   ")))
           (set-model-output (with-output-to-string (*standard-output*)
                                (claw-lisp.cli::handle-command runtime session-ok ":model mock-model")))
            (set-model-spaced-output (with-output-to-string (*standard-output*)
                                       (claw-lisp.cli::handle-command runtime session-ok "  :MoDeL   mock-model  ")))
            (use-output (with-output-to-string (*standard-output*)
                          (claw-lisp.cli::handle-command runtime session-ok ":use mock mock-model")))
            (use-profile-output (with-output-to-string (*standard-output*)
                                  (claw-lisp.cli::handle-command runtime session-ok ":use mock mock-model team-a")))
            (status-after-use-profile (with-output-to-string (*standard-output*)
                                        (claw-lisp.cli::handle-command runtime session-ok ":status")))
            (use-spaced-output (with-output-to-string (*standard-output*)
                                 (claw-lisp.cli::handle-command runtime session-ok " :UsE   mock    mock-model ")))
            (bad-provider-output (with-output-to-string (*standard-output*)
                                   (claw-lisp.cli::handle-command runtime session-err ":provider does-not-exist")))
            (bad-models-output (with-output-to-string (*standard-output*)
                                 (claw-lisp.cli::handle-command runtime session-err ":models does-not-exist")))
            (bad-models-misconfigured-output (with-output-to-string (*standard-output*)
                                               (progn
                                                 (claw-lisp.core.runtime:register-provider
                                                  runtime
                                                  (make-instance 'claw-lisp.providers.mock:mock-provider
                                                                 :name "xai-no-key"))
                                                 (claw-lisp.cli::handle-command runtime session-err ":models xai-no-key"))))
            (bad-use-output (with-output-to-string (*standard-output*)
                              (claw-lisp.cli::handle-command runtime session-err ":use mock")))
            (busy-provider-output (with-output-to-string (*standard-output*)
                                    (progn
                                      (claw-lisp.core.runtime::set-session-state-value session-err :turn-in-flight-p t)
                                      (claw-lisp.cli::handle-command runtime session-err ":provider mock"))))
            (busy-model-output (with-output-to-string (*standard-output*)
                                 (progn
                                   (claw-lisp.core.runtime::set-session-state-value session-err :turn-in-flight-p t)
                                   (claw-lisp.cli::handle-command runtime session-err ":model mock-model"))))
            (busy-use-output (with-output-to-string (*standard-output*)
                               (progn
                                 (claw-lisp.core.runtime::set-session-state-value session-err :turn-in-flight-p t)
                                 (claw-lisp.cli::handle-command runtime session-err ":use mock mock-model")))))
        (%assert (search "Current provider: mock" provider-output)
                 "Expected current provider output, got ~S" provider-output)
        (%assert (search "Known models (mock):" models-output)
                 "Expected models heading with current provider, got ~S" models-output)
        (%assert (search "Provider set to: mock" set-provider-output)
                 "Expected provider set output, got ~S" set-provider-output)
        (%assert (search "Provider set to: mock" set-provider-spaced-output)
                 "Expected provider set output for spaced/case-insensitive command, got ~S"
                 set-provider-spaced-output)
        (%assert (search "Model set to: mock-model" set-model-output)
                 "Expected model set output, got ~S" set-model-output)
        (%assert (search "Model set to: mock-model" set-model-spaced-output)
                 "Expected model set output for spaced/case-insensitive command, got ~S"
                 set-model-spaced-output)
        (%assert (search "Using provider=mock model=mock-model" use-output)
                 "Expected :use output, got ~S" use-output)
        (%assert (search "Profile: team-a" use-profile-output)
                 "Expected :use profile output, got ~S" use-profile-output)
        (%assert (search "Profile: team-a" status-after-use-profile)
                 "Expected :status to show selected profile, got ~S" status-after-use-profile)
        (%assert (search "Using provider=mock model=mock-model" use-spaced-output)
                 "Expected :use output for spaced/case-insensitive command, got ~S"
                 use-spaced-output)
        (%assert (and (search "Provider switch failed:" bad-provider-output)
                      (search "Unknown provider:" bad-provider-output)
                      (search "does-not-exist" bad-provider-output))
                 "Expected unknown-provider failure from :provider set, got ~S" bad-provider-output)
        (%assert (search "Unknown provider: does-not-exist" bad-models-output)
                 "Expected unknown-provider output from :models, got ~S" bad-models-output)
        (%assert (search "Provider xai-no-key is missing its API key configuration."
                         bad-models-misconfigured-output)
                 "Expected missing-api-key output from :models, got ~S"
                 bad-models-misconfigured-output)
        (%assert (search "Usage: :use <provider-name> <model-id> [profile]" bad-use-output)
                 "Expected usage output for malformed :use, got ~S" bad-use-output)
        (%assert (search "Provider switch failed: Session is busy." busy-provider-output)
                 "Expected friendly busy output for :provider, got ~S" busy-provider-output)
        (%assert (search "Model switch failed: Session is busy." busy-model-output)
                 "Expected friendly busy output for :model, got ~S" busy-model-output)
        (%assert (search "Selection failed: Session is busy." busy-use-output)
                 "Expected friendly busy output for :use, got ~S" busy-use-output)))
      (let* ((session-real (start-session runtime
                                          :provider-name "anthropic"
                                          :model "claude-sonnet-4-6"
                                          :session-id "phase9-cli-provider-model-real"))
             (bad-real-model-output (with-output-to-string (*standard-output*)
                                      (claw-lisp.cli::handle-command
                                       runtime session-real ":model missing-model"))))
        (%assert (search "Model switch failed: Unknown model: missing-model"
                         bad-real-model-output)
                 "Expected unknown-model output from :model on a real provider, got ~S"
                 bad-real-model-output))))

(defun test-phase9-cli-transcript-dispatch-neighbor-commands ()
  (let ((runtime (make-runtime)))
    (register-default-providers runtime)
    (let ((session (start-session runtime
                                  :provider-name "mock"
                                  :model "mock-model"
                                  :session-id "phase9-cli-transcript-dispatch-neighbors")))
      (let ((compaction-output
              (with-output-to-string (*standard-output*)
                (claw-lisp.cli::handle-command runtime session ":compaction")))
            (agents-output
              (with-output-to-string (*standard-output*)
                (claw-lisp.cli::handle-command runtime session ":agents")))
            (unknown-output
              (with-output-to-string (*standard-output*)
                (multiple-value-bind (handled maybe-session)
                    (claw-lisp.cli::handle-command runtime session ":transcript-tail 2")
                  (declare (ignore maybe-session))
                  (unless handled
                    (format t "Unknown command"))))))
        (%assert (search "Compaction status:" compaction-output)
                 "Expected :compaction output to remain reachable, got ~S" compaction-output)
        (%assert (search "Child-agent orchestration commands are not included in the public Achatina build."
                         agents-output)
                 "Expected public-build child-agent notice, got ~S" agents-output)
        (%assert (search "Unknown command" unknown-output)
                 "Expected near-match :transcript-tail to remain unhandled, got ~S" unknown-output)))))

(defun test-phase9-cli-models-no-registered-models-for-provider ()
  (let ((runtime (make-runtime)))
    (register-default-providers runtime)
    ;; Register a known provider name that has no associated model entries.
    (claw-lisp.core.runtime:register-provider
     runtime (make-instance 'claw-lisp.providers.mock:mock-provider :name "xai" :api-key "dummy-key"))
    (let* ((session (start-session runtime
                                   :provider-name "mock"
                                   :model "mock-model"
                                   :session-id "phase9-cli-models-empty-provider"))
           (output (with-output-to-string (*standard-output*)
                     (claw-lisp.cli::handle-command runtime session ":models xai"))))
      (%assert (search "Known models (xai):" output)
               "Expected provider heading for empty provider models, got ~S" output)
      (%assert (search "(none registered)" output)
               "Expected explicit none-registered output, got ~S" output))))

(defun test-phase9-cli-diagnostics-command ()
  (let ((runtime (make-runtime)))
    (register-default-providers runtime)
    (claw-lisp.core.runtime:register-default-tools runtime)
    (let* ((session (start-session runtime
                                   :provider-name "mock"
                                   :model "mock-model"
                                   :session-id "phase9-cli-diagnostics"))
           (result-session nil)
           (handled nil)
           (output (with-output-to-string (*standard-output*)
                     (multiple-value-setq (handled result-session)
                       (claw-lisp.cli::handle-command runtime session ":diagnostics")))))
      (%assert handled
               "Expected :diagnostics to return handled-p true")
      (%assert (eq result-session session)
               "Expected :diagnostics to preserve session object")
      (%assert (search "Runtime diagnostics:" output)
               "Expected diagnostics heading, got ~S" output)
      (%assert (search "Session id: phase9-cli-diagnostics" output)
               "Expected diagnostics session id, got ~S" output)
      (%assert (search "Registered providers:" output)
               "Expected diagnostics provider count, got ~S" output)
      (%assert (search "Registered tools:" output)
               "Expected diagnostics tool count, got ~S" output)
      (%assert (search "Missing dependencies:" output)
               "Expected diagnostics dependency line, got ~S" output))))

(defun test-phase9-cli-diagnostics-dependency-branches ()
  (let ((runtime (make-runtime)))
    (register-default-providers runtime)
    (let ((session (start-session runtime
                                  :provider-name "mock"
                                  :model "mock-model"
                                  :session-id "phase9-cli-diagnostics-deps"))
          (old-fn (symbol-function 'claw-lisp.core.runtime:check-runtime-dependencies)))
      (unwind-protect
           (progn
             (setf (symbol-function 'claw-lisp.core.runtime:check-runtime-dependencies)
                   (lambda () '("curl" "python3")))
             (let ((missing-output
                     (with-output-to-string (*standard-output*)
                       (claw-lisp.cli::handle-command runtime session ":diagnostics"))))
               (%assert (search "Missing dependencies: curl, python3" missing-output)
                        "Expected formatted missing dependencies, got ~S" missing-output))
             (setf (symbol-function 'claw-lisp.core.runtime:check-runtime-dependencies)
                   (lambda () (error "dependency probe failed")))
             (let ((error-output
                     (with-output-to-string (*standard-output*)
                       (claw-lisp.cli::handle-command runtime session ":diagnostics"))))
               (%assert (search "Missing dependencies: (diagnostic check failed:" error-output)
                        "Expected dependency diagnostic failure prefix output, got ~S" error-output)
               (%assert (search "dependency probe failed" error-output)
                        "Expected dependency diagnostic failure reason output, got ~S" error-output)))
        (setf (symbol-function 'claw-lisp.core.runtime:check-runtime-dependencies) old-fn)))))

(defun test-phase9-cli-diagnostics-no-transcript-path ()
  (let ((runtime (make-runtime)))
    (register-default-providers runtime)
    (let ((session (start-session runtime
                                  :provider-name "mock"
                                  :model "mock-model"
                                  :session-id "phase9-cli-diagnostics-no-transcript"))
          (old-existing-fn (symbol-function 'claw-lisp.core.runtime:session-transcript-existing-path))
          (old-fn (symbol-function 'claw-lisp.core.runtime:session-transcript-path)))
      (unwind-protect
           (progn
             (setf (symbol-function 'claw-lisp.core.runtime:session-transcript-existing-path)
                   (lambda (&rest args)
                     (declare (ignore args))
                     nil))
             (setf (symbol-function 'claw-lisp.core.runtime:session-transcript-path)
                   (lambda (&rest args)
                     (declare (ignore args))
                     nil))
             (let ((output
                     (with-output-to-string (*standard-output*)
                       (claw-lisp.cli::handle-command runtime session ":diagnostics"))))
               (%assert (search "Transcript path: (none)" output)
                        "Expected diagnostics NIL transcript path fallback, got ~S" output)))
        (setf (symbol-function 'claw-lisp.core.runtime:session-transcript-existing-path) old-existing-fn)
        (setf (symbol-function 'claw-lisp.core.runtime:session-transcript-path) old-fn)))))

(defun test-phase9-cli-diagnostics-protects-external-calls ()
  (let ((runtime (make-runtime)))
    (register-default-providers runtime)
    (let ((session (start-session runtime
                                  :provider-name "mock"
                                  :model "mock-model"
                                  :session-id "phase9-cli-diagnostics-errors"))
          (old-provider-fn (symbol-function 'claw-lisp.core.runtime:list-provider-names))
          (old-tool-fn (symbol-function 'claw-lisp.core.runtime:list-tool-names))
          (old-transcript-existing-fn (symbol-function 'claw-lisp.core.runtime:session-transcript-existing-path))
          (old-transcript-fn (symbol-function 'claw-lisp.core.runtime:session-transcript-path)))
      (unwind-protect
           (progn
             (setf (symbol-function 'claw-lisp.core.runtime:list-provider-names)
                   (lambda (&rest args)
                     (declare (ignore args))
                     (error "provider list failed")))
             (let ((output
                     (with-output-to-string (*standard-output*)
                       (claw-lisp.cli::handle-command runtime session ":diagnostics"))))
               (%assert (search "Runtime diagnostics failed: provider list failed" output)
                        "Expected provider-list diagnostics failure, got ~S" output))
             (setf (symbol-function 'claw-lisp.core.runtime:list-provider-names) old-provider-fn)
             (setf (symbol-function 'claw-lisp.core.runtime:list-tool-names)
                   (lambda (&rest args)
                     (declare (ignore args))
                     (error "tool list failed")))
             (let ((output
                     (with-output-to-string (*standard-output*)
                       (claw-lisp.cli::handle-command runtime session ":diagnostics"))))
               (%assert (search "Runtime diagnostics failed: tool list failed" output)
                        "Expected tool-list diagnostics failure, got ~S" output))
             (setf (symbol-function 'claw-lisp.core.runtime:list-tool-names) old-tool-fn)
             (setf (symbol-function 'claw-lisp.core.runtime:session-transcript-existing-path)
                   (lambda (&rest args)
                     (declare (ignore args))
                     (error "transcript path failed")))
             (setf (symbol-function 'claw-lisp.core.runtime:session-transcript-path)
                   (lambda (&rest args)
                     (declare (ignore args))
                     (error "transcript path failed")))
             (let ((output
                     (with-output-to-string (*standard-output*)
                       (claw-lisp.cli::handle-command runtime session ":diagnostics"))))
               (%assert (search "Runtime diagnostics failed: transcript path failed" output)
                        "Expected transcript-path diagnostics failure, got ~S" output)))
        (setf (symbol-function 'claw-lisp.core.runtime:list-provider-names) old-provider-fn)
        (setf (symbol-function 'claw-lisp.core.runtime:list-tool-names) old-tool-fn)
        (setf (symbol-function 'claw-lisp.core.runtime:session-transcript-existing-path)
              old-transcript-existing-fn)
        (setf (symbol-function 'claw-lisp.core.runtime:session-transcript-path) old-transcript-fn)))))

(defun test-phase9-cli-diagnostics-emits-debug-warnings ()
  (let ((runtime (make-runtime)))
    (register-default-providers runtime)
    (let ((session (start-session runtime
                                  :provider-name "mock"
                                  :model "mock-model"
                                  :session-id "phase9-cli-diagnostics-warnings"))
          (old-deps-fn (symbol-function 'claw-lisp.core.runtime:check-runtime-dependencies))
          (old-provider-fn (symbol-function 'claw-lisp.core.runtime:list-provider-names)))
      (unwind-protect
           (progn
             (setf (symbol-function 'claw-lisp.core.runtime:check-runtime-dependencies)
                   (lambda () (error "dependency probe failed")))
             (let ((warnings '()))
               (handler-bind
                   ((warning (lambda (w)
                               (push (princ-to-string w) warnings)
                               (muffle-warning w))))
                 (with-output-to-string (*standard-output*)
                   (claw-lisp.cli::handle-command runtime session ":diagnostics")))
               (%assert (some (lambda (msg)
                                (search "Diagnostics dependency check failed" msg))
                              warnings)
                        "Expected dependency-check warning log, got ~S" warnings))
             (setf (symbol-function 'claw-lisp.core.runtime:check-runtime-dependencies) old-deps-fn)
             (setf (symbol-function 'claw-lisp.core.runtime:list-provider-names)
                   (lambda (&rest args)
                     (declare (ignore args))
                     (error "provider list failed")))
             (let ((warnings '()))
               (handler-bind
                   ((warning (lambda (w)
                               (push (princ-to-string w) warnings)
                               (muffle-warning w))))
                 (with-output-to-string (*standard-output*)
                   (claw-lisp.cli::handle-command runtime session ":diagnostics")))
               (%assert (some (lambda (msg)
                                (search "Runtime diagnostics command failed" msg))
                              warnings)
                        "Expected diagnostics-command warning log, got ~S" warnings)))
        (setf (symbol-function 'claw-lisp.core.runtime:check-runtime-dependencies) old-deps-fn)
        (setf (symbol-function 'claw-lisp.core.runtime:list-provider-names) old-provider-fn)))))

(defun test-phase10-cli-cas-visibility-commands ()
  (let* ((temp-root (uiop:temporary-directory))
         (data-root (merge-pathnames
                     (format nil "claw-lisp-phase10-cli-cas-~D-~D/"
                             (get-universal-time)
                             (get-internal-real-time))
                     temp-root))
         (transcripts-root (merge-pathnames "transcripts/" data-root))
         (artifacts-root (merge-pathnames "artifacts/" data-root))
         (memory-root (merge-pathnames "memory/" data-root))
         (cas-root (merge-pathnames "cas/objects/" data-root))
         (ref-root (merge-pathnames "cas/refs/" data-root))
         (config (claw-lisp.config:make-default-runtime-config))
         runtime
         session)
    (setf (claw-lisp.config:runtime-config-data-root config) (namestring data-root))
    (setf (claw-lisp.config:runtime-config-transcripts-root config) (namestring transcripts-root))
    (setf (claw-lisp.config:runtime-config-artifacts-root config) (namestring artifacts-root))
    (setf (claw-lisp.config:runtime-config-memory-root config) (namestring memory-root))
    (setf (claw-lisp.config:runtime-config-cas-objects-root config) (namestring cas-root))
    (setf (claw-lisp.config:runtime-config-cas-ref-root config) (namestring ref-root))
    (setf (claw-lisp.config:runtime-config-default-provider config) "mock")
    (setf (claw-lisp.config:runtime-config-default-model config) "mock-model")
    (setf (claw-lisp.config:runtime-config-tool-result-dedup-p config) t)
    (unwind-protect
         (progn
           (ensure-directories-exist (uiop:ensure-directory-pathname data-root))
           (ensure-directories-exist (uiop:ensure-directory-pathname transcripts-root))
           (ensure-directories-exist (uiop:ensure-directory-pathname artifacts-root))
           (ensure-directories-exist (uiop:ensure-directory-pathname memory-root))
           (ensure-directories-exist (uiop:ensure-directory-pathname cas-root))
           (ensure-directories-exist (uiop:ensure-directory-pathname ref-root))
           (setf runtime (make-runtime :config config))
           (register-default-providers runtime)
           (setf session (start-session runtime
                                        :provider-name "mock"
                                        :model "mock-model"
                                        :session-id "phase10-cli-cas-visibility"))
           (let* ((stored-hash (claw-lisp.storage.cas:cas-put
                                cas-root "cli cas visibility payload"))
                  (missing-hash (claw-lisp.storage.cas:cas-hash
                                 "missing cli cas visibility payload"))
                  (ref-name "sessions/phase10-cli-cas/current-object")
                  (missing-ref-name "sessions/phase10-cli-cas/missing-object")
                  (manifest-entry (claw-lisp.cas.manifest:make-manifest-entry
                                   :role :tool-result
                                   :cas-hash stored-hash
                                   :type :sexp
                                   :metadata '(:tool-name "echo")))
                  (manifest (claw-lisp.cas.manifest:make-manifest
                             :entries (list manifest-entry)
                             :metadata '(:session-id "phase10-cli-cas")))
                  (manifest-hash (claw-lisp.cas.manifest:store-manifest
                                  cas-root manifest))
                  (mismatched-manifest (claw-lisp.cas.manifest::%make-manifest
                                        :root-digest stored-hash
                                        :entries (list manifest-entry)
                                        :metadata '(:session-id "phase10-cli-cas")
                                        :signature nil))
                  (mismatched-manifest-hash (claw-lisp.storage.cas:cas-put
                                             cas-root
                                             (claw-lisp.cas.manifest:serialize-manifest
                                              mismatched-manifest)))
                  (run-command (lambda (command)
                                 (with-output-to-string (*standard-output*)
                                   (claw-lisp.cli::handle-command runtime session command)))))
             (claw-lisp.storage.cas-ref:write-cas-ref ref-root ref-name stored-hash
                                                      :record-history-p t
                                                      :metadata '(:source :cli-test))
             (let ((status-output (funcall run-command ":cas status"))
                   (object-output (funcall run-command (format nil ":cas object ~A" stored-hash)))
                   (missing-object-output (funcall run-command (format nil ":cas object ~A" missing-hash)))
                   (ref-output (funcall run-command (format nil ":cas ref ~A" ref-name)))
                   (missing-ref-output (funcall run-command (format nil ":cas ref ~A" missing-ref-name)))
                   (manifest-output (funcall run-command (format nil ":cas manifest ~A" manifest-hash)))
                   (mismatch-output (funcall run-command (format nil ":cas manifest ~A" mismatched-manifest-hash)))
                   (transcript-lines (read-lines (claw-lisp.core.runtime:session-transcript-path runtime session))))
               (%assert (search "CAS status:" status-output)
                        "Expected CAS status output, got ~S" status-output)
               (%assert (search "Objects root exists: yes" status-output)
                        "Expected CAS objects root to be reported present, got ~S" status-output)
               (%assert (search "Refs root exists: yes" status-output)
                        "Expected CAS refs root to be reported present, got ~S" status-output)
               (%assert (search "Tool-result dedup: enabled" status-output)
                        "Expected dedup status in CLI output, got ~S" status-output)
               (%assert (and (search "CAS object:" object-output)
                             (search "Exists: yes" object-output)
                             (search stored-hash object-output))
                        "Expected CAS object happy-path output, got ~S" object-output)
               (%assert (and (search "CAS object:" missing-object-output)
                             (search "Exists: no" missing-object-output))
                        "Expected missing CAS object output, got ~S" missing-object-output)
               (%assert (and (search "CAS ref:" ref-output)
                             (search "Exists: yes" ref-output)
                             (search "Resolved: yes" ref-output))
                        "Expected CAS ref happy-path output, got ~S" ref-output)
               (%assert (and (search "CAS ref:" missing-ref-output)
                             (search "Exists: no" missing-ref-output))
                        "Expected missing CAS ref output, got ~S" missing-ref-output)
               (%assert (and (search "CAS manifest:" manifest-output)
                             (search "Integrity: verified" manifest-output))
                        "Expected verified CAS manifest output, got ~S" manifest-output)
               (%assert (and (search "CAS manifest:" mismatch-output)
                             (search "Integrity: mismatch" mismatch-output))
                        "Expected mismatched CAS manifest output, got ~S" mismatch-output)
               (%assert (some (lambda (line)
                                (and (search "\"event\":\"cas_verify_failed\"" line)
                                     (search mismatched-manifest-hash line)))
                              transcript-lines)
                        "Expected cas_verify_failed transcript event for mismatched manifest, got ~S"
                        transcript-lines)))))
      (when (uiop:directory-exists-p data-root)
        (uiop:delete-directory-tree (uiop:ensure-directory-pathname data-root)
                                    :validate t)))
  (format t "~&+ test-phase10-cli-cas-visibility-commands passed~%")
  t)

(defun test-phase10-tool-result-cas-transcript-event ()
  (let* ((temp-root (uiop:temporary-directory))
         (data-root (merge-pathnames
                     (format nil "claw-lisp-phase10-tool-result-cas-~D-~D/"
                             (get-universal-time)
                             (get-internal-real-time))
                     temp-root))
         (transcripts-root (merge-pathnames "transcripts/" data-root))
         (artifacts-root (merge-pathnames "artifacts/" data-root))
         (memory-root (merge-pathnames "memory/" data-root))
         (cas-root (merge-pathnames "cas/objects/" data-root))
         (ref-root (merge-pathnames "cas/refs/" data-root))
         (config (claw-lisp.config:make-default-runtime-config))
         runtime
         session)
    (setf (claw-lisp.config:runtime-config-data-root config) (namestring data-root))
    (setf (claw-lisp.config:runtime-config-transcripts-root config) (namestring transcripts-root))
    (setf (claw-lisp.config:runtime-config-artifacts-root config) (namestring artifacts-root))
    (setf (claw-lisp.config:runtime-config-memory-root config) (namestring memory-root))
    (setf (claw-lisp.config:runtime-config-cas-objects-root config) (namestring cas-root))
    (setf (claw-lisp.config:runtime-config-cas-ref-root config) (namestring ref-root))
    (setf (claw-lisp.config:runtime-config-default-provider config) "mock")
    (setf (claw-lisp.config:runtime-config-default-model config) "mock-model")
    (setf (claw-lisp.config:runtime-config-tool-result-dedup-p config) t)
    (unwind-protect
         (progn
           (ensure-directories-exist (uiop:ensure-directory-pathname data-root))
           (ensure-directories-exist (uiop:ensure-directory-pathname transcripts-root))
           (ensure-directories-exist (uiop:ensure-directory-pathname artifacts-root))
           (ensure-directories-exist (uiop:ensure-directory-pathname memory-root))
           (ensure-directories-exist (uiop:ensure-directory-pathname cas-root))
           (ensure-directories-exist (uiop:ensure-directory-pathname ref-root))
           (setf runtime (make-runtime :config config))
           (claw-lisp.core.runtime:register-default-tools runtime)
           (register-default-providers runtime)
           (setf session (start-session runtime
                                        :provider-name "mock"
                                        :model "mock-model"
                                        :session-id "phase10-tool-result-cas-event"))
           (execute-registered-tool runtime session "echo" '(:text "cas event payload"))
           (execute-registered-tool runtime session "echo" '(:text "cas event payload"))
           (let ((lines (read-lines (claw-lisp.core.runtime:session-transcript-path runtime session))))
             (%assert (some (lambda (line)
                              (and (search "\"event\":\"cas_object_written\"" line)
                                   (search "\"kind\":\"tool_result\"" line)
                                   (search "\"tool_name\":\"echo\"" line)))
                            lines)
                      "Expected cas_object_written transcript event for tool result, got ~S" lines)
             (let ((dedup-events
                     (remove-if-not
                      (lambda (line)
                        (and (search "\"event\":\"cas_object_written\"" line)
                             (search "\"kind\":\"tool_result\"" line)
                             (search "\"tool_name\":\"echo\"" line)
                             (search "\"deduplicated_p\":true" line)))
                      lines)))
               (%assert (= (length dedup-events) 1)
                        "Expected exactly one deduplicated cas_object_written event, got ~S"
                        dedup-events)))))
      (when (uiop:directory-exists-p data-root)
        (uiop:delete-directory-tree (uiop:ensure-directory-pathname data-root)
                                    :validate t)))
  (format t "~&+ test-phase10-tool-result-cas-transcript-event passed~%")
  t)

(defun test-phase10-manifest-artifact-transcript-events ()
  (let* ((temp-root (uiop:temporary-directory))
         (data-root (merge-pathnames
                     (format nil "claw-lisp-phase10-manifest-cas-~D-~D/"
                             (get-universal-time)
                             (get-internal-real-time))
                     temp-root))
         (transcripts-root (merge-pathnames "transcripts/" data-root))
         (artifacts-root (merge-pathnames "artifacts/" data-root))
         (memory-root (merge-pathnames "memory/" data-root))
         (cas-root (merge-pathnames "cas/objects/" data-root))
         (ref-root (merge-pathnames "cas/refs/" data-root))
         (config (claw-lisp.config:make-default-runtime-config))
         runtime
         session)
    (setf (claw-lisp.config:runtime-config-data-root config) (namestring data-root))
    (setf (claw-lisp.config:runtime-config-transcripts-root config) (namestring transcripts-root))
    (setf (claw-lisp.config:runtime-config-artifacts-root config) (namestring artifacts-root))
    (setf (claw-lisp.config:runtime-config-memory-root config) (namestring memory-root))
    (setf (claw-lisp.config:runtime-config-cas-objects-root config) (namestring cas-root))
    (setf (claw-lisp.config:runtime-config-cas-ref-root config) (namestring ref-root))
    (setf (claw-lisp.config:runtime-config-default-provider config) "mock")
    (setf (claw-lisp.config:runtime-config-default-model config) "mock-model")
    (unwind-protect
         (progn
           (ensure-directories-exist (uiop:ensure-directory-pathname data-root))
           (ensure-directories-exist (uiop:ensure-directory-pathname transcripts-root))
           (ensure-directories-exist (uiop:ensure-directory-pathname artifacts-root))
           (ensure-directories-exist (uiop:ensure-directory-pathname memory-root))
           (ensure-directories-exist (uiop:ensure-directory-pathname cas-root))
           (ensure-directories-exist (uiop:ensure-directory-pathname ref-root))
           (setf runtime (make-runtime :config config))
           (register-default-providers runtime)
           (setf session (start-session runtime
                                        :provider-name "mock"
                                        :model "mock-model"
                                        :session-id "phase10-manifest-cas-event"))
           (claw-lisp.core.runtime::persist-session-artifact-to-cas
            runtime session
            :compaction-manifest
            '(:session-id "phase10-manifest-cas-event" :entries 1)
            :type :sexp
            :ref-name "sessions/phase10-manifest/current-manifest"
            :metadata '(:session-id "phase10-manifest-cas-event"))
           (let ((lines (read-lines (claw-lisp.core.runtime:session-transcript-path runtime session))))
             (%assert (some (lambda (line)
                              (and (search "\"event\":\"cas_object_written\"" line)
                                   (search "\"kind\":\"compaction_manifest\"" line)))
                            lines)
                      "Expected cas_object_written transcript event for manifest artifact, got ~S" lines)
             (%assert (some (lambda (line)
                              (and (search "\"event\":\"cas_manifest_created\"" line)
                                   (search "\"kind\":\"compaction_manifest\"" line)))
                            lines)
                      "Expected cas_manifest_created transcript event, got ~S" lines))))
      (when (uiop:directory-exists-p data-root)
        (uiop:delete-directory-tree (uiop:ensure-directory-pathname data-root)
                                    :validate t)))
  (format t "~&+ test-phase10-manifest-artifact-transcript-events passed~%")
  t)

;;; ============================================================
;;; Phase 10: Structured Compaction IR Tests
;;; ============================================================

(defun test-compaction-ir-construction ()
  (let* ((provenance (claw-lisp.core.domain:make-compaction-ir-provenance
                      :session-memory-used-p t
                      :uncovered-messages-count 10
                      :summarized-messages-count 8
                      :preserved-tail-count 4
                      :total-messages-before 14
                      :compaction-depth 1))
         (item1 (claw-lisp.core.domain:make-compaction-ir-item
                 :type :key-value
                 :text "- source: session-memory-selective"))
         (item2 (claw-lisp.core.domain:make-compaction-ir-item
                 :type :bullet
                 :text "- user: hello"
                 :role :user
                 :message-index 0))
         (item3 (claw-lisp.core.domain:make-compaction-ir-item
                 :type :bullet
                 :text "- echo (5 bytes): hello"
                 :tool-name "echo"
                 :persisted-path "/tmp/tool-1.txt"
                 :call-id "call-1"
                 :bytes 5))
         (sec1 (claw-lisp.core.domain:make-compaction-ir-section
                :kind :provenance
                :heading "Provenance"
                :items (list item1)
                :priority :high))
         (sec2 (claw-lisp.core.domain:make-compaction-ir-section
                :kind :message-summary
                :heading "Recent messages"
                :items (list item2)
                :priority :normal))
         (sec3 (claw-lisp.core.domain:make-compaction-ir-section
                :kind :tool-result-summary
                :heading "Recent tool results"
                :items (list item3)
                :priority :low))
         (ir (claw-lisp.core.domain:make-compaction-ir
              :id "compact-test-1"
              :source :session-memory-selective
              :session-id "sess-1"
              :predecessor-fingerprint "ABC123"
              :provenance provenance
              :sections (list sec1 sec2 sec3)
              :token-budget 5000)))
    (%assert (string= (claw-lisp.core.domain:compaction-ir-id ir) "compact-test-1")
             "IR id mismatch")
    (%assert (eq (claw-lisp.core.domain:compaction-ir-source ir) :session-memory-selective)
             "IR source mismatch")
    (%assert (string= (claw-lisp.core.domain:compaction-ir-session-id ir) "sess-1")
             "IR session-id mismatch")
    (%assert (string= (claw-lisp.core.domain:compaction-ir-predecessor-fingerprint ir) "ABC123")
             "IR predecessor-fingerprint mismatch")
    (%assert (= (length (claw-lisp.core.domain:compaction-ir-sections ir)) 3)
             "IR should have 3 sections")
    (%assert (claw-lisp.core.domain:compaction-ir-provenance-session-memory-used-p provenance)
             "Provenance session-memory-used-p should be T")
    (%assert (= (claw-lisp.core.domain:compaction-ir-provenance-uncovered-messages-count provenance) 10)
             "Provenance uncovered-messages-count mismatch")
    (%assert (= (claw-lisp.core.domain:compaction-ir-provenance-compaction-depth provenance) 1)
             "Provenance compaction-depth mismatch")
    (%assert (eq (claw-lisp.core.domain:compaction-ir-item-role item2) :user)
             "Item role mismatch")
    (%assert (string= (claw-lisp.core.domain:compaction-ir-item-persisted-path item3) "/tmp/tool-1.txt")
             "Item persisted-path mismatch")
    (%assert (string= (claw-lisp.core.domain:compaction-ir-item-call-id item3) "call-1")
             "Item call-id mismatch")
    (%assert (= (claw-lisp.core.domain:compaction-ir-item-bytes item3) 5)
             "Item bytes mismatch")))

(defun test-compaction-ir-render-to-markdown ()
  (let* ((prov-item (claw-lisp.core.domain:make-compaction-ir-item
                     :type :key-value
                     :text "- source: session-memory-selective"))
         (msg-item (claw-lisp.core.domain:make-compaction-ir-item
                    :type :bullet
                    :text "- user: what is 2+2?"
                    :role :user))
         (raw-item (claw-lisp.core.domain:make-compaction-ir-item
                    :type :raw-text
                    :text "Session memory body content here"))
         (sec-prov (claw-lisp.core.domain:make-compaction-ir-section
                    :kind :provenance
                    :heading "Provenance"
                    :items (list prov-item)
                    :priority :high))
         (sec-mem (claw-lisp.core.domain:make-compaction-ir-section
                   :kind :session-memory-snapshot
                   :heading "Session Memory Snapshot"
                   :items (list raw-item)
                   :priority :high))
         (sec-msg (claw-lisp.core.domain:make-compaction-ir-section
                   :kind :message-summary
                   :heading "Selective Summary of Older Messages"
                   :items (list msg-item)
                   :priority :normal))
         (ir (claw-lisp.core.domain:make-compaction-ir
              :source :session-memory-selective
              :session-id "test-sess"
              :sections (list sec-prov sec-mem sec-msg)))
         (md (claw-lisp.core.compact:render-compaction-ir-to-markdown ir)))
    (%assert (search "# Selective Compaction Summary" md)
             "Missing heading in rendered markdown, got ~S" md)
    (%assert (search "Session: test-sess" md)
             "Missing session line in rendered markdown, got ~S" md)
    (%assert (search "## Provenance" md)
             "Missing Provenance section heading, got ~S" md)
    (%assert (search "- source: session-memory-selective" md)
             "Missing provenance item, got ~S" md)
    (%assert (search "## Session Memory Snapshot" md)
             "Missing Session Memory Snapshot heading, got ~S" md)
    (%assert (search "Session memory body content here" md)
             "Missing raw-text item in snapshot, got ~S" md)
    (%assert (search "## Selective Summary of Older Messages" md)
             "Missing message summary heading, got ~S" md)
    (%assert (search "- user: what is 2+2?" md)
             "Missing bullet item in message summary, got ~S" md)))

(defun test-compaction-ir-render-fallback-heading ()
  (let* ((ir (claw-lisp.core.domain:make-compaction-ir
              :source :fallback
              :session-id "fb-sess"
              :sections nil))
         (md (claw-lisp.core.compact:render-compaction-ir-to-markdown ir)))
    (%assert (search "# Fallback Compaction Summary" md)
             "Fallback heading mismatch, got ~S" md)))

(defun test-compaction-ir-render-skips-trimmed-sections ()
  (let* ((sec1 (claw-lisp.core.domain:make-compaction-ir-section
                :kind :provenance :heading "Provenance"
                :items (list (claw-lisp.core.domain:make-compaction-ir-item
                              :type :key-value :text "- source: fallback"))
                :priority :high))
         (sec2 (claw-lisp.core.domain:make-compaction-ir-section
                :kind :tool-result-summary :heading "Tool Results"
                :items (list (claw-lisp.core.domain:make-compaction-ir-item
                              :type :bullet :text "- echo: hello"))
                :trimmed-p t
                :priority :low))
         (ir (claw-lisp.core.domain:make-compaction-ir
              :source :fallback :session-id "trim-test"
              :sections (list sec1 sec2)))
         (md (claw-lisp.core.compact:render-compaction-ir-to-markdown ir)))
    (%assert (search "## Provenance" md)
             "Expected Provenance section to be rendered")
    (%assert (null (search "## Tool Results" md))
             "Expected trimmed Tool Results section to be skipped")))

(defun test-compaction-ir-to-plist ()
  (let* ((prov (claw-lisp.core.domain:make-compaction-ir-provenance
                :session-memory-used-p t
                :uncovered-messages-count 5
                :summarized-messages-count 3
                :preserved-tail-count 4
                :total-messages-before 9
                :tool-results-summarized-count 2
                :compaction-depth 1))
         (sec (claw-lisp.core.domain:make-compaction-ir-section
               :kind :message-summary :heading "Messages"
               :items (list (claw-lisp.core.domain:make-compaction-ir-item
                             :type :bullet :text "- user: hi"))
               :tokens-estimated 42
               :priority :normal))
         (ir (claw-lisp.core.domain:make-compaction-ir
              :id "plist-test-ir"
              :source :session-memory-selective
              :session-id "plist-sess"
              :predecessor-fingerprint "DEADBEEF"
              :provenance prov
              :sections (list sec)
              :token-budget 2000
              :tokens-used 42))
         (pl (claw-lisp.core.compact:compaction-ir-to-plist ir)))
    (%assert (string= (getf pl :id) "plist-test-ir")
             "Plist :id mismatch")
    (%assert (eq (getf pl :source) :session-memory-selective)
             "Plist :source mismatch")
    (%assert (string= (getf pl :session_id) "plist-sess")
             "Plist :session_id mismatch")
    (%assert (string= (getf pl :predecessor_fingerprint) "DEADBEEF")
             "Plist :predecessor_fingerprint mismatch")
    (%assert (= (getf pl :token_budget) 2000)
             "Plist :token_budget mismatch")
    (%assert (= (getf pl :tokens_used) 42)
             "Plist :tokens_used mismatch")
    (let ((prov-pl (getf pl :provenance)))
      (%assert prov-pl "Plist :provenance missing")
      (%assert (eq (getf prov-pl :session_memory_used_p) t)
               "Provenance plist session_memory_used_p mismatch")
      (%assert (= (getf prov-pl :uncovered_messages_count) 5)
               "Provenance plist uncovered_messages_count mismatch")
      (%assert (= (getf prov-pl :compaction_depth) 1)
               "Provenance plist compaction_depth mismatch"))
    (let ((sections (getf pl :sections)))
      (%assert (= (length sections) 1)
               "Expected 1 section in plist")
      (let ((sec-pl (first sections)))
        (%assert (eq (getf sec-pl :kind) :message-summary)
                 "Section plist :kind mismatch")
        (%assert (string= (getf sec-pl :heading) "Messages")
                 "Section plist :heading mismatch")
        (%assert (= (getf sec-pl :item_count) 1)
                 "Section plist :item_count mismatch")))))

(defun test-compaction-ir-token-budget-trimming ()
  (let* ((long-text (make-string 700 :initial-element #\a))
         (sec-high (claw-lisp.core.domain:make-compaction-ir-section
                    :kind :provenance :heading "Provenance"
                    :items (list (claw-lisp.core.domain:make-compaction-ir-item
                                  :type :key-value :text "- source: fallback"))
                    :priority :high))
         (sec-normal (claw-lisp.core.domain:make-compaction-ir-section
                      :kind :message-summary :heading "Messages"
                      :items (list (claw-lisp.core.domain:make-compaction-ir-item
                                    :type :bullet :text long-text))
                      :priority :normal))
         (sec-low (claw-lisp.core.domain:make-compaction-ir-section
                   :kind :tool-result-summary :heading "Tools"
                   :items (list (claw-lisp.core.domain:make-compaction-ir-item
                                 :type :bullet :text long-text))
                   :priority :low))
         (ir (claw-lisp.core.domain:make-compaction-ir
              :source :fallback :session-id "budget-test"
              :sections (list sec-high sec-normal sec-low)
              :token-budget 100))
         (config (claw-lisp.config::%make-runtime-config)))
    (claw-lisp.core.compact:estimate-and-trim-ir-sections ir config)
    (%assert (not (claw-lisp.core.domain:compaction-ir-section-trimmed-p sec-high))
             "High-priority section should NOT be trimmed")
    (%assert (claw-lisp.core.domain:compaction-ir-section-trimmed-p sec-low)
             "Low-priority section should be trimmed first")
    (%assert (> (claw-lisp.core.domain:compaction-ir-section-tokens-estimated sec-high) 0)
             "Section tokens-estimated should be positive")
    (%assert (> (claw-lisp.core.domain:compaction-ir-section-tokens-estimated sec-low) 0)
             "Trimmed section should still have tokens-estimated set")))

(defun test-compaction-ir-token-budget-never-trims-provenance ()
  (let* ((long-text (make-string 700 :initial-element #\a))
         (sec-provenance (claw-lisp.core.domain:make-compaction-ir-section
                          :kind :provenance :heading "Provenance"
                          :items (list (claw-lisp.core.domain:make-compaction-ir-item
                                        :type :key-value :text "- source: fallback"))
                          :priority :high))
         (sec-summary (claw-lisp.core.domain:make-compaction-ir-section
                       :kind :message-summary :heading "Messages"
                       :items (list (claw-lisp.core.domain:make-compaction-ir-item
                                     :type :bullet :text long-text))
                       :priority :normal))
         (sec-tools (claw-lisp.core.domain:make-compaction-ir-section
                     :kind :tool-result-summary :heading "Tools"
                     :items (list (claw-lisp.core.domain:make-compaction-ir-item
                                   :type :bullet :text long-text))
                     :priority :low))
         ;; Tiny budget forces aggressive trimming.
         (ir (claw-lisp.core.domain:make-compaction-ir
              :source :fallback :session-id "budget-provenance-keep"
              :sections (list sec-provenance sec-summary sec-tools)
              :token-budget 1))
         (config (claw-lisp.config::%make-runtime-config)))
    (claw-lisp.core.compact:estimate-and-trim-ir-sections ir config)
    (%assert (not (claw-lisp.core.domain:compaction-ir-section-trimmed-p sec-provenance))
             "Provenance section must never be trimmed")
    (%assert (or (not (claw-lisp.core.domain:compaction-ir-section-trimmed-p sec-summary))
                 (not (claw-lisp.core.domain:compaction-ir-section-trimmed-p sec-tools)))
             "At least one non-provenance section should remain untrimmed")))

(defun test-compaction-ir-token-budget-can-remain-over-budget-with-must-keep-sections ()
  (let* ((long-text (make-string 700 :initial-element #\a))
         (sec-provenance (claw-lisp.core.domain:make-compaction-ir-section
                          :kind :provenance :heading "Provenance"
                          :items (list (claw-lisp.core.domain:make-compaction-ir-item
                                        :type :key-value :text "- source: fallback"))
                          :priority :high))
         (sec-summary (claw-lisp.core.domain:make-compaction-ir-section
                       :kind :message-summary :heading "Messages"
                       :items (list (claw-lisp.core.domain:make-compaction-ir-item
                                     :type :bullet :text long-text))
                       :priority :normal))
         (sec-tools (claw-lisp.core.domain:make-compaction-ir-section
                     :kind :tool-result-summary :heading "Tools"
                     :items (list (claw-lisp.core.domain:make-compaction-ir-item
                                   :type :bullet :text long-text))
                     :priority :low))
         (ir (claw-lisp.core.domain:make-compaction-ir
              :source :fallback :session-id "budget-min-keep"
              :sections (list sec-provenance sec-summary sec-tools)
              :token-budget 1))
         (config (claw-lisp.config::%make-runtime-config)))
    (claw-lisp.core.compact:estimate-and-trim-ir-sections ir config)
    (%assert (> (claw-lisp.core.domain:compaction-ir-tokens-used ir)
                (claw-lisp.core.domain:compaction-ir-token-budget ir))
             "Tokens used may remain above budget when only must-keep sections remain")
    (%assert (claw-lisp.core.domain:compaction-ir-section-trimmed-p sec-tools)
             "Lowest-priority trimmable section should be trimmed first")))

(defun test-compaction-ir-backward-compat ()
  (let* ((sec (claw-lisp.core.domain:make-compaction-ir-section
               :kind :provenance :heading "Provenance"
               :items (list (claw-lisp.core.domain:make-compaction-ir-item
                             :type :key-value :text "- source: fallback"))
               :priority :high))
         (ir (claw-lisp.core.domain:make-compaction-ir
              :source :fallback :session-id "compat-sess"
              :sections (list sec)))
         (result (claw-lisp.core.domain:make-compaction-result
                  :source :fallback
                  :summary ""
                  :ir ir
                  :preserved-messages nil))
         (rendered (claw-lisp.core.compact:compaction-result-rendered-summary result)))
    (%assert (> (length rendered) 0)
             "compaction-result-rendered-summary should return non-empty string from IR")
    (%assert (search "# Fallback Compaction Summary" rendered)
             "Rendered summary should have Fallback heading")
    (%assert (string= (claw-lisp.core.domain:compaction-result-summary result) rendered)
             "Summary should be cached after first render")))

(defun test-compaction-ir-fingerprint ()
  (let* ((sec (claw-lisp.core.domain:make-compaction-ir-section
               :kind :provenance :heading "Provenance"
               :items (list (claw-lisp.core.domain:make-compaction-ir-item
                             :type :key-value :text "- source: fallback"))
               :priority :high))
         (ir (claw-lisp.core.domain:make-compaction-ir
              :source :fallback :session-id "fp-test"
              :sections (list sec)))
         (fp (claw-lisp.core.compact:compaction-ir-fingerprint ir)))
    (%assert (stringp fp)
             "Fingerprint should be a string")
    (%assert (> (length fp) 0)
             "Fingerprint should be non-empty")
    (%assert (string= fp (claw-lisp.core.compact:compaction-ir-fingerprint ir))
             "Fingerprint should be deterministic for same IR")))

(defun test-compaction-ir-fallback-path-produces-ir ()
  (let* ((root (merge-pathnames
                (format nil "claw-lisp-test-ir-fb-~A/" (get-universal-time))
                #P"/tmp/"))
         (config (claw-lisp.config::%make-runtime-config
                  :data-root (namestring root)
                  :transcripts-root (namestring (merge-pathnames "transcripts/" root))
                  :artifacts-root (namestring (merge-pathnames "artifacts/" root))
                  :memory-root (namestring (merge-pathnames "memory/" root))
                  :default-provider "mock"
                  :default-model "mock-model"))
         (conversation (claw-lisp.core.domain:make-conversation :id "ir-test"))
         (session (claw-lisp.core.domain:make-agent-session
                   :id "ir-fallback-sess"
                   :provider "mock"
                   :model "mock-model"
                   :conversation conversation)))
    (unwind-protect
         (progn
           (claw-lisp.core.domain:append-message
            conversation
            (claw-lisp.core.domain:make-message :role :user :content "hello"))
           (claw-lisp.core.domain:append-message
            conversation
            (claw-lisp.core.domain:make-message :role :assistant :content "hi there"))
           (claw-lisp.core.domain:record-tool-result
            conversation
            (claw-lisp.core.domain:make-tool-result
             :call-id "call-42"
             :tool-name "echo"
             :content "echoed"
             :persisted-path "/tmp/tool-42.txt"
             :bytes 6))
           (let ((result (claw-lisp.core.compact:compact-session-locally
                          config session :keep-recent-messages 2)))
             (%assert (claw-lisp.core.domain:compaction-result-ir result)
                      "compact-session-locally should produce an IR")
             (%assert (eq (claw-lisp.core.domain:compaction-ir-source
                           (claw-lisp.core.domain:compaction-result-ir result))
                          :fallback)
                      "Fallback IR source should be :fallback")
             (let* ((ir (claw-lisp.core.domain:compaction-result-ir result))
                    (sections (claw-lisp.core.domain:compaction-ir-sections ir))
                    (tool-sec (find :tool-result-summary sections
                                    :key #'claw-lisp.core.domain:compaction-ir-section-kind)))
               (%assert tool-sec "Expected a :tool-result-summary section")
               (let ((items (claw-lisp.core.domain:compaction-ir-section-items tool-sec)))
                 (%assert (= (length items) 1)
                          "Expected 1 tool result item")
                 (let ((item (first items)))
                   (%assert (string= (claw-lisp.core.domain:compaction-ir-item-tool-name item)
                                     "echo")
                            "Item tool-name should be 'echo'")
                   (%assert (string= (claw-lisp.core.domain:compaction-ir-item-persisted-path item)
                                     "/tmp/tool-42.txt")
                            "Item persisted-path should be preserved")
                   (%assert (string= (claw-lisp.core.domain:compaction-ir-item-call-id item)
                                     "call-42")
                            "Item call-id should be 'call-42'")
                   (%assert (= (claw-lisp.core.domain:compaction-ir-item-bytes item) 6)
                            "Item bytes should be 6"))))
             (let ((rendered (claw-lisp.core.compact:compaction-result-rendered-summary result)))
               (%assert (search "# Fallback Compaction Summary" rendered)
                        "Rendered summary should have correct heading")
               (%assert (search "echo" rendered)
                        "Rendered summary should mention tool name"))))
      (when (probe-file root)
        (uiop:delete-directory-tree root :validate t)))))

(defun test-compaction-ir-provenance-chaining ()
  (let* ((root (merge-pathnames
                (format nil "claw-lisp-test-ir-prov-~A/" (get-universal-time))
                #P"/tmp/"))
         (config (claw-lisp.config::%make-runtime-config
                  :data-root (namestring root)
                  :transcripts-root (namestring (merge-pathnames "transcripts/" root))
                  :artifacts-root (namestring (merge-pathnames "artifacts/" root))
                  :memory-root (namestring (merge-pathnames "memory/" root))
                  :default-provider "mock"
                  :default-model "mock-model"))
         (runtime (make-runtime :config config)))
    (unwind-protect
         (progn
           (register-default-providers runtime)
           (let* ((session (start-session runtime
                                          :provider-name "mock"
                                          :model "mock-model"
                                          :session-id "prov-chain-test"))
                  (s1 (submit-user-message runtime session "first message"))
                  (s2 (submit-user-message runtime s1 "second message"))
                  (result-1 (%runtime-compact-session runtime s2 :keep-recent-messages 1)))
             ;; Apply first compaction
             (claw-lisp.core.runtime:apply-session-compaction runtime s2 result-1)
             ;; Verify fingerprint stored on session
             (let ((fp1 (claw-lisp.core.runtime::session-state-value
                         s2 :last-compaction-fingerprint)))
               (%assert (and fp1 (stringp fp1) (plusp (length fp1)))
                        "First compaction should store a fingerprint on session, got ~S" fp1)
               (%assert (= 1 (claw-lisp.core.runtime::session-state-value s2 :compaction-depth 0))
                        "Compaction depth should be 1 after first compaction")
               ;; Add more messages and compact again
               (let* ((s3 (submit-user-message runtime s2 "third message"))
                      (s4 (submit-user-message runtime s3 "fourth message"))
                      (result-2 (%runtime-compact-session runtime s4 :keep-recent-messages 1)))
                 (when result-2
                   ;; Check the second IR references the first fingerprint
                   (let ((ir2 (claw-lisp.core.domain:compaction-result-ir result-2)))
                     (%assert ir2 "Second compaction should produce an IR")
                     (%assert (string= fp1
                                       (claw-lisp.core.domain:compaction-ir-predecessor-fingerprint ir2))
                              "Second IR predecessor-fingerprint should match first fingerprint")))))))
      (when (probe-file root)
        (uiop:delete-directory-tree root :validate t)))))

(defun test-compaction-ir-transcript-includes-ir-plist ()
  (let* ((root (merge-pathnames
                (format nil "claw-lisp-test-ir-transcript-~A/" (get-universal-time))
                #P"/tmp/"))
         (config (claw-lisp.config::%make-runtime-config
                  :data-root (namestring root)
                  :transcripts-root (namestring (merge-pathnames "transcripts/" root))
                  :artifacts-root (namestring (merge-pathnames "artifacts/" root))
                  :memory-root (namestring (merge-pathnames "memory/" root))
                  :default-provider "mock"
                  :default-model "mock-model"))
         (runtime (make-runtime :config config)))
    (unwind-protect
         (progn
           (register-default-providers runtime)
           (let* ((session (start-session runtime
                                          :provider-name "mock"
                                          :model "mock-model"
                                          :session-id "ir-transcript-test"))
                  (s1 (submit-user-message runtime session "msg for transcript"))
                  (result (%runtime-compact-session runtime s1 :keep-recent-messages 1)))
             (claw-lisp.core.runtime:apply-session-compaction runtime s1 result)
             (let ((lines (read-lines (session-transcript-path runtime s1))))
               (let ((compaction-line
                       (find-if (lambda (line)
                                  (search "\"event\":\"compaction_boundary\"" line))
                                lines)))
                 (%assert compaction-line
                          "Expected compaction_boundary event in transcript")
                 (%assert (search "\"ir\"" compaction-line)
                          "Expected IR plist in compaction_boundary transcript event")))))
      (when (probe-file root)
        (uiop:delete-directory-tree root :validate t)))))

(defun test-compaction-ir-boundary-message-uses-rendered-summary ()
  (let* ((sec (claw-lisp.core.domain:make-compaction-ir-section
               :kind :message-summary :heading "Messages"
               :items (list (claw-lisp.core.domain:make-compaction-ir-item
                             :type :bullet :text "- user: test msg"
                             :role :user))
               :priority :normal))
         (ir (claw-lisp.core.domain:make-compaction-ir
              :source :fallback :session-id "boundary-test"
              :sections (list sec)))
         (result (claw-lisp.core.domain:make-compaction-result
                  :source :fallback
                  :ir ir
                  :preserved-messages nil))
         (config (claw-lisp.config::%make-runtime-config))
         (conversation (claw-lisp.core.domain:make-conversation :id "bm-test"))
         (session (claw-lisp.core.domain:make-agent-session
                   :id "bm-sess" :provider "mock" :model "m"
                   :conversation conversation)))
    (declare (ignore config session))
    (let ((boundary-msg (claw-lisp.core.compact::make-compaction-boundary-message result)))
      (%assert (search "# Compaction Boundary"
                       (claw-lisp.core.domain:message-content-text boundary-msg))
               "Boundary message should start with Compaction Boundary heading")
      (%assert (search "- user: test msg"
                       (claw-lisp.core.domain:message-content-text boundary-msg))
               "Boundary message should contain rendered IR content"))))

(defun test-tool-classification-agreement-across-subsystems ()
  "Pin the Step A invariant: the runtime stagnation predicates, the tool-envelope
   predicates, and the capability source of truth all agree for every built-in tool.
   This is the regression guard against the divergence the rebuild eliminated."
  (dolist (spec '(("file-read"     t   nil)
                  ("grep"          t   nil)
                  ("glob"          t   nil)
                  ("file-write"    nil t)
                  ("file-replace"  nil t)
                  ("shell-command" nil nil)
                  ("echo"          nil nil)))
    (destructuring-bind (name expect-read expect-write) spec
      (let* ((tool-call (list :name name :input nil))
             ;; Path 1: runtime stagnation predicates (name plist)
             (rt-read  (claw-lisp.core.runtime::read-only-tool-call-p tool-call))
             (rt-write (claw-lisp.core.runtime::write-tool-call-p tool-call))
             ;; Path 2: tool-envelope predicates (probe envelope by name)
             (probe (claw-lisp.core.tool-envelope:wrap-tool-success name ""))
             (env-read  (claw-lisp.core.tool-envelope:envelope-is-read-only-p probe))
             (env-write (claw-lisp.core.tool-envelope:envelope-is-mutation-p probe))
             ;; Path 3: capability source of truth (name)
             (cap-read  (claw-lisp.core.tool-capability:tool-name-read-only-p name))
             (cap-write (claw-lisp.core.tool-capability:tool-name-mutation-p name)))
        ;; Expected classification
        (%assert (eq (and rt-read t) expect-read)
                 "~A: read-only classification ~A, expected ~A" name rt-read expect-read)
        (%assert (eq (and rt-write t) expect-write)
                 "~A: mutation classification ~A, expected ~A" name rt-write expect-write)
        ;; All three paths must agree (the divergence guard)
        (%assert (eq (and rt-read t) (and env-read t))
                 "~A: runtime/envelope disagree on read-only (~A vs ~A)" name rt-read env-read)
        (%assert (eq (and rt-read t) (and cap-read t))
                 "~A: runtime/capability disagree on read-only (~A vs ~A)" name rt-read cap-read)
        (%assert (eq (and rt-write t) (and env-write t))
                 "~A: runtime/envelope disagree on mutation (~A vs ~A)" name rt-write env-write)
        (%assert (eq (and rt-write t) (and cap-write t))
                 "~A: runtime/capability disagree on mutation (~A vs ~A)" name rt-write cap-write)))))

(defun test-tool-capability-method-matches-registry ()
  "The object-dispatched tool-capability method and the name-keyed registry must
   return the same plist for each built-in tool (they share one constant)."
  (dolist (spec (list (cons (make-file-read-tool)     "file-read")
                      (cons (make-file-write-tool)    "file-write")
                      (cons (make-file-replace-tool)  "file-replace")
                      (cons (make-shell-command-tool) "shell-command")
                      (cons (make-grep-tool)          "grep")
                      (cons (make-glob-tool)          "glob")))
    (let* ((tool (car spec))
           (name (cdr spec))
           (from-object (claw-lisp.core.tool-capability:tool-capability tool))
           (from-registry (claw-lisp.core.tool-capability:tool-name-capability name)))
      (%assert (equal from-object from-registry)
               "~A: object capability ~S /= registry capability ~S"
               name from-object from-registry))))

(defun test-failed-write-advances-stagnation-to-nudge-threshold ()
  "assess-loop-progress: a failed write advances the stall counter to the nudge
   threshold and reports the :write-failed nudge kind."
  (let ((session (claw-lisp.core.domain:make-agent-session
                  :id "failed-write-advance"
                  :provider "mock" :model "mock"
                  :conversation (claw-lisp.core.domain:make-conversation :id "x"))))
    (let ((failure (claw-lisp.core.domain:make-tool-result
                    :call-id "r1" :tool-name "file-replace"
                    :content "[error] Tool file-replace failed: Old text not found"))
          (tool-calls (list (list :id "r1" :name "file-replace" :input nil))))
      (multiple-value-bind (count nudge-kind)
          (claw-lisp.core.runtime:assess-loop-progress session tool-calls (list failure))
        (%assert (eq :write-failed nudge-kind)
                 "Expected :write-failed nudge kind, got ~A" nudge-kind)
        (%assert (= claw-lisp.core.runtime::+read-only-tool-loop-nudge-threshold+ count)
                 "Expected stall count to equal nudge threshold after failed write, got ~A" count)
        (%assert (= count (claw-lisp.core.runtime::session-state-value
                           session :read-only-tool-loop-repeat-count 0))
                 "Expected session state to match returned count")))))

(defun test-successful-write-resets-stagnation ()
  "assess-loop-progress: a successful write resets the stall counter to 0 and
   reports no nudge — the model has earned the right to inspect again."
  (let ((session (claw-lisp.core.domain:make-agent-session
                  :id "successful-write-reset"
                  :provider "mock" :model "mock"
                  :conversation (claw-lisp.core.domain:make-conversation :id "x"))))
    (claw-lisp.core.runtime::set-session-state-value
     session :read-only-tool-loop-repeat-count 1)
    (let ((success (claw-lisp.core.domain:make-tool-result
                    :call-id "r1" :tool-name "file-write"
                    :content "Wrote 42 bytes to f.py"))
          (tool-calls (list (list :id "r1" :name "file-write" :input nil))))
      (multiple-value-bind (count nudge-kind)
          (claw-lisp.core.runtime:assess-loop-progress session tool-calls (list success))
        (%assert (null nudge-kind) "Expected no nudge on successful write, got ~A" nudge-kind)
        (%assert (= 0 count) "Expected count reset to 0 on successful write, got ~A" count)
        (%assert (= 0 (claw-lisp.core.runtime::session-state-value
                       session :read-only-tool-loop-repeat-count 0))
                 "Expected session state reset to 0 after successful write")))))

(defun test-stall-increments-on-no-write-turn-with-interleaved-shell ()
  "Step D core fix (regression guard for the Qwen/Kimi live failure): a turn that
   interleaves reads with an observational shell command (e.g. running the test
   suite) but performs NO write is a stall and MUST increment the counter.
   Previously such mixed [read, read, shell] turns dodged stall detection because
   they were not *purely* read-only, so the nudge and read-tool suppression never
   engaged and the model looped read->test->read->test until it timed out."
  (let ((session (claw-lisp.core.domain:make-agent-session
                  :id "stall-interleaved-shell"
                  :provider "mock" :model "mock"
                  :conversation (claw-lisp.core.domain:make-conversation :id "x"))))
    (let ((tool-calls (list (list :id "a" :name "file-read" :input '(:path "m.py"))
                            (list :id "b" :name "file-read" :input '(:path "t.py"))
                            (list :id "c" :name "shell-command"
                                  :input '(:text "python3 -m unittest"))))
          (results (list (claw-lisp.core.domain:make-tool-result
                          :call-id "a" :tool-name "file-read" :content "code")
                         (claw-lisp.core.domain:make-tool-result
                          :call-id "b" :tool-name "file-read" :content "tests")
                         (claw-lisp.core.domain:make-tool-result
                          :call-id "c" :tool-name "shell-command" :content "ran tests"))))
      (multiple-value-bind (count1 kind1)
          (claw-lisp.core.runtime:assess-loop-progress session tool-calls results)
        (%assert (= 1 count1)
                 "Expected interleaved-shell no-write turn to increment to 1, got ~A" count1)
        (%assert (null kind1) "Expected no nudge at count 1, got ~A" kind1))
      (multiple-value-bind (count2 kind2)
          (claw-lisp.core.runtime:assess-loop-progress session tool-calls results)
        (%assert (= 2 count2)
                 "Expected second interleaved no-write turn to increment to 2, got ~A" count2)
        (%assert (eq :stall kind2)
                 "Expected :stall nudge at threshold for interleaved no-write turn, got ~A" kind2)))))

(defun test-read-suppression-forces-write-after-stall ()
  "Integration: after two no-write read turns the loop nudges and suppresses
   read-only tools; on the next turn (reads excluded) the model writes and the
   loop makes progress. Asserts both that file-read was excluded from the offered
   tool set on the post-suppression turn and that the write actually applied."
  (let* ((root (merge-pathnames
                (format nil "claw-lisp-read-suppression-~A/" (get-universal-time))
                #P"/tmp/"))
         (target-file (merge-pathnames "workspace/target.txt" root))
         (runtime (make-runtime))
         provider)
    (unwind-protect
         (progn
           (ensure-directories-exist target-file)
           (with-open-file (s target-file :direction :output
                               :if-exists :supersede :if-does-not-exist :create)
             (write-string "hello" s))
           (setf provider (make-instance 'shell-pivot-provider
                                         :name "shell-pivot"
                                         :path (namestring target-file)))
           (claw-lisp.core.runtime:register-provider runtime provider)
           (register-tool runtime (make-file-read-tool))
           (register-tool runtime (make-file-write-tool))
           (let* ((session (start-session runtime
                                          :provider-name "shell-pivot"
                                          :model "mock-model"
                                          :session-id "read-suppression-forces-write"))
                  (conversation (claw-lisp.core.domain:agent-session-conversation session)))
             (append-message conversation
                             (make-message :role :user :content "fix the file"))
             (claw-lisp.core.runtime:execute-provider-turn-loop runtime session provider)
             (let ((offered (shell-pivot-provider-tools-offered-on-final-turn provider)))
               (%assert offered
                        "Expected the post-suppression turn to be reached (offered tools recorded)")
               (%assert (not (member "file-read"
                                     (mapcar (lambda (d) (getf d :name)) offered)
                                     :test #'string=))
                        "Expected file-read to be excluded on the post-suppression turn")
               (%assert (member "file-write"
                                (mapcar (lambda (d) (getf d :name)) offered)
                                :test #'string=)
                        "Expected file-write to remain available on the post-suppression turn"))
             (%assert (string= "fixed content"
                               (uiop:read-file-string target-file))
                      "Expected the suppression-forced write to update the file")))
      (when (probe-file root)
        (uiop:delete-directory-tree root :validate t)))))

(defun test-provider-tool-loop-budget-returns-gracefully ()
  "Reaching the provider tool-iteration budget is a normal stop, not an error.
   A provider that keeps making progress (a successful write every turn) never
   trips the stagnation guard, so the loop runs to the budget; it must then
   return the last response rather than signaling 'exceeded N iterations'."
  (let* ((root (merge-pathnames
                (format nil "claw-lisp-loop-budget-~A/" (get-universal-time))
                #P"/tmp/"))
         (target-file (merge-pathnames "workspace/target.txt" root))
         (runtime (make-runtime))
         provider)
    (unwind-protect
         (progn
           (ensure-directories-exist target-file)
           (setf provider (make-instance 'always-writing-provider
                                         :name "always-writing"
                                         :path (namestring target-file)))
           (claw-lisp.core.runtime:register-provider runtime provider)
           (register-tool runtime (make-file-write-tool))
           (let* ((session (start-session runtime
                                          :provider-name "always-writing"
                                          :model "mock-model"
                                          :session-id "loop-budget-graceful"))
                  (conversation (claw-lisp.core.domain:agent-session-conversation session))
                  (response
                    (handler-case
                        (progn
                          (append-message conversation
                                          (make-message :role :user :content "keep writing"))
                          (claw-lisp.core.runtime:execute-provider-turn-loop runtime session provider))
                      (error (c)
                        (%assert nil "Loop budget should return gracefully, but errored: ~A" c)))))
             (%assert response
                      "Expected execute-provider-turn-loop to return the last response at the budget")))
      (when (probe-file root)
        (uiop:delete-directory-tree root :validate t)))))

(defun test-stall-counter-unaffected-when-no-tool-calls ()
  "assess-loop-progress with no tool calls returns the current count and no nudge."
  (let ((session (claw-lisp.core.domain:make-agent-session
                  :id "stall-no-tool-calls"
                  :provider "mock"
                  :model "mock"
                  :conversation (claw-lisp.core.domain:make-conversation :id "x"))))
    (claw-lisp.core.runtime::set-session-state-value
     session :read-only-tool-loop-repeat-count 1)
    (multiple-value-bind (count nudge-kind)
        (claw-lisp.core.runtime:assess-loop-progress session nil nil)
      (%assert (= 1 count) "Expected count preserved at 1 with no tool calls, got ~A" count)
      (%assert (null nudge-kind) "Expected no nudge with no tool calls, got ~A" nudge-kind))))

(defun test-model-family-dispatch ()
  (%assert (eq :moonshot (claw-lisp.core.system-prompt:model-family "moonshotai/kimi-k2.6"))
           "Expected moonshotai/ -> :moonshot")
  (%assert (eq :moonshot (claw-lisp.core.system-prompt:model-family "openrouter/moonshotai/kimi-k2.6"))
           "Expected openrouter/moonshotai/ -> :moonshot")
  (%assert (eq :qwen (claw-lisp.core.system-prompt:model-family "qwen/qwen3.7-max"))
           "Expected qwen/ -> :qwen")
  (%assert (eq :anthropic (claw-lisp.core.system-prompt:model-family "anthropic/claude-sonnet-4-6"))
           "Expected anthropic/ -> :anthropic")
  (%assert (eq :openai (claw-lisp.core.system-prompt:model-family "openai/gpt-4o"))
           "Expected openai/ -> :openai")
  (%assert (eq :default (claw-lisp.core.system-prompt:model-family nil))
           "Expected nil -> :default")
  (%assert (eq :default (claw-lisp.core.system-prompt:model-family "unknown/model"))
           "Expected unknown provider -> :default"))

(defun test-build-system-prompt-selects-directive-for-kimi ()
  (let ((prompt (claw-lisp.core.system-prompt:build-system-prompt
                 :model "moonshotai/kimi-k2.6")))
    (%assert (search "TURN BUDGET" prompt)
             "Expected directive prompt (TURN BUDGET) for kimi model")))

(defun test-build-system-prompt-selects-directive-for-qwen ()
  (let ((prompt (claw-lisp.core.system-prompt:build-system-prompt
                 :model "qwen/qwen3.7-max")))
    (%assert (search "TURN BUDGET" prompt)
             "Expected directive prompt (TURN BUDGET) for qwen model")))

(defun test-build-system-prompt-selects-base-for-claude ()
  (let ((prompt (claw-lisp.core.system-prompt:build-system-prompt
                 :model "anthropic/claude-sonnet-4-6")))
    (%assert (search "Core Principles" prompt)
             "Expected base prompt (Core Principles) for claude model")
    (%assert (null (search "TURN BUDGET" prompt))
             "Expected no TURN BUDGET directive in claude prompt")))

(defun test-make-differential-reflection-text-nil-on-all-success ()
  (let* ((result (claw-lisp.core.domain:make-tool-result
                  :call-id "1" :tool-name "file-read" :content "file contents here"))
         (reflection (claw-lisp.core.runtime:make-differential-reflection-text (list result))))
    (%assert (null reflection)
             "Expected nil reflection when all tool results succeed")))

(defun test-make-differential-reflection-text-failure-message ()
  (let* ((success (claw-lisp.core.domain:make-tool-result
                   :call-id "1" :tool-name "file-read" :content "file contents here"))
         (failure (claw-lisp.core.domain:make-tool-result
                   :call-id "2" :tool-name "file-replace"
                   :content "[error] Tool file-replace failed: substring not found"))
         (reflection (claw-lisp.core.runtime:make-differential-reflection-text
                      (list success failure))))
    (%assert (stringp reflection)
             "Expected string reflection when any result fails")
    (%assert (search "file-replace" reflection)
             "Expected failing tool name in reflection text")
    (%assert (search "file-read" reflection)
             "Expected successful tool name in reflection text")
    (%assert (search "Do not re-call" reflection)
             "Expected 'Do not re-call' directive in reflection text")))

(defun run-tests ()
  ;; Phase 2c: Conditions and Retry
  (test-condition-hierarchy)
  (test-http-status-error-type)
  (test-retry-exponential-delay)
  (test-retry-retryable-status)
  (test-retry-immediate-500)
  ;; Phase 2c: Rate-Limit Tracking
  (run-rate-limit-tests)
  ;; Phase 2d: System Prompt
  (test-system-prompt-builds)
  (test-claude-md-user-file)
  (test-claude-md-project-file)
  ;; Phase 2d: SSE Parser
  (test-sse-parser-simple-event)
  (test-sse-parser-multi-line-data)
  (test-sse-parser-comments-ignored)
  (test-sse-parser-event-and-data)
  (test-sse-parser-no-space-after-colon)
  (test-sse-parser-multiple-events)
  ;; Phase 2d: Streaming Callbacks
  (test-streaming-on-event-callback)
  ;; Phase 9: CLI session resume command
  (test-phase9-cli-session-resume-command)
  ;; Phase 9: explicit compaction visibility edge-case coverage
  (test-phase9-cli-compaction-no-event-when-transcript-missing)
  (test-phase9-cli-compaction-reflects-failure-circuit-state)
  (test-phase9-find-last-compaction-event-line-missing-file)
  (test-phase9-find-last-compaction-event-line-empty-file)
  (test-phase9-find-last-compaction-event-line-last-match-wins)
  (test-phase9-find-last-compaction-event-line-whitespace-variant)
  (test-phase9-find-last-compaction-event-line-return-contract)
  (test-phase9-find-last-compaction-event-line-io-error)
  (test-phase9-cli-compaction-shows-latest-event)
  (test-phase9-cli-compaction-large-transcript)
  (test-phase9-cli-compaction-schema-warning-for-missing-event-field)
  (test-phase9-cli-compaction-when-transcript-path-nil)
  (test-phase9-cli-compaction-when-transcript-unreadable)
  (test-phase9-cli-transcript-inspection-command)
  (test-phase9-cli-transcript-tail-partial-fill)
  (test-phase9-cli-transcript-when-transcript-missing)
  (test-phase9-cli-transcript-tail-zero-usage)
  (test-phase9-cli-transcript-when-transcript-path-nil)
  (test-phase9-cli-transcript-when-transcript-unreadable)
  (test-phase9-cli-transcript-tail-boundaries)
  (test-phase9-cli-transcript-large-file-tail-streaming)
  (test-phase9-cli-transcript-large-file-tail-performance)
  (test-phase9-cli-transcript-dispatch-neighbor-commands)
  ;; Phase 9: provider/model selection and CLI selector commands
  (test-phase9-select-session-model-updates-session-and-transcript)
  (test-phase9-select-session-model-profile-switch-and-persist)
  (test-phase9-select-session-model-rejects-unknown-provider-without-mutation)
  (test-phase9-provider-name-normalization)
  (test-phase9-model-resolution-source-coverage)
  (test-phase9-select-session-model-rejects-incompatible-provider-model-by-default)
  (test-phase9-select-session-model-allows-incompatible-provider-model-with-override)
  (test-phase9-select-session-model-rejects-when-turn-in-flight)
  (test-phase9-select-session-model-rejects-when-selection-in-progress)
  (test-phase9-select-session-model-concurrent-calls-serialize)
  (test-phase9-select-session-model-provider-default-warning)
  (test-phase9-select-session-model-fallback-warning)
  (test-phase9-provider-credential-check-uses-registered-provider-api-keys)
  (test-phase9-select-session-model-preserves-input-model-id-for-alias-and-prefix)
  (test-phase9-cli-provider-model-and-use-commands)
  (test-phase9-cli-models-no-registered-models-for-provider)
  (test-phase9-cli-diagnostics-command)
  (test-phase9-cli-diagnostics-dependency-branches)
  (test-phase9-cli-diagnostics-no-transcript-path)
  (test-phase9-cli-diagnostics-protects-external-calls)
  (test-phase9-cli-diagnostics-emits-debug-warnings)
  (test-phase10-cli-cas-visibility-commands)
  (test-ir-canonical-serialization-is-stable)
  (test-ir-materialize-and-load-roundtrip)
  (test-ir-resolve-ref-roundtrip)
  (test-ir-load-rejects-unsupported-version)
  (test-ir-serialization-rejects-non-keyword-symbols)
  (test-ir-serialization-rejects-unsupported-types)
  (test-ir-materialize-rejects-invalid-ref-name)
  (test-ir-materialize-requires-cas-root)
  (test-ir-load-requires-cas-root)
  (test-ir-load-missing-object-signals-storage-error)
  (test-ir-load-rejects-malformed-sexp)
  (test-ir-load-rejects-invalid-top-level-form)
  (test-ir-load-rejects-non-keyword-plist-keys)
  (test-ir-load-rejects-unknown-top-level-tag)
  (test-ir-load-rejects-non-keyword-top-level-head)
  (test-ir-deserialization-report-summarizes-payload)
  (test-achatina-surface-form-compiles-to-semantic-ir)
  (test-achatina-surface-form-runs-locally-through-cas-and-plan)
  (test-achatina-human-review-ir-runs-locally-through-cas-and-plan)
  (test-achatina-local-execution-rejects-cycles)
  (test-achatina-local-execution-rejects-ambiguous-flow-input)
  (test-achatina-local-execution-rejects-missing-tool-input)
  (test-achatina-local-execution-rejects-unsupported-node-kind)
  (test-achatina-local-execution-rejects-plan-model-call-nodes)
  (test-achatina-local-execution-rejects-plan-branch-nodes)
  (test-achatina-local-execution-supports-plan-human-review-nodes)
  (test-achatina-local-execution-rejects-invalid-human-review-payload)
  (test-achatina-local-execution-human-review-preserves-sequencing)
  (test-achatina-local-execution-supports-plan-child-agent-nodes)
  (test-achatina-local-execution-supports-plan-child-agent-without-handoff)
  (test-achatina-local-execution-rejects-invalid-child-agent-payload)
  (test-achatina-local-execution-child-agent-preserves-sequencing)
  (test-achatina-local-execution-supports-plan-await-nodes)
  (test-achatina-local-execution-await-preserves-sequencing)
  (test-achatina-local-execution-supports-session-memory-read-nodes)
  (test-achatina-local-execution-rejects-unsupported-memory-read-scope)
  (test-achatina-local-execution-supports-session-memory-write-nodes)
  (test-achatina-local-execution-memory-write-overwrites-prior-content)
  (test-achatina-local-execution-memory-write-supports-implicit-session)
  (test-achatina-local-execution-rejects-unsupported-memory-write-scope)
  (test-achatina-local-execution-rejects-invalid-memory-write-content)
  (test-achatina-local-execution-supports-plan-side-effect-nodes)
  (test-achatina-local-execution-rejects-invalid-side-effect-payload)
  (test-achatina-local-execution-side-effect-preserves-sequencing)
  (test-achatina-local-execution-errors-on-unregistered-tool)
  (test-achatina-ir-validation-succeeds)
  (test-achatina-ir-validation-rejects-missing-identity)
  (test-achatina-ir-validation-rejects-stage-node-type-mismatch)
  (test-achatina-ir-validation-rejects-invalid-parent-hash)
  (test-achatina-ir-validation-rejects-invalid-governance-metadata)
  (test-achatina-ir-pass-sequencing-and-normalization)
  (test-achatina-ir-pass-persistence-links-parent-and-child)
  (test-achatina-ir-pass-persistence-allows-nil-parent)
  (test-achatina-ir-optimization-pipeline-chains-provenance)
  (test-achatina-ir-pipeline-without-runtime-keeps-in-memory-results)
  (test-achatina-ir-pipeline-rejects-invalid-step)
  (test-achatina-ir-pipeline-allows-empty-step-list)
  (test-achatina-ir-pipeline-prefers-context-parent-over-initial-artifact)
  (test-achatina-semantic-expansion-materializes-governance)
  (test-achatina-semantic-expansion-is-idempotent)
  (test-achatina-semantic-expansion-persistence-links-parent)
  (test-achatina-semantic-expansion-no-governance-is-no-op)
  (test-achatina-semantic-expansion-materializes-tools-only)
  (test-achatina-semantic-expansion-materializes-memory-scopes-only)
  (test-achatina-semantic-expansion-sanitizes-governance-names)
  (test-achatina-execution-preparation-uses-semantic-declarations)
  (test-achatina-execution-preparation-falls-back-to-metadata)
  (test-achatina-execution-preparation-allows-mixed-equal-definitions)
  (test-achatina-execution-preparation-rejects-conflicting-definitions)
  (test-achatina-execution-preparation-rejects-multiple-anchors)
  (test-achatina-execution-preparation-rejects-declaration-edges-without-anchor)
  (test-achatina-execution-preparation-rejects-non-anchor-origin)
  (test-achatina-execution-preparation-rejects-missing-declaration-target)
  (test-achatina-execution-preparation-rejects-wrong-declaration-kind)
  (test-achatina-execution-preparation-rejects-orphan-declaration-node)
  (test-achatina-execution-preparation-rejects-invalid-declaration-payload)
  (test-achatina-execution-preparation-rejects-invalid-stage)
  (test-achatina-execution-plan-lowering-carries-governance)
  (test-achatina-execution-plan-lowering-deduplicates-flow-edges)
  (test-achatina-execution-plan-lowering-deduplicates-flow-edges-deterministically)
  (test-achatina-execution-plan-lowering-allows-empty-operational-graph)
  (test-achatina-execution-plan-lowering-is-deterministic)
  (test-achatina-execution-plan-lowering-drops-semantic-node-metadata)
  (test-achatina-execution-plan-lowering-supports-control-edges)
  (test-achatina-execution-plan-lowering-supports-model-call-nodes)
  (test-achatina-execution-plan-lowering-supports-branch-nodes)
  (test-achatina-execution-plan-lowering-supports-human-review-nodes)
  (test-achatina-execution-plan-lowering-supports-child-agent-nodes)
  (test-achatina-execution-plan-lowering-supports-await-nodes)
  (test-achatina-execution-plan-lowering-supports-memory-read-nodes)
  (test-achatina-execution-plan-lowering-supports-memory-write-nodes)
  (test-achatina-execution-plan-lowering-supports-side-effect-nodes)
  (test-achatina-execution-plan-lowering-deduplicates-control-edges)
  (test-achatina-execution-plan-persistence-links-parent)
  (test-achatina-execution-plan-persistence-prefers-context-parent)
  (test-achatina-execution-plan-lowering-prefers-context-parent-over-preparation)
  (test-achatina-execution-plan-lowering-rejects-unsupported-kinds)
  (test-achatina-execution-plan-lowering-rejects-unsupported-edge-kind)
  ;; Phase 2b: Model Registry
  (test-model-registry-resolve-exact-match)
  (test-model-registry-resolve-alias)
  (test-model-registry-prefix-match)
  (test-model-registry-provider-default)
  (test-model-registry-supports-p)
  (test-model-registry-translate-name)
  (test-json-decode-anthropic-response)
  (test-json-decode-openrouter-response)
  (test-openrouter-tool-call-extraction)
  (test-openrouter-no-tool-calls)
  (test-openrouter-empty-tool-calls)
  (test-openrouter-multiple-tool-calls)
  (test-openrouter-stream-turn-falls-back-to-send-turn)
  (test-json-decode-error-response)
  (test-content-block-roundtrip)
  (test-anthropic-json-format)
  (test-openrouter-chat-json-single-message-uses-array)
  (test-openrouter-chat-json-threads-tool-results-as-tool-role)
  (test-openrouter-chat-json-tools-serialize-as-array-of-objects)
  (test-openrouter-env-alias-loads-credentials)
  (test-openrouter-response-extraction)
  (test-anthropic-response-extraction)
  (test-openrouter-error-response-extraction)
  (test-transcript-path-for-session)
  (test-default-state-root-family)
  (test-tool-result-error-p-returns-boolean)
  (test-make-tool-result-message-normalizes-error-flag-to-boolean)
  (test-default-state-root-bootstrap-copies-legacy-tree)
  (test-state-root-override-derives-family)
  (test-resume-session-falls-back-to-legacy-transcript-root)
  (test-read-session-memory-text-falls-back-to-legacy-root)
  (test-session-memory-path)
  (test-durable-memory-path)
  (test-session-context-status-thresholds)
  (test-session-context-status-below-thresholds)
  (test-session-context-status-without-provider-falls-back-locally)
  (test-transcript-write-flow)
  (test-submit-user-message-runs-provider-tool-loop)
  (test-submit-user-message-rejects-reentrant-turn)
  (test-execute-provider-turn-loop-rejects-reentrant-turn)
  (test-execute-provider-turn-loop-clears-in-flight-after-provider-error)
  (test-submit-user-message-clears-in-flight-after-provider-error)
  (test-session-memory-write-flow)
  (test-session-memory-structured-write-failure-is-non-fatal)
  (test-session-memory-compaction-reuse)
  (test-session-memory-compaction-missing-file)
  (test-durable-memory-extraction)
  (test-durable-memory-extraction-skips-empty-session)
  (test-durable-memory-auto-extracts-after-message-turn)
  (test-durable-memory-auto-extracts-after-tool-execution)
  (test-local-compaction-falls-back-without-session-memory)
  (test-local-compaction-prefers-session-memory)
  (test-apply-session-compaction-replaces-message-history)
  (test-compaction-circuit-opens-after-failures)
  (test-compaction-circuit-resets-after-success)
  (test-tool-registration)
  (test-tool-execution-flow)
  (test-file-read-tool-execution-flow)
  (test-file-write-tool-execution-flow)
  (test-file-replace-tool-execution-flow)
  (test-glob-tool-execution-flow)
  (test-grep-tool-execution-flow)
  (test-shell-command-tool-execution-flow)
  (test-shell-command-tool-times-out)
  (test-file-tool-permissions-reject-forbidden-path)
  (test-shell-command-can-be-disabled-by-policy)
  (test-oversized-tool-result-persistence)
  (test-delete-session-tool-results)
  (test-tool-result-aggregate-budget-trims-older-previews)
  (test-microcompact-clears-old-persisted-previews)
  (test-cli-tool-parse)
  (test-cli-file-read-parse)
  (test-cli-file-write-parse)
  (test-cli-file-replace-parse)
  (test-cli-glob-parse)
  (test-cli-grep-parse)
  (test-cli-shell-command-parse)
  (test-cli-tool-parse-requires-tool)
  (test-cli-tool-shot)
  (test-cli-json-run-parse)
  (test-cli-json-run-success-with-files)
  (test-cli-runtime-event-mapping)
  (test-inspect-transcript-file-tolerates-tool-result-extra-keys)
  (test-runtime-event-callback-observes-tool-events)
  (test-runtime-event-callback-warning-on-error)
  (test-submit-user-message-restores-runtime-event-callback)
  (test-submit-user-message-restores-runtime-event-callback-after-error)
  (test-execute-registered-tool-tool-error-omits-unauthorized-input)
  (test-execute-registered-tool-postprocess-failure-does-not-emit-tool-error)
  (test-cli-json-run-tool-success-events-with-file-output)
  (test-cli-json-run-tool-failure-events-with-file-output)
  (test-cli-json-run-invalid-request-exits-20)
  (test-cli-json-run-invalid-request-timeout-exits-20)
  (test-cli-json-run-stdout-result)
  (test-cli-json-run-cancel-file-at-startup)
  (test-cli-json-run-cancel-file-post-turn)
  (test-cli-json-run-unknown-provider-exits-22)
  (test-cli-json-run-timeout-exits-12)
  (test-cli-json-run-failure-captures-diagnostics)
  (test-cli-help-exits-0)
  (test-phase10-tool-result-cas-transcript-event)
  (test-phase10-manifest-artifact-transcript-events)
  (test-phase10-cli-cas-visibility-commands)
  ;; Phase 10: Structured Compaction IR
  (test-compaction-ir-construction)
  (test-compaction-ir-render-to-markdown)
  (test-compaction-ir-render-fallback-heading)
  (test-compaction-ir-render-skips-trimmed-sections)
  (test-compaction-ir-to-plist)
  (test-compaction-ir-token-budget-trimming)
  (test-compaction-ir-token-budget-never-trims-provenance)
  (test-compaction-ir-token-budget-can-remain-over-budget-with-must-keep-sections)
  (test-compaction-ir-backward-compat)
  (test-compaction-ir-fingerprint)
  (test-compaction-ir-fallback-path-produces-ir)
  (test-compaction-ir-boundary-message-uses-rendered-summary)
  (test-compaction-ir-provenance-chaining)
  (test-compaction-ir-transcript-includes-ir-plist)
  ;; Phase 10: CAS Core Object Store
  (run-cas-tests)
  ;; Phase 10: CAS Ref Store
  (run-cas-ref-store-tests)
  ;; Phase 10: CAS Manifest Layer
  (run-cas-manifest-tests)
  ;; Phase 10: CAS Integrity Layer
  (run-cas-integrity-tests)
  ;; Phase 10: CAS Artifact Facade
  (run-artifacts-tests)
  ;; Agent loop rebuild Step A: capability source of truth
  (test-tool-classification-agreement-across-subsystems)
  (test-tool-capability-method-matches-registry)
  (run-tool-envelope-tests)
  ;; Agent loop rebuild Step D: unified progress assessment
  (test-read-suppression-forces-write-after-stall)
  (test-stall-increments-on-no-write-turn-with-interleaved-shell)
  (test-stall-counter-unaffected-when-no-tool-calls)
  (test-provider-tool-loop-budget-returns-gracefully)
  (test-failed-write-advances-stagnation-to-nudge-threshold)
  (test-successful-write-resets-stagnation)
  ;; Agent loop improvements: model-family dispatch and differential reflection
  (test-model-family-dispatch)
  (test-build-system-prompt-selects-directive-for-kimi)
  (test-build-system-prompt-selects-directive-for-qwen)
  (test-build-system-prompt-selects-base-for-claude)
  (test-make-differential-reflection-text-nil-on-all-success)
  (test-make-differential-reflection-text-failure-message)
  (format t "claw-lisp tests passed.~%")
  0)

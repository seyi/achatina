(in-package #:claw-lisp.cli)

;; --- CLI Entry Point ---
;;
;; Provides a REPL (Read-Eval-Print Loop) for interacting with Claw.
;;
;; Usage:
;;   sbcl --load achatina-cli.asd --eval "(claw-lisp.cli:run-cli)"
;;
;; Or from the Makefile:
;;   make repl

(defparameter +cli-version+ "0.1.0"
  "Claw Lisp CLI version.")

(defparameter +cli-welcome-message+
  "Welcome to Achatina CLI ~A.
Type your message and press Enter. Use :help for commands.
Type :quit or Ctrl+D to exit.~%"
  "Welcome message displayed at CLI startup.")

(defparameter +cli-prompt+ "> "
  "The input prompt string.")

(defparameter +transcript-default-tail-count+ 10
  "Default line count for `:transcript tail` when no explicit N is provided.")

(defparameter +transcript-max-tail-count+ 200
  "Maximum line count accepted by `:transcript tail N`.")

(defparameter +transcript-tail-line-preview-max-len+ 240
  "Maximum displayed characters for one transcript tail line preview.")

(defparameter +runner-protocol-version+ "claw-runner/v0"
  "Current non-interactive runner protocol version.")

(defparameter +cli-commands+
  '((":quit" . "Exit the CLI")
    (":exit" . "Exit the CLI")
    (":help" . "Show this help message")
    (":status" . "Show runtime status")
    (":tools" . "List available tools")
    (":providers" . "List available providers")
    (":provider" . "Show current provider")
    (":provider <provider-name>" . "Set current provider")
    (":models" . "List known models for current provider")
    (":models <provider-name>" . "List known models for a provider")
    (":memory" . "Show session memory status")
    (":memory-content" . "Show session memory contents")
    (":clear" . "Clear the conversation")
    (":model" . "Show current model")
    (":model <model-id>" . "Set current model")
    (":use <provider-name> <model-id> [profile]" . "Atomically set provider, model, and optional profile")
    (":config" . "Show runtime configuration")
    (":diagnostics" . "Show runtime diagnostics and dependency checks")
    (":compaction" . "Show compaction status and latest compaction event")
    (":transcript" . "Show transcript summary for current session")
    (":transcript tail [n]" . "Show last n transcript lines (default 10, max 200)")
    (":resume <session-id>" . "Resume a prior session from transcript")
    (":tasks" . "Show background task status buckets [active|completed|ended|other]")
    (":agents" . "List child agent progress")
    (":agent <child-id>" . "Show one child agent detail")
    (":cas" . "Show CAS status")
    (":cas object <hash>" . "Inspect a CAS object by versioned hash")
    (":cas ref <ref-name>" . "Inspect a CAS ref and its target status")
    (":cas manifest <hash>" . "Inspect and verify a CAS manifest"))
  "Available CLI commands and their descriptions.")

(defun %command-dispatch (command keyword)
  "Return two values: matched-p and trailing argument string (or NIL).
Matches KEYWORD case-insensitively, with optional whitespace-separated args."
  (let* ((trimmed (string-trim '(#\Space #\Tab) command))
         (len (length keyword)))
    (cond
      ((string-equal trimmed keyword)
       (values t nil))
      ((and (> (length trimmed) len)
            (string-equal keyword (subseq trimmed 0 len))
            (member (char trimmed len) '(#\Space #\Tab) :test #'char=))
       (values t (string-trim '(#\Space #\Tab) (subseq trimmed len))))
      (t
       (values nil nil)))))

(defun %format-selection-error (operation condition)
  "Print OPERATION-specific selection error with friendlier busy-session text."
  (let ((message (princ-to-string condition)))
    (cond
      ((or (search "busy executing a turn" message :test #'char-equal)
           (search "busy updating provider/model configuration" message :test #'char-equal))
       (format t "~A failed: Session is busy. Wait for the current operation to finish and retry.~%"
               operation))
      (t
       (format t "~A failed: ~A~%" operation condition)))))

(defun %find-child-handle (runtime session child-id)
  "Return child handle for CHILD-ID under SESSION, or NIL."
  (find child-id
        (claw-lisp.core.runtime:list-child-agents runtime session)
        :key #'claw-lisp.core.domain:child-agent-handle-child-id
        :test #'string=))

(defun %print-child-snapshot-line (snapshot)
  "Print one-line summary for SNAPSHOT."
  (format t "  - ~A  status=~A  messages=~D  tools=~D~%"
          (claw-lisp.core.domain:child-progress-snapshot-child-id snapshot)
          (claw-lisp.core.domain:child-progress-snapshot-status snapshot)
          (claw-lisp.core.domain:child-progress-snapshot-messages-count snapshot)
          (claw-lisp.core.domain:child-progress-snapshot-tool-calls-count snapshot)))

(defun %background-status-bucket (status)
  "Map child STATUS to a background task bucket label."
  (cond
    ((member status '(:running :starting :restarting) :test #'eq) "active")
    ((member status '(:completed) :test #'eq) "completed")
    ((member status '(:cancelled :timed-out :failed) :test #'eq) "ended")
    (t "other")))

(defun %file-size-bytes (path)
  "Return file size in bytes for PATH, or NIL if unavailable."
  (when (probe-file path)
    (with-open-file (stream path :direction :input :element-type '(unsigned-byte 8))
      (file-length stream))))

(defun %universal-time-to-unix-seconds (universal-time)
  "Convert Common Lisp UNIVERSAL-TIME to Unix epoch seconds."
  (- universal-time 2208988800))

(defun %find-last-compaction-event-line (transcript-path)
  "Return three values: last event line, I/O error text, and parsed JSON lines missing :event."
  (let ((summary (claw-lisp.core.runtime::inspect-transcript-file transcript-path)))
    (values (getf summary :last-compaction-event-line)
            (getf summary :error)
            (getf summary :missing-event-count 0))))

(defun %truncate-line (line &optional (max-len 160))
  "Return LINE truncated to MAX-LEN characters with ellipsis when needed."
  (if (and line (> (length line) max-len))
      (concatenate 'string (subseq line 0 max-len) "...")
      line))

(defun %split-cli-args (raw)
  (when raw
    (remove-if (lambda (part) (zerop (length part)))
               (uiop:split-string (string-trim '(#\Space #\Tab) raw)
                                  :separator '(#\Space #\Tab)))))

(defun %iso8601-from-universal-time (universal-time)
  "Return UNIVERSAL-TIME in RFC3339/ISO8601 UTC form."
  (multiple-value-bind (second minute hour day month year)
      (decode-universal-time universal-time 0)
    (format nil "~4,'0D-~2,'0D-~2,'0DT~2,'0D:~2,'0D:~2,'0DZ"
            year month day hour minute second)))

(defun %iso8601-now ()
  "Return the current UTC time in RFC3339/ISO8601 form."
  (%iso8601-from-universal-time (get-universal-time)))

(defun %parse-positive-integer-string (value)
  "Return VALUE parsed as a non-negative integer, or NIL on failure."
  (handler-case
      (let ((parsed (parse-integer value :junk-allowed nil)))
        (when (>= parsed 0)
          parsed))
    (error () nil)))

(defun %plist-boolean-p (value)
  "Return true when VALUE is truthy in the request plist sense."
  (not (null value)))

(defun %plist-string (plist key)
  "Return string value for KEY in PLIST, or NIL."
  (let ((value (getf plist key)))
    (and (stringp value) value)))

(defun %write-json-object (stream value)
  "Write VALUE as one JSON object to STREAM."
  (write-string (claw-lisp.providers.http-utils:json-encode-string value) stream)
  (finish-output stream))

(defun %write-json-line (stream value)
  "Write VALUE as one line-delimited JSON object to STREAM."
  (%write-json-object stream value)
  (terpri stream)
  (finish-output stream))

(defun %ensure-parent-directory (path)
  "Create PATH's parent directory when needed."
  (let* ((pathname (pathname path))
         (directory-path (make-pathname :name nil :type nil :defaults pathname)))
    (ensure-directories-exist directory-path)))

(defun parse-args (argv)
  "Parse CLI argument vector ARGV into a plist.

Recognizes both legacy single-tool flags used by older tests and the new
non-interactive runner flags."
  (labels ((need-value (flag rest)
             (unless rest
               (return-from parse-args
                 (list :error (format nil "Missing value for ~A" flag))))
             (values (first rest) (rest rest)))
           (set-flag (plist key value)
             (append plist (list key value))))
    (loop with args = (if (and argv (string= (first argv) "--"))
                          (rest argv)
                          argv)
          with parsed = nil
          while args
          do (let ((arg (first args)))
               (setf args (rest args))
               (cond
                 ((string= arg "--")
                  (setf args nil))
                 ((string= arg "--help")
                  (setf parsed (set-flag parsed :help t)))
                 ((string= arg "--json-run")
                  (setf parsed (set-flag parsed :json-run t)))
                 ((member arg '("--request-file" "--result-file" "--event-file" "--cancel-file"
                                "--timeout-seconds" "--provider" "--model" "--project-root"
                                "--tool" "--tool-text" "--tool-path" "--tool-match")
                          :test #'string=)
                  (multiple-value-bind (value rest-args) (need-value arg args)
                    (setf args rest-args)
                    (setf parsed
                          (set-flag parsed
                                    (intern (string-upcase (subseq arg 2)) "KEYWORD")
                                    value))))
                 (t
                  (return-from parse-args
                    (list :error (format nil "Unknown argument: ~A" arg))))))
          finally
             (when (and (or (getf parsed :tool-text)
                            (getf parsed :tool-path)
                            (getf parsed :tool-match))
                        (null (getf parsed :tool)))
               (return (list :error "--tool is required when using --tool-text, --tool-path, or --tool-match")))
             (when (getf parsed :timeout-seconds)
               (let ((parsed-timeout
                       (%parse-positive-integer-string (getf parsed :timeout-seconds))))
                 (when (null parsed-timeout)
                   (return (list :error "--timeout-seconds must be a non-negative integer")))
                 (setf (getf parsed :timeout-seconds) parsed-timeout)))
             (return parsed))))

(defun print-usage ()
  "Print top-level CLI usage."
  (format t "Usage: achatina [--json-run [runner flags]]~%")
  (format t "       achatina            ; interactive REPL~%")
  (format t "General flags:~%")
  (format t "  --help~%")
  (format t "Runner flags:~%")
  (format t "  --request-file <path>~%")
  (format t "  --result-file <path>~%")
  (format t "  --event-file <path>~%")
  (format t "  --cancel-file <path>~%")
  (format t "  --timeout-seconds <n>~%")
  (format t "  --provider <name>~%")
  (format t "  --model <id>~%")
  (format t "  --project-root <path>~%")
  (format t "Legacy single-tool flags:~%")
  (format t "  --tool <name>~%")
  (format t "  --tool-text <text>~%")
  (format t "  --tool-path <path>~%")
  (format t "  --tool-match <pattern>~%"))

(defun %make-runner-failure (category code message &key retryable details)
  "Build a structured failure payload."
  (list :category category
        :code code
        :message message
        :retryable (%plist-boolean-p retryable)
        :details details))

(defun %runner-exit-code-for-status (status)
  "Return the protocol exit code for STATUS."
  (cond
    ((string= status "succeeded") 0)
    ((string= status "failed") 10)
    ((string= status "cancelled") 11)
    ((string= status "timed_out") 12)
    ((string= status "human_input_required") 13)
    (t 22)))

(defun %runner-base-fields (request)
  "Return the standard identity fields copied from REQUEST."
  (let ((fields
          (list :protocol_version +runner-protocol-version+
                :request_id (getf request :request_id)
                :correlation_id (getf request :correlation_id)
                :job_id (getf request :job_id)
                :task_id (getf request :task_id)
                :node_id (getf request :node_id)
                :session_id (getf request :session_id))))
    (let ((turn-id (getf request :turn_id)))
      (if turn-id
          (append fields (list :turn_id turn-id))
          fields))))

(defun %emit-runner-event (stream request event-id event-type &key payload sequence parent-event-id)
  "Write one protocol event to STREAM when STREAM is non-NIL."
  (when stream
    (let ((event (append (%runner-base-fields request)
                         (list :event_id event-id
                               :event_type event-type
                               :timestamp (%iso8601-now))
                         (when sequence (list :sequence sequence))
                         (when parent-event-id (list :parent_event_id parent-event-id))
                         (when payload (list :payload payload)))))
      (%write-json-line stream event))))

(defun %normalize-runtime-status (status)
  "Return STATUS as a stable lowercase string when possible."
  (when status
    (string-downcase
     (etypecase status
       (string status)
       (symbol (string status))))))

(defun %runtime-tool-result-event-type (event)
  "Return the runner event type for one runtime tool_result EVENT.

Malformed tool_result payloads fail closed as tool.failed so runner consumers
do not silently treat unknown outcomes as success."
  (let* ((result (getf event :result))
         (sentinel (list :missing))
         (is-error (if (listp result)
                       (getf result :is_error sentinel)
                       sentinel)))
    (cond
      ((eq is-error sentinel)
       (warn "Runtime tool_result event missing :is_error classification: ~S" event)
       "tool.failed")
      (is-error
       "tool.failed")
      (t
       "tool.completed"))))

(defun %runtime-event->runner-events (event)
  "Map one raw runtime EVENT plist to zero or more runner protocol events.

Events without a protocol mapping, such as budget-trim and compaction-boundary
observability records, are intentionally dropped in runner v0."
  (let ((event-name (getf event :event)))
    (cond
      ((string= event-name "tool_start")
       (list (list :event_type "tool.started"
                   :payload (list :call_id (getf event :call_id)
                                  :tool_name (getf event :tool_name)
                                  :input (getf event :input)))))
      ((string= event-name "tool_result")
       (let ((result (getf event :result)))
         (list (list :event_type (%runtime-tool-result-event-type event)
                     :payload (list :call_id (getf event :call_id)
                                    :tool_name (getf event :tool_name)
                                    :input (getf event :input)
                                    :result result)))))
      ((string= event-name "tool_error")
       (list (list :event_type "tool.failed"
                   :payload (list :call_id (getf event :call_id)
                                  :tool_name (getf event :tool_name)
                                  :input (getf event :input)
                                  :error (getf event :error)))))
      ((string= event-name "durable_memory_extract")
       (list (list :event_type "memory.extracted"
                   :payload (list :saved_count (getf event :saved_count)))))
      ((string= event-name "microcompact")
       (list (list :event_type "context.microcompacted"
                   :payload (list :cleared_count (getf event :cleared_count)
                                  :keep_recent (getf event :keep_recent)))))
      ((string= event-name "child_spawned")
       (list (list :event_type "child_agent.spawned"
                   :payload (list :child_id (getf event :child_id)
                                  :status (getf event :status)
                                  :child_transcript_path (getf event :child_transcript_path)))))
      ((string= event-name "child_finished")
       (let ((status-label (%normalize-runtime-status (getf event :status))))
         (list (list :event_type (cond
                                   ((and status-label (string= status-label "completed"))
                                    "child_agent.completed")
                                   ((and status-label (string= status-label "failed"))
                                    "child_agent.failed")
                                   (t
                                    "child_agent.progress"))
                   :payload (list :child_id (getf event :child_id)
                                  :status (getf event :status)
                                  :error (getf event :error)
                                  :child_transcript_path
                                  (getf event :child_transcript_path))))))
      (t nil))))

(defun %runner-request-timeout-seconds (request cli-timeout)
  "Return the effective timeout seconds from REQUEST and CLI-TIMEOUT."
  (or cli-timeout
      (let* ((timeouts (getf request :timeouts))
             (request-timeout (and (listp timeouts)
                                   (getf timeouts :turn_timeout_seconds))))
        (and (integerp request-timeout)
             (>= request-timeout 0)
             request-timeout))))

(defun %validate-runner-request (request)
  "Return NIL when REQUEST is valid, otherwise a structured failure plist."
  (labels ((required-string (key)
             (let ((value (%plist-string request key)))
               (and value (> (length value) 0))))
           (valid-request-timeout-p ()
             (let ((timeouts (getf request :timeouts)))
               (cond
                 ((null timeouts) t)
                 ((not (listp timeouts))
                  (fail "invalid_timeout"
                        "timeouts must be an object when present."))
                 (t
                  (let ((request-timeout (getf timeouts :turn_timeout_seconds)))
                    (if (or (null request-timeout)
                            (and (integerp request-timeout)
                                 (>= request-timeout 0)))
                        t
                        (fail "invalid_timeout"
                              "turn_timeout_seconds must be a non-negative integer when present."
                              (list :actual request-timeout))))))))
           (fail (code message &optional details)
             (%make-runner-failure "protocol_error" code message :details details)))
    (let ((timeout-validation (and (listp request)
                                   (valid-request-timeout-p))))
      (cond
        ((not (listp request))
         (fail "invalid_request" "Runner request must decode to a plist/object."))
        ((not (%plist-string request :protocol_version))
         (fail "missing_protocol_version" "Request is missing protocol_version."))
        ((not (string= (%plist-string request :protocol_version) +runner-protocol-version+))
         (fail "unsupported_protocol_version"
               "Unsupported protocol_version."
               (list :expected +runner-protocol-version+
                     :actual (getf request :protocol_version))))
        ((not (required-string :request_id))
         (fail "missing_request_id" "Request is missing request_id."))
        ((not (required-string :correlation_id))
         (fail "missing_correlation_id" "Request is missing correlation_id."))
        ((not (required-string :job_id))
         (fail "missing_job_id" "Request is missing job_id."))
        ((not (required-string :task_id))
         (fail "missing_task_id" "Request is missing task_id."))
        ((not (required-string :node_id))
         (fail "missing_node_id" "Request is missing node_id."))
        ((not (required-string :session_id))
         (fail "missing_session_id" "Request is missing session_id."))
        ((not (required-string :user_input))
         (fail "missing_user_input" "Request is missing user_input."))
        ((not (eq t timeout-validation))
         timeout-validation)
        ((let ((provider (getf request :provider)))
           (or (null provider) (stringp provider)))
         (let ((model (getf request :model)))
           (if (or (null model) (stringp model))
               nil
               (fail "invalid_model" "model must be a string when present."))))
        (t
         (fail "invalid_provider" "provider must be a string when present."))))))

(defun %read-runner-request (request-file stdin)
  "Read and decode one runner request from REQUEST-FILE or STDIN."
  (let ((raw-text (if request-file
                      (uiop:read-file-string request-file)
                      (with-output-to-string (content)
                        (loop for line = (read-line stdin nil nil)
                              while line
                              do (progn
                                   (write-string line content)
                                   (terpri content)))))))
    (claw-lisp.providers.http-utils:json-decode raw-text)))

(defun %request-environment-path (request key)
  "Return a namestring override from REQUEST environment KEY, or NIL."
  (let* ((environment (getf request :environment))
         (value (and (listp environment) (getf environment key))))
    (and (stringp value) value)))

(defun %build-runner-config (request &key provider model cwd)
  "Build runtime config for one runner invocation."
  (let ((config (claw-lisp.config:load-runtime-config
                 :overrides (list :default-provider provider
                                  :default-model model))))
    (let ((state-root (%request-environment-path request :state_root))
          (data-root (%request-environment-path request :data_root))
          (transcripts-root (%request-environment-path request :transcript_root))
          (memory-root (%request-environment-path request :memory_root))
          (artifacts-root (%request-environment-path request :artifacts_root))
          (cas-objects-root (%request-environment-path request :cas_objects_root))
          (cas-ref-root (%request-environment-path request :cas_ref_root)))
      (when state-root
        (claw-lisp.config:apply-state-root config state-root))
      (when data-root
        (claw-lisp.config:apply-state-root config data-root))
      (when transcripts-root
        (setf (claw-lisp.config:runtime-config-transcripts-root config) transcripts-root))
      (when memory-root
        (setf (claw-lisp.config:runtime-config-memory-root config) memory-root))
      (when artifacts-root
        (setf (claw-lisp.config:runtime-config-artifacts-root config) artifacts-root))
      (when cas-objects-root
        (setf (claw-lisp.config:runtime-config-cas-objects-root config) cas-objects-root))
      (when cas-ref-root
        (setf (claw-lisp.config:runtime-config-cas-ref-root config) cas-ref-root)))
    (when cwd
      (setf (claw-lisp.config:runtime-config-tool-allowed-roots config)
            (list (uiop:ensure-directory-pathname cwd)
                  #P"/tmp/")))
    config))

(defun %runner-find-last-assistant-message (session)
  "Return the most recent assistant message in SESSION, or NIL."
  (find :assistant
        (reverse (claw-lisp.core.domain:conversation-messages
                  (claw-lisp.core.domain:agent-session-conversation session)))
        :key #'claw-lisp.core.domain:message-role
        :test #'eq))

(defun %runner-memory-paths (runtime session)
  "Return durable memory artifact paths for SESSION."
  (let ((path (claw-lisp.core.runtime:session-memory-path-for-session runtime session)))
    (if (and path (probe-file path))
        (list (namestring path))
        '())))

(defun %make-runner-result (request session runtime status started-universal-time failure)
  "Build the final runner result envelope."
  (let* ((completed-universal-time (get-universal-time))
         (started-unix (- started-universal-time 2208988800))
         (completed-unix (- completed-universal-time 2208988800))
         (assistant-message (%runner-find-last-assistant-message session))
         (assistant-text (and assistant-message
                              (claw-lisp.core.domain:message-content-text assistant-message)))
         (transcript-path (claw-lisp.core.runtime:session-transcript-path runtime session))
         (message (and assistant-text
                       (list :role "assistant"
                             :content (list (list :type "text" :text assistant-text))))))
    (append (%runner-base-fields request)
            (list :status status
                  :message message
                  ;; Usage metrics are not yet populated in runner v0.
                  ;; Downstream consumers should treat NIL as "not available yet",
                  ;; not as a zero-usage guarantee.
                  :usage nil
                  :artifacts (list :transcript_path
                                   (and transcript-path (namestring transcript-path))
                                   :memory_paths (%runner-memory-paths runtime session)
                                   :artifact_refs nil)
                  :timing (list :started_at (%iso8601-from-universal-time started-universal-time)
                                :completed_at (%iso8601-from-universal-time completed-universal-time)
                                :duration_ms (* 1000 (max 0 (- completed-unix started-unix))))
                  :failure failure))))

(defun %runner-cancel-file-present-p (cancel-file)
  "Return true when CANCEL-FILE exists."
  (and cancel-file (probe-file cancel-file)))

(defun %submit-runner-user-message (runtime session request cwd timeout-seconds
                                     &key on-event runtime-event-callback)
  "Submit one runner user message, optionally under an internal timeout."
  #+sbcl
  (if timeout-seconds
      (sb-ext:with-timeout timeout-seconds
        (uiop:with-current-directory (cwd)
          (claw-lisp.core.runtime:submit-user-message
           runtime session (getf request :user_input)
           :on-event on-event
           :runtime-event-callback runtime-event-callback)))
      (uiop:with-current-directory (cwd)
        (claw-lisp.core.runtime:submit-user-message
         runtime session (getf request :user_input)
         :on-event on-event
         :runtime-event-callback runtime-event-callback)))
  #-sbcl
  (progn
    (declare (ignore timeout-seconds))
    (uiop:with-current-directory (cwd)
      (claw-lisp.core.runtime:submit-user-message
       runtime session (getf request :user_input)
       :on-event on-event
       :runtime-event-callback runtime-event-callback))))

(defun %runner-captured-diagnostics (stream)
  "Return captured diagnostic text from STREAM, or NIL when empty."
  (let ((content (get-output-stream-string stream)))
    (unless (zerop (length content))
      content)))

(defun %runner-failure-with-diagnostics (failure diagnostics)
  "Attach DIAGNOSTICS text to FAILURE details when available."
  (if (or (null failure) (null diagnostics))
      failure
      (let ((details (getf failure :details)))
        (append failure
                (list :details
                      (append details
                              (list :stderr diagnostics)))))))

(defun %run-json-request (request options stdin stdout stderr)
  "Execute one runner request and return the protocol exit code."
  (declare (ignore stdin))
  (let* ((validation-error (%validate-runner-request request))
         (request-file (getf options :request-file))
         (result-file (getf options :result-file))
         (event-file (getf options :event-file))
         (cancel-file (getf options :cancel-file))
         (cli-timeout (getf options :timeout-seconds)))
    (declare (ignore request-file))
    (when validation-error
      (format stderr "~A~%" (getf validation-error :message))
      (return-from %run-json-request 20))
    (let* ((provider (or (%plist-string request :provider)
                         (getf options :provider)
                         "anthropic"))
           (model (or (%plist-string request :model)
                      (getf options :model)
                      "claude-sonnet-4-6"))
           (cwd (or (%plist-string request :cwd)
                    (getf options :project-root)
                    (uiop:getcwd)))
           (event-stream-enabled-p
             (or (getf options :event-file)
                 (%plist-boolean-p
                  (let ((features (getf request :features)))
                    (and (listp features) (getf features :event_stream))))))
           (timeout-seconds (%runner-request-timeout-seconds request cli-timeout))
           (started-universal-time (get-universal-time))
           (event-counter 0)
           (suppressed-errors (make-string-output-stream)))
      (labels ((next-event-id ()
                 (incf event-counter)
                 (format nil "evt_~D" event-counter))
               (run-with-targets (event-target result-target)
                 (let ((*error-output* suppressed-errors))
                   (execute-turn event-target result-target)))
               (execute-turn (event-target result-target)
                 (labels ((emit (event-type &key payload)
                            (when event-stream-enabled-p
                              (%emit-runner-event event-target request (next-event-id) event-type
                                                  :payload payload)))
                          (emit-runtime-event (event)
                            (dolist (mapped (%runtime-event->runner-events event))
                              (emit (getf mapped :event_type)
                                    :payload (getf mapped :payload)))))
                   (let* ((config (%build-runner-config request :provider provider :model model :cwd cwd))
                          (runtime (claw-lisp.core.runtime:make-runtime
                                    :config config
                                    :project-root cwd))
                          (session nil)
                          (status "succeeded")
                          (failure nil))
                     (claw-lisp.core.runtime:register-default-providers runtime)
                     (claw-lisp.core.runtime:register-default-tools runtime)
                     (unless (claw-lisp.core.runtime:resolve-provider runtime provider)
                       (error "Unknown or unconfigured provider: ~A" provider))
                     (setf session
                           (if (and (let ((resume (getf request :resume)))
                                      (and (listp resume)
                                           (%plist-boolean-p (getf resume :load_existing_session))))
                                    (probe-file
                                     (claw-lisp.storage.transcripts:transcript-path-for-session
                                      config (getf request :session_id))))
                               (claw-lisp.core.runtime:resume-session
                                runtime (getf request :session_id)
                                :provider-name provider
                                :model model)
                               (claw-lisp.core.runtime:start-session
                                runtime
                                :provider-name provider
                                :model model
                                :session-id (getf request :session_id))))
                     (when (%runner-cancel-file-present-p cancel-file)
                       (emit "runner.cancel_requested"
                             :payload (list :cancel_file cancel-file :phase "startup"))
                       (setf status "cancelled"
                             failure (%make-runner-failure
                                      "cancelled" "cancel_file_present"
                                      "Cancellation requested before execution."
                                      :details (list :cancel_file cancel-file))))
                     (emit "session.started" :payload (list :provider provider :model model))
                     (emit "turn.started" :payload (list :session_id (getf request :session_id)))
                     (when (string= status "succeeded")
                       (handler-case
                           (%submit-runner-user-message
                            runtime session request cwd timeout-seconds
                            :runtime-event-callback #'emit-runtime-event)
                         #+sbcl
                         (sb-ext:timeout ()
                           (setf status "timed_out"
                                 failure (%make-runner-failure
                                          "timed_out" "turn_timeout"
                                          "Runner turn timed out."
                                          :details (list :timeout_seconds timeout-seconds)))
                           (emit "runner.timed_out"
                                 :payload (list :timeout_seconds timeout-seconds)))
                         (error (condition)
                           (setf status "failed"
                                 failure (%make-runner-failure
                                          "internal_error" "runner_execution_failed"
                                          (princ-to-string condition))))))
                     (when (and (string= status "succeeded")
                                (%runner-cancel-file-present-p cancel-file))
                       (setf status "cancelled"
                             failure (%make-runner-failure
                                      "cancelled" "cancel_file_present"
                                      "Cancellation requested during execution."
                                      :details (list :cancel_file cancel-file)))
                       (emit "runner.cancel_requested"
                             :payload (list :cancel_file cancel-file :phase "post-turn")))
                     (setf failure
                           (%runner-failure-with-diagnostics
                            failure
                            (%runner-captured-diagnostics suppressed-errors)))
                     (let* ((result (%make-runner-result request session runtime status
                                                         started-universal-time failure))
                            (event-name (if (string= status "succeeded")
                                            "turn.completed"
                                            "turn.failed")))
                       (emit event-name :payload (list :status status))
                       (emit (if (string= status "succeeded")
                                 "session.completed"
                                 "session.failed")
                             :payload (list :status status))
                       (%write-json-object result-target result)
                       (%runner-exit-code-for-status status))))))
        (cond
          (event-file
           (%ensure-parent-directory event-file)
           (with-open-file (event-target event-file
                                         :direction :output
                                         :if-exists :supersede
                                         :if-does-not-exist :create)
             (if result-file
                 (progn
                   (%ensure-parent-directory result-file)
                   (with-open-file (result-target result-file
                                                 :direction :output
                                                 :if-exists :supersede
                                                 :if-does-not-exist :create)
                     (run-with-targets event-target result-target)))
                 (run-with-targets event-target stdout))))
          (result-file
           (%ensure-parent-directory result-file)
           (with-open-file (result-target result-file
                                         :direction :output
                                         :if-exists :supersede
                                         :if-does-not-exist :create)
             (run-with-targets (and event-stream-enabled-p stderr) result-target)))
          (t
           (run-with-targets (and event-stream-enabled-p stderr) stdout)))))))

(defun %dispatch-cli (args &key (stdin *standard-input*) (stdout *standard-output*) (stderr *error-output*))
  "Dispatch one CLI invocation and return an exit code."
  (let ((parsed (parse-args args)))
    (when (getf parsed :error)
      (format stderr "~A~%" (getf parsed :error))
      (return-from %dispatch-cli 1))
    (cond
      ((getf parsed :help)
       (let ((*standard-output* stdout))
         (print-usage))
       0)
      ((getf parsed :json-run)
       (let ((request
               (handler-case
                   (%read-runner-request (getf parsed :request-file) stdin)
                 (error (condition)
                   (format stderr "~A~%" condition)
                   (return-from %dispatch-cli 20)))))
         (handler-case
             (%run-json-request request parsed stdin stdout stderr)
           (error (condition)
             (format stderr "~A~%" condition)
             22))))
      (t
       (run-cli :provider (or (getf parsed :provider) "anthropic")
                :model (or (getf parsed :model) "claude-sonnet-4-6")
                :project-root (getf parsed :project-root))
       0))))

(defun %cas-root-display (root)
  (if root
      (namestring (uiop:ensure-directory-pathname root))
      "(none)"))

(defun %print-cas-status (runtime)
  (let ((cas-root (claw-lisp.core.artifacts:runtime-effective-cas-root runtime))
        (ref-root (claw-lisp.core.artifacts:runtime-effective-cas-ref-root runtime))
        (config (claw-lisp.core.runtime:runtime-settings runtime)))
    (format t "CAS status:~%")
    (format t "  Objects root: ~A~%" (%cas-root-display cas-root))
    (format t "  Objects root exists: ~A~%" (if (and cas-root (uiop:directory-exists-p cas-root))
                                                 "yes"
                                                 "no"))
    (format t "  Refs root: ~A~%" (%cas-root-display ref-root))
    (format t "  Refs root exists: ~A~%" (if (and ref-root (uiop:directory-exists-p ref-root))
                                              "yes"
                                              "no"))
    (format t "  Tool-result dedup: ~A~%"
            (if (claw-lisp.config:runtime-config-tool-result-dedup-p config)
                "enabled"
                "disabled"))))

(defun %print-cas-object-status (runtime versioned-hash)
  (let ((cas-root (claw-lisp.core.artifacts:runtime-effective-cas-root runtime)))
    (cond
      ((or (null versioned-hash) (zerop (length versioned-hash)))
       (format t "Usage: :cas object <versioned-hash>~%"))
      ((not (claw-lisp.storage.cas:valid-versioned-hash-p versioned-hash))
       (format t "Invalid CAS hash: ~A~%" versioned-hash))
      ((null cas-root)
       (format t "No CAS object root configured.~%"))
      (t
       (handler-case
           (let ((path (claw-lisp.storage.cas:cas-object-path cas-root versioned-hash))
                 (exists (claw-lisp.storage.cas:cas-exists-p cas-root versioned-hash)))
             (format t "CAS object:~%")
             (format t "  Hash: ~A~%" versioned-hash)
             (format t "  Path: ~A~%" (namestring path))
             (format t "  Exists: ~A~%" (if exists "yes" "no"))
             (when exists
               (let ((payload (or (claw-lisp.storage.cas:cas-get cas-root versioned-hash) "")))
                 (format t "  Size bytes: ~D~%" (length payload))
                 (format t "  Preview: ~A~%" (%truncate-line payload 240)))))
         (claw-lisp.storage.cas:cas-invalid-hash-error (e)
           (format t "CAS object lookup failed: ~A~%" e))
         (error (e)
           (format t "CAS object lookup failed: ~A~%" e)))))))

(defun %print-cas-ref-status (runtime ref-name)
  (let ((cas-root (claw-lisp.core.artifacts:runtime-effective-cas-root runtime))
        (ref-root (claw-lisp.core.artifacts:runtime-effective-cas-ref-root runtime)))
    (cond
      ((or (null ref-name) (zerop (length ref-name)))
       (format t "Usage: :cas ref <ref-name>~%"))
      ((null ref-root)
       (format t "No CAS ref root configured.~%"))
      (t
       (handler-case
           (let ((record (claw-lisp.storage.cas-ref:read-cas-ref ref-root ref-name)))
             (format t "CAS ref:~%")
             (format t "  Ref: ~A~%" ref-name)
             (format t "  Path: ~A~%"
                     (namestring (claw-lisp.storage.cas-ref:cas-ref-path ref-root ref-name)))
             (if record
                 (progn
                   (format t "  Exists: yes~%")
                   (format t "  Hash: ~A~%" (getf record :cas-hash))
                   (format t "  Version: ~D~%" (or (getf record :version) 0))
                   (format t "  Metadata: ~S~%" (getf record :metadata))
                   (if cas-root
                       (handler-case
                           (progn
                             (claw-lisp.storage.cas-ref:resolve-cas-ref
                              ref-root cas-root ref-name :require-object-p t)
                             (format t "  Resolved: yes~%"))
                         (claw-lisp.storage.cas-ref:cas-ref-dangling-error (e)
                           (format t "  Resolved: no (dangling: ~A)~%" e)))
                       (format t "  Resolved: unavailable (no CAS object root)~%")))
                 (format t "  Exists: no~%")))
         (claw-lisp.storage.cas-ref:cas-ref-invalid-name-error (e)
           (format t "CAS ref lookup failed: ~A~%" e))
         (error (e)
           (format t "CAS ref lookup failed: ~A~%" e)))))))

(defun %print-cas-manifest-status (runtime session manifest-hash)
  (let ((cas-root (claw-lisp.core.artifacts:runtime-effective-cas-root runtime)))
    (cond
      ((or (null manifest-hash) (zerop (length manifest-hash)))
       (format t "Usage: :cas manifest <versioned-hash>~%"))
      ((not (claw-lisp.storage.cas:valid-versioned-hash-p manifest-hash))
       (format t "Invalid CAS hash: ~A~%" manifest-hash))
      ((null cas-root)
       (format t "No CAS object root configured.~%"))
      (t
       (handler-case
           (let ((manifest (claw-lisp.cas.manifest:load-manifest
                            cas-root manifest-hash
                            :verify-integrity-p nil
                            :verify-signature-p t
                            :preserve-stored-root-digest-p t)))
             (if (null manifest)
                 (format t "CAS manifest not found: ~A~%" manifest-hash)
                 (let ((integrity-ok-p
                         (claw-lisp.cas.manifest:verify-manifest-integrity manifest)))
                   (unless integrity-ok-p
                     (claw-lisp.core.runtime:maybe-append-cas-verify-failed-event
                      session
                      (claw-lisp.core.runtime:session-transcript-path runtime session)
                      :manifest
                      manifest-hash
                      :integrity-mismatch
                      "Manifest root digest did not match manifest contents."))
                   (format t "CAS manifest:~%")
                   (format t "  Hash: ~A~%" manifest-hash)
                   (format t "  Root digest: ~A~%"
                           (claw-lisp.cas.manifest:manifest-root-digest manifest))
                   (format t "  Entries: ~D~%"
                           (length (claw-lisp.cas.manifest:manifest-entries manifest)))
                   (format t "  Metadata: ~S~%"
                           (claw-lisp.cas.manifest:manifest-metadata manifest))
                   (format t "  Signature: ~A~%"
                           (if (claw-lisp.cas.manifest:manifest-signature manifest)
                               "present"
                               "none"))
                   (format t "  Integrity: ~A~%"
                           (if integrity-ok-p
                               "verified"
                               "mismatch")))))
         (claw-lisp.cas.manifest:cas-manifest-parse-error (e)
           (claw-lisp.core.runtime:maybe-append-cas-verify-failed-event
            session
            (claw-lisp.core.runtime:session-transcript-path runtime session)
            :manifest manifest-hash :parse-error e)
           (format t "CAS manifest lookup failed: ~A~%" e))
         (claw-lisp.cas.manifest:cas-manifest-signature-error (e)
           (claw-lisp.core.runtime:maybe-append-cas-verify-failed-event
            session
            (claw-lisp.core.runtime:session-transcript-path runtime session)
            :manifest manifest-hash :signature-error e)
           (format t "CAS manifest lookup failed: ~A~%" e))
         (error (e)
           (claw-lisp.core.runtime:maybe-append-cas-verify-failed-event
            session
            (claw-lisp.core.runtime:session-transcript-path runtime session)
            :manifest manifest-hash :lookup-error e)
           (format t "CAS manifest lookup failed: ~A~%" e)))))))

(defun %parse-positive-integer (value)
  "Return VALUE parsed as a positive integer, or NIL when invalid."
  (handler-case
      (let ((parsed (parse-integer value :junk-allowed nil)))
        (when (> parsed 0) parsed))
    (error () nil)))

(defun %read-transcript-summary (transcript-path &key (tail-count 0))
  "Return two values: summary plist and tail-lines list."
  (let* ((summary (claw-lisp.core.runtime::inspect-transcript-file
                   transcript-path :tail-count tail-count))
         (tail-lines (or (getf summary :tail-lines) '())))
    (values summary tail-lines)))

(defun %print-child-snapshot-detail (runtime session snapshot)
  "Print detailed progress information for one child SNAPSHOT."
  (let* ((child-id (claw-lisp.core.domain:child-progress-snapshot-child-id snapshot))
         (handle (%find-child-handle runtime session child-id))
         (child-session (and handle (claw-lisp.core.domain:child-agent-handle-session handle)))
         (transcript-path (and child-session
                               (claw-lisp.core.runtime:session-transcript-path
                                runtime child-session))))
    (format t "Child: ~A~%" child-id)
    (format t "Status: ~A~%" (claw-lisp.core.domain:child-progress-snapshot-status snapshot))
    (format t "Messages: ~D~%" (claw-lisp.core.domain:child-progress-snapshot-messages-count snapshot))
    (format t "Tool calls: ~D~%" (claw-lisp.core.domain:child-progress-snapshot-tool-calls-count snapshot))
    (format t "Last updated: ~D~%"
            (claw-lisp.core.domain:child-progress-snapshot-last-updated-universal-time snapshot))
    (when transcript-path
      (format t "Transcript: ~A~%" (namestring transcript-path)))
    (let ((summary (claw-lisp.core.domain:child-progress-snapshot-summary-text snapshot)))
      (when (and summary (> (length summary) 0))
        (format t "Summary: ~A~%" summary)))))

(defun print-welcome ()
  "Print the CLI welcome message."
  (format t +cli-welcome-message+ +cli-version+))

(defun print-help ()
  "Print available CLI commands."
  (format t "~%Available commands:~%")
  (dolist (cmd +cli-commands+)
    (format t "  ~A~10T~A~%" (car cmd) (cdr cmd)))
  (format t "~%"))

(defun handle-command (runtime session command)
  "Handle a CLI command.

   Returns (values handled-p maybe-new-session)."
  (cond
    ((member command '(":quit" ":exit") :test #'string=)
     (format t "Goodbye!~%")
     (throw 'cli-exit t))
    ((string= command ":help")
     (print-help)
     (values t session))
    ((string= command ":status")
     (let* ((provider (claw-lisp.core.domain:agent-session-provider session))
            (model (claw-lisp.core.domain:agent-session-model session))
            (profile (claw-lisp.core.runtime::session-state-value session :profile nil))
            (conversation (claw-lisp.core.domain:agent-session-conversation session))
            (message-count (length (claw-lisp.core.domain:conversation-messages conversation)))
            (tool-result-count (length (claw-lisp.core.domain:conversation-tool-results conversation))))
       (format t "Provider: ~A~%" provider)
       (format t "Model: ~A~%" model)
       (when profile
         (format t "Profile: ~A~%" profile))
       (format t "Messages: ~A~%" message-count)
       (format t "Tool results: ~A~%" tool-result-count))
     (values t session))
    ((string= command ":tools")
     (let ((tools (claw-lisp.core.runtime:list-tool-names runtime)))
       (format t "Available tools:~%")
       (dolist (tool tools)
         (format t "  - ~A~%" tool)))
     (values t session))
    ((string= command ":providers")
     (let ((providers (claw-lisp.core.runtime:list-provider-names runtime)))
       (format t "Available providers:~%")
       (dolist (provider providers)
         (format t "  - ~A~%" provider)))
     (values t session))
    ((string= command ":provider")
     (format t "Current provider: ~A~%"
             (claw-lisp.core.domain:agent-session-provider session))
     (values t session))
    ((multiple-value-bind (matched provider) (%command-dispatch command ":provider")
       (when matched
         (if (null provider)
             (progn
               (format t "Current provider: ~A~%"
                       (claw-lisp.core.domain:agent-session-provider session))
               (values t session))
             (progn
               (if (zerop (length provider))
                   (format t "Usage: :provider <provider-name>~%")
                   (handler-case
                     (let ((result (claw-lisp.core.runtime:select-session-model
                                     runtime session :provider-name provider)))
                         (format t "Provider set to: ~A~%" (getf result :provider))
                         (dolist (warning (or (getf result :warnings) '()))
                           (format t "Warning: ~A~%" warning)))
                     (error (e)
                       (%format-selection-error "Provider switch" e))))
               (values t session))))))
    ((string= command ":models")
     (let* ((provider (claw-lisp.core.domain:agent-session-provider session))
            (models (claw-lisp.core.runtime:list-model-names
                     runtime
                     :provider-name provider)))
       (format t "Known models (~A):~%" provider)
       (if models
           (dolist (model-id models)
             (format t "  - ~A~%" model-id))
           (format t "  (none registered)~%")))
     (values t session))
    ((multiple-value-bind (matched provider) (%command-dispatch command ":models")
       (when (and matched provider)
         (let ((provider-error (claw-lisp.core.runtime:check-provider-configuration runtime provider)))
           (if (zerop (length provider))
               (format t "Usage: :models <provider-name>~%")
               (if provider-error
                   (format t "~A~%" provider-error)
                   (let ((models (claw-lisp.core.runtime:list-model-names runtime :provider-name provider)))
                     (format t "Known models (~A):~%" provider)
                     (if models
                         (dolist (model-id models)
                           (format t "  - ~A~%" model-id))
                         (format t "  (none registered)~%")))))
           (values t session)))))
    ((string= command ":memory")
     (let ((path (or (claw-lisp.storage.session-memory:session-memory-existing-path
                      (claw-lisp.core.runtime:runtime-settings runtime)
                      (claw-lisp.core.domain:agent-session-id session))
                     (claw-lisp.core.runtime:session-memory-path-for-session runtime session))))
       (format t "Session memory status:~%")
       (format t "  Path: ~A~%" path)
       (if (probe-file path)
           (let* ((size (%file-size-bytes path))
                  (written-at (file-write-date path)))
             (format t "  Exists: yes~%")
             (if size
                 (format t "  Size bytes: ~D~%" size)
                 (format t "  Size bytes: unknown~%"))
             (format t "  Last write (unix-seconds): ~D~%"
                     (%universal-time-to-unix-seconds written-at)))
           (format t "  Exists: no~%")))
     (values t session))
    ((string= command ":memory-content")
     (let ((path (or (claw-lisp.storage.session-memory:session-memory-existing-path
                      (claw-lisp.core.runtime:runtime-settings runtime)
                      (claw-lisp.core.domain:agent-session-id session))
                     (claw-lisp.core.runtime:session-memory-path-for-session runtime session))))
       (if (probe-file path)
           (progn
             (format t "Session memory at: ~A~%" path)
             (format t "~A~%" (uiop:read-file-string path)))
           (format t "No session memory file yet.~%")))
     (values t session))
    ((string= command ":clear")
     (let ((conversation (claw-lisp.core.domain:agent-session-conversation session)))
       (setf (claw-lisp.core.domain:conversation-messages conversation) nil
             (claw-lisp.core.domain:conversation-tool-results conversation) nil))
     (format t "Conversation cleared.~%")
     (values t session))
    ((string= command ":model")
     (format t "Current model: ~A~%" (claw-lisp.core.domain:agent-session-model session))
     (values t session))
    ((multiple-value-bind (matched model-id) (%command-dispatch command ":model")
       (when (and matched model-id)
         (if (zerop (length model-id))
             (format t "Usage: :model <model-id>~%")
             (handler-case
                 (let ((result (claw-lisp.core.runtime:select-session-model
                                runtime session :model model-id)))
                   (format t "Model set to: ~A~%" (getf result :model))
                   (dolist (warning (or (getf result :warnings) '()))
                     (format t "Warning: ~A~%" warning)))
               (error (e)
                 (%format-selection-error "Model switch" e))))
         (values t session))))
    ((multiple-value-bind (matched raw) (%command-dispatch command ":use")
       (when (and matched raw)
         (let* ((raw (string-trim '(#\Space #\Tab) raw))
                (parts (remove-if (lambda (part) (zerop (length part)))
                                  (uiop:split-string raw :separator '(#\Space #\Tab))))
                (provider (first parts))
                (model-id (second parts))
                (profile (third parts)))
           (if (or (< (length parts) 2)
                   (> (length parts) 3)
                   (zerop (length provider))
                   (zerop (length model-id)))
               (format t "Usage: :use <provider-name> <model-id> [profile]~%")
               (handler-case
                   (let ((result (claw-lisp.core.runtime:select-session-model
                                  runtime session
                                  :provider-name provider
                                  :model model-id
                                  :profile profile)))
                     (format t "Using provider=~A model=~A~%"
                             (getf result :provider)
                             (getf result :model))
                     (when (getf result :profile)
                       (format t "Profile: ~A~%" (getf result :profile)))
                     (dolist (warning (or (getf result :warnings) '()))
                       (format t "Warning: ~A~%" warning)))
                 (error (e)
                   (%format-selection-error "Selection" e))))
           (values t session)))))
    ((string= command ":config")
     (let ((config (claw-lisp.core.runtime:runtime-settings runtime)))
       (format t "Data root: ~A~%" (claw-lisp.config:runtime-config-data-root config))
       (format t "Default provider: ~A~%" (claw-lisp.config:runtime-config-default-provider config))
       (format t "Default model: ~A~%" (claw-lisp.config:runtime-config-default-model config))
       (format t "Tool preview bytes: ~A~%" (claw-lisp.config:runtime-config-tool-preview-bytes config))
       (format t "Shell command enabled: ~A~%" (claw-lisp.config:runtime-config-shell-command-enabled-p config)))
     (values t session))
    ((string= command ":diagnostics")
     (handler-case
         (let* ((dependency-check-error nil)
                (missing-deps (handler-case
                                  (claw-lisp.core.runtime:check-runtime-dependencies)
                                (error (condition)
                                  (warn "Diagnostics dependency check failed: ~A" condition)
                                  (setf dependency-check-error condition)
                                  :error)))
                (providers (claw-lisp.core.runtime:list-provider-names runtime))
                (tools (claw-lisp.core.runtime:list-tool-names runtime))
                (transcript-path (or (claw-lisp.core.runtime:session-transcript-existing-path runtime session)
                                     (claw-lisp.core.runtime:session-transcript-path runtime session))))
           (format t "Runtime diagnostics:~%")
           (format t "  Session id: ~A~%" (claw-lisp.core.domain:agent-session-id session))
           (format t "  Provider: ~A~%" (claw-lisp.core.domain:agent-session-provider session))
           (format t "  Model: ~A~%" (claw-lisp.core.domain:agent-session-model session))
           (format t "  Registered providers: ~D~%" (length providers))
           (format t "  Registered tools: ~D~%" (length tools))
           (format t "  Transcript path: ~A~%"
                   (if transcript-path (namestring transcript-path) "(none)"))
           (cond
             ((eq missing-deps :error)
              (format t "  Missing dependencies: (diagnostic check failed: ~A)~%"
                      (or dependency-check-error "unknown error")))
             (missing-deps
              (format t "  Missing dependencies: ~{~A~^, ~}~%" missing-deps))
             (t
              (format t "  Missing dependencies: none~%"))))
       (error (condition)
         (warn "Runtime diagnostics command failed: ~A" condition)
         (format t "Runtime diagnostics failed: ~A~%" condition)))
     (values t session))
    ((string= command ":compaction")
     (let* ((status (claw-lisp.core.runtime:session-context-status runtime session))
            (state (claw-lisp.core.domain:agent-session-state session))
            (failures (getf state :compaction-failure-count 0))
            (circuit-open (claw-lisp.core.runtime:compaction-circuit-open-p session))
            (transcript-path (or (claw-lisp.core.runtime:session-transcript-existing-path runtime session)
                                 (claw-lisp.core.runtime:session-transcript-path runtime session))))
       (multiple-value-bind (last-event-line transcript-error missing-event-count)
           (%find-last-compaction-event-line transcript-path)
       (format t "Compaction status:~%")
       (format t "  Tokens: ~D / ~D~%"
               (getf status :tokens 0)
               (getf status :effective-window 0))
       (format t "  Warning threshold: ~D~%" (getf status :warning-threshold 0))
       (format t "  Compaction threshold: ~D~%" (getf status :compaction-threshold 0))
       (format t "  Warning: ~A~%" (if (getf status :warning-p nil) "yes" "no"))
       (format t "  Compaction needed: ~A~%"
               (if (getf status :compaction-needed-p nil) "yes" "no"))
       (format t "  Failure count: ~D~%" failures)
       (format t "  Circuit open: ~A~%" (if circuit-open "yes" "no"))
       (if last-event-line
           (format t "  Last event: ~A~%" (%truncate-line last-event-line))
           (format t "  Last event: none~%"))
       (when (and missing-event-count (> missing-event-count 0))
         (format t "  Transcript schema warning: ~D parsed line(s) missing event field~%"
                 missing-event-count))
       (when transcript-error
         (format t "  Transcript read warning: ~A~%" transcript-error))))
     (values t session))
    ((multiple-value-bind (matched args) (%command-dispatch command ":transcript")
       (when matched
         (let* ((transcript-path (or (claw-lisp.core.runtime:session-transcript-existing-path runtime session)
                                     (claw-lisp.core.runtime:session-transcript-path runtime session)))
                (trimmed-args (and args (string-trim '(#\Space #\Tab) args)))
                (parts (and trimmed-args
                            (remove-if (lambda (part) (zerop (length part)))
                                       (uiop:split-string trimmed-args :separator '(#\Space #\Tab)))))
                (tail-count
                  (cond
                    ((null parts) 0)
                    ((and (= (length parts) 1)
                          (string-equal (first parts) "tail"))
                     +transcript-default-tail-count+)
                    ((and (= (length parts) 2)
                          (string-equal (first parts) "tail"))
                     (or (%parse-positive-integer (second parts))
                         :invalid))
                    (t :invalid))))
           (if (or (eq tail-count :invalid)
                   (and (integerp tail-count)
                        (> tail-count +transcript-max-tail-count+)))
               (format t "Usage: :transcript | :transcript tail [n] (n<=~D)~%"
                       +transcript-max-tail-count+)
               (multiple-value-bind (summary tail-lines)
                   (%read-transcript-summary transcript-path :tail-count tail-count)
                 (format t "Transcript summary:~%")
                 (format t "  Path: ~A~%" (or transcript-path "(none)"))
                 (format t "  Exists: ~A~%" (if (getf summary :exists) "yes" "no"))
                 (cond
                   ((null transcript-path)
                    (format t "  Last event name: none~%")
                    (format t "  Note: Transcript not configured for this session.~%"))
                   ((getf summary :exists)
                     (progn
                       (format t "  Size bytes: ~A~%"
                               (or (%file-size-bytes transcript-path) "unknown"))
                       (format t "  Lines: ~D~%" (getf summary :line-count 0))
                       (format t "  Parsed JSON lines: ~D~%" (getf summary :parsed-json-count 0))
                       (format t "  Non-JSON or malformed JSON lines: ~D~%"
                               (getf summary :malformed-json-count 0))
                       (format t "  Event lines: ~D~%" (getf summary :event-count 0))
                       (format t "  Message events: ~D~%" (getf summary :message-count 0))
                       (format t "  Last event name: ~A~%"
                               (or (getf summary :last-event) "none"))))
                   (t
                    (format t "  Last event name: none~%")
                    (format t "  Note: Transcript file missing.~%")))
                 (when (getf summary :error)
                   (format t "  Transcript read warning: ~A~%" (getf summary :error)))
                (when (> tail-count 0)
                   (format t "Transcript tail (~D):~%" tail-count)
                   (if tail-lines
                       (dolist (line tail-lines)
                         (format t "  ~A~%"
                                 (%truncate-line line +transcript-tail-line-preview-max-len+)))
                       (format t "  (no lines)~%")))))
           (values t session)))))
    ((multiple-value-bind (matched args) (%command-dispatch command ":cas")
       (when matched
         (let* ((parts (%split-cli-args args))
                (subcommand (and parts (first parts))))
           (cond
             ((or (null parts) (string-equal subcommand "status"))
              (%print-cas-status runtime))
             ((string-equal subcommand "object")
              (%print-cas-object-status runtime (second parts)))
             ((string-equal subcommand "ref")
             (%print-cas-ref-status runtime (second parts)))
             ((string-equal subcommand "manifest")
              (%print-cas-manifest-status runtime session (second parts)))
             (t
              (format t "Usage: :cas [status|object <hash>|ref <ref-name>|manifest <hash>]~%"))))
         (values t session))))
    ((string= command ":resume")
     (format t "Usage: :resume <session-id>~%")
     (values t session))
    ((and (>= (length command) 8)
          (string= ":resume " (subseq command 0 8)))
     (let ((session-id (string-trim '(#\Space #\Tab) (subseq command 8))))
       (cond
         ((zerop (length session-id))
          (format t "Usage: :resume <session-id>~%")
          (values t session))
         (t
          (handler-case
              (let ((resumed (claw-lisp.core.runtime:resume-session runtime session-id)))
                (format t "Resumed session ~A (~D restored messages).~%"
                        session-id
                        (getf (claw-lisp.core.domain:agent-session-state resumed)
                              :resumed-message-count
                              0))
                (values t resumed))
            (error (e)
              (format t "Resume failed: ~A~%" e)
              (values t session)))))))
    ((string= command ":tasks")
     (let ((snapshots (claw-lisp.core.runtime:list-child-progress-snapshots runtime session)))
       (if snapshots
           (progn
             (format t "Background tasks:~%")
             (dolist (snapshot snapshots)
               (format t "  - [~A] ~A  status=~A  messages=~D  tools=~D~%"
                       (%background-status-bucket
                        (claw-lisp.core.domain:child-progress-snapshot-status snapshot))
                       (claw-lisp.core.domain:child-progress-snapshot-child-id snapshot)
                       (claw-lisp.core.domain:child-progress-snapshot-status snapshot)
                       (claw-lisp.core.domain:child-progress-snapshot-messages-count snapshot)
                       (claw-lisp.core.domain:child-progress-snapshot-tool-calls-count snapshot))))
           (format t "No background tasks right now.~%")))
     (values t session))
    ((string= command ":agents")
     (format t "Child-agent orchestration commands are not included in the public Achatina build.~%")
     (values t session))
    ((string= command ":agent")
     (format t "Usage: :agent <child-id>~%")
     (values t session))
    ((and (>= (length command) 7)
          (string= ":agent " (subseq command 0 7)))
     (let* ((child-id (string-trim '(#\Space #\Tab) (subseq command 7)))
            (snapshot (and (> (length child-id) 0)
                           (claw-lisp.core.runtime:child-progress-snapshot runtime session child-id))))
       (cond
         ((zerop (length child-id))
          (format t "Usage: :agent <child-id>~%"))
         (snapshot
          (%print-child-snapshot-detail runtime session snapshot))
         (t
          (format t "No child agent found: ~A~%" child-id))))
     (values t session))
    (t (values nil nil))))

(defun execute-user-message (runtime session user-text)
  "Execute a user message and print the response.

   USER-TEXT is the raw user input."
  (let* ((conversation (claw-lisp.core.domain:agent-session-conversation session))
         (transcript-path (claw-lisp.core.runtime:session-transcript-path runtime session))
         (provider (claw-lisp.core.runtime:resolve-provider
                    runtime
                    (claw-lisp.core.domain:agent-session-provider session))))
    ;; Append user message
    (claw-lisp.core.domain:append-message
     conversation
     (claw-lisp.core.domain:make-message :role :user :content user-text))
    ;; Record in transcript
    (claw-lisp.core.runtime:maybe-append-transcript-event
     session
     transcript-path
     (list :event "message"
           :session_id (claw-lisp.core.domain:agent-session-id session)
           :role "user"
           :content user-text))
    ;; Execute provider turn loop
    (format t "~%[Thinking...~%")
    (finish-output)
    (handler-case
        (let ((response (claw-lisp.core.runtime:execute-provider-turn-loop
                         runtime session provider)))
          (let ((assistant-text (claw-lisp.core.domain:transport-response-assistant-text response))
                (tool-calls (claw-lisp.core.domain:transport-response-tool-calls response)))
            (format t "~%~A~%" assistant-text)
            (when tool-calls
              (format t "~%[Executed ~D tool(s)]~%" (length tool-calls)))))
      (error (e)
        (format t "~%[Error: ~A]~%" e)))))

(defun run-repl-loop (runtime session)
  "Run the main REPL loop.

   Returns when the user exits."
  (catch 'cli-exit
    (loop
      (format t +cli-prompt+)
      (finish-output)
      (let ((input (read-line *standard-input* nil nil)))
        (unless input
          ;; EOF (Ctrl+D)
          (format t "~%Goodbye!~%")
          (throw 'cli-exit t))
        (let ((trimmed (string-trim '(#\Space #\Tab #\Newline) input)))
          (cond
            ((zerop (length trimmed))
             ;; Empty input - ignore
             nil)
            ((char= (char trimmed 0) #\:)
             ;; Command
             (multiple-value-bind (handled-p maybe-new-session)
                 (handle-command runtime session trimmed)
               (when maybe-new-session
                 (setf session maybe-new-session))
               (unless handled-p
                 (format t "Unknown command: ~A~%Type :help for available commands.~%" trimmed))))
            (t
             ;; User message
             (execute-user-message runtime session trimmed))))))))

(defun make-cli-runtime (&key (provider "anthropic") (model "claude-sonnet-4-6") (project-root nil))
  "Create a runtime configured for CLI use.

   Returns the runtime and initial session as two values."
  (let* ((config (claw-lisp.config:make-default-runtime-config))
         (runtime (claw-lisp.core.runtime:make-runtime
                   :config config
                   :project-root project-root))
         (session nil))
    (claw-lisp.core.runtime:register-default-providers runtime)
    (claw-lisp.core.runtime:register-default-tools runtime)
    ;; Create session
    (setf session (claw-lisp.core.runtime:start-session
                   runtime
                   :provider-name provider
                   :model model
                   :session-id (format nil "cli-~A" (get-universal-time))))
    (values runtime session)))

(defun run-cli (&key (provider "anthropic") (model "claude-sonnet-4-6") (project-root nil))
  "Run the Claw Lisp CLI.

   PROVIDER is the provider name (default: \"anthropic\").
   MODEL is the model name (default: \"claude-sonnet-4-6\").
   PROJECT-ROOT is the project directory for CLAUDE.md discovery."
  (multiple-value-bind (runtime session)
      (make-cli-runtime :provider provider :model model :project-root project-root)
    (print-welcome)
    (format t "Provider: ~A, Model: ~A~%" provider model)
    (format t "Type :help for commands.~%~%")
    (run-repl-loop runtime session)))

(defun main (&optional args)
  "Main entry point for the CLI.

   ARGS is the command-line argument list."
  (%dispatch-cli (or args (uiop:command-line-arguments))))

(defun main-entry-point ()
  "Process entry point for the CLI executable."
  (uiop:quit (main)))

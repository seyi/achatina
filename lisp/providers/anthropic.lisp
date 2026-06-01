(in-package #:claw-lisp.providers.anthropic)

;;; ============================================================
;;; Class Definition
;;; ============================================================

(defclass anthropic-provider (provider)
  ((credentials
    :initarg :credentials
    :accessor anthropic-provider-credentials
    :documentation "Anthropic API credentials.")
   (rate-limit-state
    :initarg :rate-limit-state
    :initform nil
    :accessor anthropic-provider-rate-limit-state
    :documentation "Rate-limit state for tracking x-ratelimit-* headers."))
  (:documentation "Anthropic provider with rate-limit tracking."))

(defun make-anthropic-provider (config)
  "Create the Anthropic provider object from runtime config."
  (declare (type runtime-config config))
  (let ((creds (claw-lisp.config:config-credentials config :anthropic)))
    (make-instance 'anthropic-provider
                   :name "anthropic"
                   :api-key (and creds
                                 (claw-lisp.config:provider-credentials-api-key creds))
                   :credentials creds
                   :rate-limit-state (claw-lisp.providers.rate-limit:make-rate-limit-state
                                      :provider :anthropic))))

;;; ============================================================
;;; Helpers
;;; ============================================================

(defun normalize-conversation-for-anthropic (conversation model-capabilities)
  "Normalize CONVERSATION messages for Anthropic API submission.
   MODEL-CAPABILITIES is the resolved model capability struct."
  (let* ((messages (claw-lisp.core.domain:conversation-messages conversation))
         (tool-results (claw-lisp.core.domain:conversation-tool-results conversation))
         (normalized (claw-lisp.core.message-normalization:normalize-messages-for-api
                      messages model-capabilities tool-results)))
    (claw-lisp.core.domain:make-conversation
     :id (claw-lisp.core.domain:conversation-id conversation)
     :messages normalized
     :tool-results tool-results
     :metadata (claw-lisp.core.domain:conversation-metadata conversation))))

(defun log-rate-limit-update (rl-state headers context)
  "Update rate-limit state from HEADERS, logging failures instead of silencing them."
  (when (and rl-state headers)
    (handler-case
        (claw-lisp.providers.rate-limit:update-rate-limit-state rl-state headers)
      (error (e)
        (warn "Failed to update Anthropic rate-limit state (~A): ~A" context e)))))

(defun provider-configured-p (provider)
  "Return T if PROVIDER has valid credentials."
  (let ((creds (anthropic-provider-credentials provider)))
    (claw-lisp.providers.auth:credentials-configured-p creds)))

(defun make-unconfigured-response ()
  "Return a transport-response indicating the provider is not configured."
  (claw-lisp.core.domain:make-transport-response
   :ok-p nil
   :status 0
   :assistant-text "Anthropic provider registered, but credentials are not configured."
   :raw-response ""
   :provider "anthropic"))

;;; ============================================================
;;; send-turn (synchronous)
;;; ============================================================

(defmethod send-turn ((provider anthropic-provider) conversation &key model tools system)
  "Send a turn to the Anthropic API and return the response.
   Updates rate-limit state from response headers."
  (if (not (provider-configured-p provider))
      (make-unconfigured-response)
      (let* ((creds (anthropic-provider-credentials provider))
             (api-key (claw-lisp.config:provider-credentials-api-key creds))
             (base-url (or (claw-lisp.config:provider-credentials-base-url creds)
                           "https://api.anthropic.com/v1/messages"))
             (api-version (claw-lisp.config:anthropic-credentials-api-version creds))
             (model-capabilities (claw-lisp.core.model-registry:resolve-model
                                  (provider-model-registry provider)
                                  model))
             (normalized-conversation (normalize-conversation-for-anthropic
                                       conversation model-capabilities))
             (body-plist (conversation->anthropic-json
                          normalized-conversation model :tools tools))
             (body-with-system (if system
                                   (append body-plist (list :system system))
                                   body-plist))
             (conv-id (claw-lisp.core.domain:conversation-id conversation))
             (rl-state (anthropic-provider-rate-limit-state provider)))
        ;; Capture raw request for validation harness
        (claw-lisp.core.message-normalization:capture-payload
         body-with-system :request
         :conversation-id conv-id :turn-id "sync")
        (multiple-value-bind (status body headers)
            (post-json-with-headers base-url
                                    (list (cons "x-api-key" api-key)
                                          (cons "anthropic-version" api-version))
                                    body-with-system
                                    :rate-limit-state rl-state)
          ;; Update rate-limit state from response headers
          (log-rate-limit-update rl-state headers "send-turn")
          ;; Capture response
          (claw-lisp.core.message-normalization:capture-payload
           body :response
           :conversation-id conv-id :turn-id "sync")
          (let ((success-p (http-post-result-success-p status))
                (response-text (extract-anthropic-response-text body)))
            (claw-lisp.core.domain:make-transport-response
             :ok-p success-p
             :status status
             :assistant-text response-text
             :raw-response body
             :error-message (unless success-p response-text)
             :provider "anthropic"
             :tool-calls (extract-anthropic-tool-calls body)))))))

;;; ============================================================
;;; stream-turn (SSE streaming)
;;; ============================================================

(defmethod stream-turn ((provider anthropic-provider) conversation
                        &key model tools on-event system)
  "Stream a turn from the Anthropic API.

   ON-EVENT is called for each streaming event (for UI updates):
     - :message_start: (list :id message-id :model model-name)
     - :text_delta: text-string
     - :content_block_start: (list :type \"tool_use\" :id tool-id :name tool-name)
     - :input_json_delta: partial-json-string
     - :tool_use_complete: (list :id tool-id :name tool-name :input parsed-input)
     - :message_delta: (list :stop_reason reason :stop_sequence seq)
     - :usage: usage-plist
     - :message_stop: nil
   SYSTEM is the system prompt to send with the request.
   Returns the final transport-response when the stream completes.
   Updates rate-limit state from response headers."
  (if (not (provider-configured-p provider))
      (make-unconfigured-response)
      ;; Normalize conversation for API submission
      (let* ((creds (anthropic-provider-credentials provider))
             (api-key (claw-lisp.config:provider-credentials-api-key creds))
             (base-url (or (claw-lisp.config:provider-credentials-base-url creds)
                           "https://api.anthropic.com/v1/messages"))
             (api-version (claw-lisp.config:anthropic-credentials-api-version creds))
             (model-capabilities (claw-lisp.core.model-registry:resolve-model
                                  (provider-model-registry provider)
                                  model))
             (normalized-conversation (normalize-conversation-for-anthropic
                                       conversation model-capabilities))
             ;; Build the request body with streaming enabled
             (body-plist (conversation->anthropic-json
                          normalized-conversation model :tools tools))
             ;; Add "stream": true and system prompt to the body
             (streaming-body (list* :stream t body-plist))
             (final-body (if system
                             (list* :system system streaming-body)
                             streaming-body))
             (conv-id (claw-lisp.core.domain:conversation-id conversation))
             (acc (claw-lisp.providers.stream-accumulator:make-stream-accumulator
                   :on-event on-event))
             (url base-url)
             (request-headers (list (cons "x-api-key" api-key)
                                    (cons "anthropic-version" api-version)
                                    (cons "accept" "text/event-stream")))
             (rl-state (anthropic-provider-rate-limit-state provider))
             ;; Track whether we got a meaningful error to report
             (stream-error-message nil))
        ;; Capture the exact request payload sent to Anthropic
        (claw-lisp.core.message-normalization:capture-payload
         final-body :request
         :conversation-id conv-id :turn-id "stream")
        (handler-case
            ;; Make the initial request with retry support
            (let ((result
                    (claw-lisp.providers.retry:call-with-retry
                     (lambda ()
                       (handler-case
                           (multiple-value-bind (body status-code hdrs)
                               (dexador:post url
                                            :content (claw-lisp.providers.http-utils:json-encode-string
                                                      final-body)
                                            :headers request-headers
                                            :content-type "application/json"
                                            :want-stream t
                                            :throw t
                                            :read-timeout 120
                                            :connect-timeout 30)
                             ;; Success - Dexador only returns on 2xx with :throw t
                             (list :ok t :stream body :headers hdrs :status status-code))
                         (dexador:http-request-failed (e)
                           ;; Extract status and headers from the condition
                           (let ((status (claw-lisp.providers.http-utils:dexador-response-status e))
                                 (hdrs (claw-lisp.providers.http-utils:dexador-response-headers e)))
                             (log-rate-limit-update rl-state hdrs "stream-turn/http-error")
                             (if (claw-lisp.providers.retry:retryable-status-p status)
                                 ;; Retryable - signal error to trigger retry
                                 (error e)
                                 ;; Non-retryable - return error result
                                 (list :ok nil :status status :headers hdrs))))))
                     :rate-limit-state rl-state
                     :max-retries 3
                     :base-delay 1
                     :max-delay 60)))
              ;; Process the result
              (let ((stream (getf result :stream))
                    (response-headers (getf result :headers))
                    (status (getf result :status)))
                (cond
                  ;; Success case - we have a stream
                  (stream
                   ;; Update rate-limit state from response headers
                   (log-rate-limit-update rl-state response-headers "stream-turn/success")
                   ;; Read the stream and process SSE events
                   ;; CRITICAL: ensure stream is closed on all exit paths
                   (unwind-protect
                        (handler-case
                            (loop for event = (claw-lisp.providers.sse-parser:read-sse-event stream)
                                  while event
                                  do (claw-lisp.providers.stream-accumulator:process-stream-event acc event))
                          (error (e)
                            (setf stream-error-message
                                  (format nil "Error processing SSE stream: ~A" e))
                            (warn "~A" stream-error-message)))
                     ;; Cleanup: always close the stream
                     (handler-case (close stream)
                       (error (e)
                         (warn "Failed to close Anthropic stream: ~A" e)))))
                  ;; Error case - no stream, status code available
                  (status
                   (setf stream-error-message
                         (format nil "Streaming request failed with status ~A" status))
                   (warn "~A" stream-error-message))
                  ;; Fallback - unexpected result structure
                  (t
                   (setf stream-error-message "Unexpected result from retry wrapper")
                   (warn "Anthropic stream-turn: unexpected result structure: ~S" result)))))
          ;; Outer error handler for unexpected errors (network failures, retry exhaustion, etc.)
          (dexador:http-request-failed (e)
            ;; Dexador-specific errors: extract headers for rate-limit tracking
            (let ((hdrs (claw-lisp.providers.http-utils:dexador-response-headers e)))
              (log-rate-limit-update rl-state hdrs "stream-turn/outer-dex-error"))
            (setf stream-error-message (format nil "HTTP request failed: ~A" e))
            (warn "Streaming error: ~A" e))
          (error (e)
            ;; Generic errors: no headers to extract
            (setf stream-error-message (format nil "Streaming error: ~A" e))
            (warn "Streaming error: ~A" e)))
        ;; Build the final response from the accumulator
        (let ((response (claw-lisp.providers.stream-accumulator:accumulator->transport-response
                         acc :provider "anthropic")))
          ;; If we had a stream-level error and the accumulator produced an
          ;; apparently-ok but empty response, override with the error info
          (if (and stream-error-message
                   (or (not (claw-lisp.core.domain:transport-response-ok-p response))
                       (null (claw-lisp.core.domain:transport-response-assistant-text response))
                       (string= "" (claw-lisp.core.domain:transport-response-assistant-text
                                    response))))
              (claw-lisp.core.domain:make-transport-response
               :ok-p nil
               :status (or (claw-lisp.core.domain:transport-response-status response) 0)
               :assistant-text ""
               :raw-response (or (claw-lisp.core.domain:transport-response-raw-response response) "")
               :error-message stream-error-message
               :provider "anthropic")
              response)))))

;;; ============================================================
;;; Provider Protocol Methods
;;; ============================================================

(defmethod normalize-response ((provider anthropic-provider) response)
  (declare (ignore provider))
  response)

(defmethod count-tokens ((provider anthropic-provider) messages &key model)
  "Count tokens in MESSAGES using a simple length-based heuristic.

   Note: This is an approximation. Anthropic uses BPE tokenization.
   For accurate counts, use Anthropic's /messages/count_tokens endpoint.

   MODEL is ignored in this implementation."
  (declare (ignore provider model))
  (reduce #'+
          messages
          :key (lambda (message)
                 (max 1
                      (ceiling
                       (length (claw-lisp.core.domain:message-content-text message))
                       4)))
          :initial-value 0))

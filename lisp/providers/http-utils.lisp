(in-package #:claw-lisp.providers.http-utils)

(defparameter *http-debug-p* nil)

(defun command-available-p (command)
  "Return true when COMMAND can be resolved on PATH."
  (string= "ok"
           (string-trim
            '(#\Space #\Tab #\Newline #\Return)
            (uiop:run-program (list "sh" "-lc"
                                    (format nil "command -v ~A >/dev/null 2>&1 && printf ok" command))
                              :output '(:string :stripped t)
                              :error-output '(:string :stripped t)
                              :ignore-error-status t))))

;; --- JSON Serialization (yason) ---
;; Uses value->json-safe and plist-to-json-object from http-json.lisp

(defun json-encode (value stream)
  "Encode VALUE to STREAM using yason.
   Plists are converted to hash tables so yason produces JSON objects."
  (yason:encode (value->json-safe value) stream))

(defun json-encode-string (value)
  "Encode VALUE to a JSON string.
   Plists become JSON objects; lists become JSON arrays."
  (with-output-to-string (stream)
    (json-encode value stream)))

(defun yason->plist (obj)
  "Convert yason-parsed object to keyword-keyed plists.

   Yason :alist mode produces nested alists of cons cells:
     ((\"key\" . value) (\"key2\" . value2))
   JSON arrays are parsed as vectors.

   This function recursively converts to keyword-keyed plists:
     (:key value :key2 value2)"
  (cond
    ((null obj) nil)
    ((stringp obj) obj)
    ((numberp obj) obj)
    ((eq obj :json-false) nil)
    ((eq obj t) t)
    ((vectorp obj)
     ;; JSON array -> Lisp list
     (loop for i from 0 below (length obj)
           collect (yason->plist (aref obj i))))
    ((and (consp obj) (consp (car obj)) (stringp (caar obj)))
     ;; Alist of cons cells ((\"key\" . val) ...) -> keyword plist
     (loop for cell in obj
           collect (intern (string-upcase (car cell)) "KEYWORD")
           collect (yason->plist (cdr cell))))
    ((listp obj)
     ;; Plain list — map recursively
     (mapcar #'yason->plist obj))
    (t obj)))

(defun json-decode (text)
  "Decode JSON TEXT into a Lisp object with keyword-keyed plists."
  (let ((yason:*parse-object-as* :alist))
    (yason->plist (yason:parse text))))

;; --- HTTP via dexador with retry ---

(defun dexador-error-response (condition)
  "Extract the response object from a dexador error condition.
   Uses slot-value since dexador doesn't export a public accessor."
  (handler-case
      (slot-value condition 'dexador::response)
    (error () nil)))

(defun dexador-response-status (condition)
  "Extract HTTP status from a dexador error condition."
  (let ((resp (dexador-error-response condition)))
    (if resp
        (handler-case (slot-value resp 'dexador::status) (error () 0))
        0)))

(defun dexador-response-body (condition)
  "Extract HTTP body from a dexador error condition."
  (let ((resp (dexador-error-response condition)))
    (if resp
        (handler-case (slot-value resp 'dexador::body) (error () ""))
        "")))

(defun dexador-response-headers (condition)
  "Extract HTTP headers from a dexador error condition."
  (let ((resp (dexador-error-response condition)))
    (if resp
        (handler-case (slot-value resp 'dexador::headers) (error () nil))
        nil)))

(defun extract-retry-after (condition)
  "Extract Retry-After value from a dexador error condition, or NIL."
  (let ((headers (dexador-response-headers condition)))
    (when headers
      (let ((retry-after (gethash "retry-after" headers)))
        (when retry-after
          (handler-case
              (parse-integer (string-trim '(#\Space) retry-after))
            (error () nil)))))))

(defun compute-retry-delay (attempt base-delay max-delay &optional retry-after)
  "Return delay seconds. Uses Retry-After if available, otherwise exponential backoff."
  (if (and retry-after (> retry-after 0))
      (min max-delay retry-after)
      (claw-lisp.providers.retry:exponential-delay attempt base-delay max-delay)))

(defun post-json (url headers body-plist)
  "POST BODY-PLIST as JSON to URL with HEADERS using dexador.

Retries on 429 and 5xx errors with exponential backoff.
Honors the Retry-After header when present.
Signals appropriate conditions from the claw-error hierarchy on failure.

Returns (values status-code body-text) where body-text is the raw response."
  (multiple-value-bind (status body headers)
      (post-json-with-headers url headers body-plist)
    (declare (ignore headers))
    (values status body)))

(defun post-json-with-headers (url headers body-plist &key rate-limit-state)
  "POST BODY-PLIST as JSON to URL with HEADERS using dexador.

Retries on 429 and 5xx errors with exponential backoff.
Honors the Retry-After header when present.
Signals appropriate conditions from the claw-error hierarchy on failure.

Returns (values status-code body-text headers-hash) where:
  - body-text is the raw response string
  - headers-hash is a hash-table with lowercase string keys
RATE-LIMIT-STATE (optional) is updated with parsed rate-limit headers."
  (let* ((header-plist
           (loop for h in headers
                 collect (let ((pos (position #\: h)))
                           (if pos
                               (cons (subseq h 0 pos)
                                     (string-trim '(#\Space) (subseq h (1+ pos))))
                               (cons h "")))))
         (json-body (json-encode-string body-plist))
         (response-headers nil))
    (when *http-debug-p*
      (format *error-output* "→ POST ~A~%" url)
      (format *error-output* "  headers: ~S~%" headers)
      (format *error-output* "  body: ~A~%" json-body))
    (multiple-value-bind (status-code body)
        (claw-lisp.providers.retry:call-with-retry
            (lambda ()
              (handler-case
                  (multiple-value-bind (body sc headers uri stream must-close reason)
                      (dexador:post url
                                    :content json-body
                                    :headers header-plist
                                    :content-type "application/json"
                                    :want-stream nil)
                    (declare (ignore uri stream must-close reason))
                    (setf response-headers headers)
                    (values sc body))
                (dexador:http-request-failed (e)
                  (let ((sc (dexador-response-status e))
                        (bd (dexador-response-body e))
                        (hdrs (dexador-response-headers e)))
                    (setf response-headers hdrs)
                    (if (claw-lisp.providers.retry:retryable-status-p sc)
                        (error "HTTP retryable [status ~A]: ~A" sc bd)
                        (values sc bd))))))
          :rate-limit-state rate-limit-state
          :on-retry (lambda (attempt status err)
                      (when *http-debug-p*
                        (format *error-output* "  retry ~D [status ~A]~@[ error: ~A~]~%"
                                attempt status err))))
      (if (and status-code (>= status-code 200) (< status-code 300))
          (values status-code body response-headers)
          (cond
            ((= status-code 429)
             (error 'claw-lisp.core.conditions:rate-limit-error
                    :provider "http" :status status-code
                    :retry-after (when response-headers
                                   (gethash "retry-after" response-headers))
                    :message (format nil "Rate limited [status ~A]" status-code)))
            ((or (= status-code 401) (= status-code 403))
             (error 'claw-lisp.core.conditions:auth-error
                    :provider "http" :status status-code
                    :response-body body
                    :message (format nil "Authentication failed [status ~A]" status-code)))
            ((= status-code 413)
             (error 'claw-lisp.core.conditions:context-exceeded-error
                    :provider "http" :status status-code
                    :response-body body
                    :message "Context window exceeded"))
            (t
             (error 'claw-lisp.core.conditions:provider-error
                    :provider "http" :status status-code
                    :response-body body
                    :message (format nil "HTTP error [status ~A]: ~A" status-code body))))))))

(defun http-post-result-success-p (status)
  (and (>= status 200) (< status 300)))

;; --- Content Block Serialization ---

(defun role->anthropic-role (role)
  "Map internal ROLE keywords to Anthropic API role strings."
  (case role
    (:system "system")
    (:assistant "assistant")
    (otherwise "user")))

(defun role->chat-role (role)
  "Map internal ROLE keywords to chat API role strings."
  (case role
    (:assistant "assistant")
    (:system "system")
    (:tool "tool")
    (otherwise "user")))

(defun content-block->anthropic-block (block)
  "Convert a single content-block struct to an Anthropic content block plist."
  (typecase block
    (text-block
     (list :type "text" :text (text-block-text block)))
    (tool-use-block
     (list :type "tool_use"
           :id (tool-use-block-id block)
           :name (tool-use-block-name block)
           :input (or (tool-use-block-input block) (list))))
    (tool-result-block
     (list :type "tool_result"
           :tool_use_id (tool-result-block-tool-use-id block)
           :content (tool-result-block-content block)
           :is_error (tool-result-block-is-error block)))
    (thinking-block
     (list :type "thinking"
           :thinking (thinking-block-thinking block)
           :signature (thinking-block-signature block)))
    (t
     (list :type "text" :text (format nil "~A" block)))))

(defun content-blocks->anthropic-array (blocks)
  "Convert a list of content-block structs to an Anthropic content array."
  (mapcar #'content-block->anthropic-block blocks))

(defun content-blocks-chat-text (blocks)
  "Extract concatenated text from text-blocks in BLOCKS."
  (with-output-to-string (out)
    (dolist (block blocks)
      (when (text-block-p block)
        (write-string (text-block-text block) out)))))

(defun content-blocks-chat-tool-calls (blocks)
  "Extract tool-use blocks as OpenAI-style tool_calls."
  (loop for block in blocks
        when (tool-use-block-p block)
        collect (list :id (tool-use-block-id block)
                      :type "function"
                      :function (list :name (tool-use-block-name block)
                                      :arguments (json-encode-string
                                                  (or (tool-use-block-input block) (list)))))))

;; --- Message Serialization ---

(defun message->anthropic-block (message)
  "Convert a MESSAGE to an Anthropic API message block (as a plist)."
  (let ((content (message-content message)))
    (if (stringp content)
        (list :role (role->anthropic-role (message-role message))
              :content content)
        (list :role (role->anthropic-role (message-role message))
              :content (content-blocks->anthropic-array content)))))

(defun message->chat-completion-block (message)
  "Convert a MESSAGE to an OpenAI-style chat completion block."
  (let ((content (message-content message)))
    (if (stringp content)
        (list :role (role->chat-role (message-role message))
              :content content)
        (let* ((text (content-blocks-chat-text content))
               (tool-calls (content-blocks-chat-tool-calls content)))
          (let ((block (list :role (role->chat-role (message-role message))
                             :content (or text ""))))
            (when tool-calls
              (push (cons :tool_calls tool-calls) block))
            block)))))

;; --- Conversation to JSON ---

(defun conversation->anthropic-json (conversation model &key tools system)
  "Render CONVERSATION into an Anthropic messages API request body.

   Returns a plist suitable for json-encode-string.
   TOOLS is a list of tool definition plists.
   SYSTEM is an optional string or list of content blocks."
  (declare (ignorable system))
  (let ((messages
          (loop for msg in (conversation-messages conversation)
                collect (message->anthropic-block msg))))
    (let ((body (list*
                 :model model
                 :max_tokens 1024
                 :messages messages)))
      (when tools
        (push (cons :tools tools) body))
      body)))

(defun conversation->chat-json (conversation model &key tools)
  "Render CONVERSATION into an OpenRouter/chat-completions JSON request body.

   Returns a plist suitable for json-encode-string."
  (let ((messages
          (loop for msg in (conversation-messages conversation)
                collect (message->chat-completion-block msg))))
    (let ((body (list* :model model :messages messages)))
      (when tools
        (push (cons :tools tools) body))
      body)))

;; --- Response Extraction (yason-based, no Python) ---

(defun extract-anthropic-response-text (json-text)
  "Extract assistant text from an Anthropic messages API response.

   Uses yason for JSON parsing — no Python subprocess required."
  (handler-case
      (let ((obj (json-decode json-text)))
        (let ((error (getf obj :error)))
          (when error
            (let ((msg (if (listp error)
                           (or (getf error :message) (format nil "~A" error))
                           (format nil "~A" error))))
              (return-from extract-anthropic-response-text msg))))
        (let ((content (getf obj :content)))
          (if (and content (listp content))
              (with-output-to-string (out)
                (dolist (part content)
                  (when (and (listp part)
                             (string= (getf part :type) "text"))
                    (write-string (getf part :text) out))))
              json-text)))
    (error (condition)
      (format nil "[JSON parse error: ~A]" condition))))

(defun extract-anthropic-tool-calls (json-text)
  "Extract tool-use blocks from an Anthropic messages API response.

   Returns a list of tool-call plists: (:id ... :name ... :input ...).
   Uses yason for JSON parsing."
  (handler-case
      (let ((obj (json-decode json-text)))
        (let ((content (getf obj :content)))
          (when (and content (listp content))
            (loop for part in content
                  when (and (listp part)
                            (string= (getf part :type) "tool_use"))
                  collect (list :id (getf part :id)
                                :name (getf part :name)
                                :input (getf part :input))))))
    (error (condition)
      (declare (ignore condition))
      nil)))

(defun extract-openrouter-response-text (json-text)
  "Extract assistant text from an OpenRouter chat-completions response.

   Uses yason for JSON parsing — no Python subprocess required."
  (handler-case
      (let ((obj (json-decode json-text)))
        (let ((error (getf obj :error)))
          (when error
            (let ((msg (if (listp error)
                           (or (getf error :message) (format nil "~A" error))
                           (format nil "~A" error))))
              (return-from extract-openrouter-response-text msg))))
        (let ((choices (getf obj :choices)))
          (if (and choices (listp choices) (not (null choices)))
              (let* ((first-choice (first choices))
                     (message (getf first-choice :message))
                     (content (getf message :content)))
                (if (stringp content)
                    content
                    (if (listp content)
                        (with-output-to-string (out)
                          (dolist (part content)
                            (when (and (listp part)
                                       (string= (getf part :type) "text"))
                              (write-string (getf part :text) out))))
                        (format nil "~A" content))))
              json-text)))
    (error (condition)
      (format nil "[JSON parse error: ~A]" condition))))

(defun extract-openrouter-tool-calls (json-text)
  "Extract tool_calls from an OpenRouter chat-completions response.

   Returns a list of tool-call plists: (:id ... :name ... :input ...)."
  (handler-case
      (let ((obj (json-decode json-text)))
        (let ((choices (getf obj :choices)))
          (when (and choices (listp choices) (not (null choices)))
            (let* ((first-choice (first choices))
                   (message (getf first-choice :message))
                   (tool-calls (getf message :tool_calls)))
              (when (and tool-calls (listp tool-calls))
                (loop for tc in tool-calls
                      collect
                      (let* ((function (getf tc :function))
                             (name (getf function :name))
                             (args-str (getf function :arguments))
                             (args (handler-case
                                      (json-decode args-str)
                                    (error () args-str))))
                        (list :id (getf tc :id)
                              :name name
                              :input args))))))))
    (error (condition)
      (declare (ignore condition))
      nil)))

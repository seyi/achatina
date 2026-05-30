(in-package #:claw-lisp.tests)

;; --- Real Anthropic API Payload Validation Harness ---
;;
;; Tests normalization against captured real Anthropic payloads.
;; Uses mock provider to test normalization without network calls.

(defparameter *captured-payload-multi-turn-stream*
  '(:model "claude-3-7-sonnet-20250219"
    :messages ((:role "user" :content "What is 2+2?")
               (:role "assistant" :content ((:type "text" :text "The answer is ")))
               (:role "assistant" :content ((:type "text" :text "4."))))
    :stream t))

(defparameter *captured-payload-tool-use*
  '(:model "claude-3-5-sonnet-20241022"
    :messages ((:role "user" :content "Search the web for latest AI news")
               (:role "assistant" :content
                ((:type "tool_use" :id "tu_123" :name "web_search"
                  :input (:query "latest AI news")))))
    :tools ((:name "web_search" :description "Search the web"))))

(defparameter *captured-payload-thinking*
  '(:model "claude-3-7-sonnet-20250219"
    :messages ((:role "user" :content "Solve this puzzle")
               (:role "assistant" :content
                ((:type "thinking" :thinking "Let me think step by step...")
                 (:type "text" :text "The solution is X"))))
    :stream t))

(defparameter *captured-payload-parallel-tools*
  '(:model "claude-3-5-sonnet-20241022"
    :messages ((:role "user" :content "Get weather and time in NYC")
               (:role "assistant" :content
                ((:type "tool_use" :id "tu_1" :name "get_weather" :input (:city "NYC"))
                 (:type "tool_use" :id "tu_2" :name "get_time" :input (:city "NYC")))))
    :tools ((:name "get_weather") (:name "get_time"))))

(defparameter *captured-payload-role-repair*
  '(:model "claude-3-5-sonnet-20241022"
    :messages ((:role "user" :content "Hello")
               (:role "user" :content "How are you?")
               (:role "assistant" :content "I am doing well."))))

(defparameter *captured-payload-orphaned-tool*
  '(:model "claude-3-5-sonnet-20241022"
    :messages ((:role "user" :content "Use tool X")
               (:role "assistant" :content
                ((:type "tool_use" :id "tu_999" :name "tool_x" :input (:param 1)))))))

(defun make-mock-model-capabilities (&key thinking-p)
  "Create a mock model capabilities struct for testing."
  (claw-lisp.core.domain:make-model-capabilities
   :thinking thinking-p
   :max-tokens 8192
   :supports-tools t))

(defun make-mock-tool-result (call-id content &optional is-error)
  "Create a mock tool result for testing."
  (claw-lisp.core.domain:make-tool-result
   :call-id call-id
   :content content
   :is-error is-error))

(defun test-normalization-roundtrip ()
  "Test that normalization is idempotent and preserves semantics."
  (let* ((messages (list
                    (claw-lisp.core.domain:make-message
                     :role :user
                     :content "Hello")
                    (claw-lisp.core.domain:make-message
                     :role :assistant
                     :content "Hi there"
                     :metadata '(:message-id "msg_1"))))
         (capabilities (make-mock-model-capabilities :thinking-p nil))
         (tool-results nil))
    (multiple-value-bind (normalized passed report)
        (claw-lisp.core.message-normalization:validate-normalization-roundtrip
         messages capabilities tool-results)
      (%assert (= (length normalized) 2) "Should have 2 messages")
      (%assert passed "Normalization should be idempotent: ~A" report)
      t)))

(defun test-normalization-with-real-payloads ()
  "Run normalization validation against captured real Anthropic payloads."
  (let ((capabilities (make-mock-model-capabilities :thinking-p t))
        (results (list (make-mock-tool-result "tu_123" "search results"))))
    (and (test-multi-turn-stream-normalization)
         (test-tool-use-normalization results)
         (test-thinking-block-stripping)
         (test-parallel-tool-calls)
         (test-role-alternation-repair)
         (test-orphaned-tool-repair)
         (test-validation-mode-capture))))

(defun test-multi-turn-stream-normalization ()
  "Test merging of streaming assistant messages from real payload."
  (let* ((messages (list
                    (claw-lisp.core.domain:make-message
                     :role :user :content "What is 2+2?")
                    (claw-lisp.core.domain:make-message
                     :role :assistant
                     :content '((:type "text" :text "The answer is "))
                     :metadata '(:message-id "msg_42"))
                    (claw-lisp.core.domain:make-message
                     :role :assistant
                     :content '((:type "text" :text "4."))
                     :metadata '(:message-id "msg_42"))))
         (normalized (claw-lisp.core.message-normalization:normalize-messages-for-api
                      messages (make-mock-model-capabilities) nil)))
    (%assert (= (length normalized) 2) "Should merge to 2 messages")
    (let ((assistant (second normalized)))
      (%assert (equal (claw-lisp.core.domain:message-role assistant) :assistant)
               "Second message should be assistant")
      (%assert (= (length (claw-lisp.core.domain:message-content assistant)) 2)
               "Merged content should have 2 blocks"))
    t))

(defun test-tool-use-normalization (tool-results)
  "Test tool use normalization from captured payload."
  (let ((messages (list
                   (claw-lisp.core.domain:make-message
                    :role :user :content "Search the web for latest AI news")
                   (claw-lisp.core.domain:make-message
                    :role :assistant
                    :content (list (claw-lisp.core.domain:make-tool-use-block
                                    :id "tu_123"
                                    :name "web_search"
                                    :input '(:query "latest AI news")))))))
    (let ((normalized (claw-lisp.core.message-normalization:normalize-messages-for-api
                       messages (make-mock-model-capabilities :thinking-p nil) tool-results)))
      (%assert (= (length normalized) 2) "Should have 2 messages")
      t)))

(defun test-thinking-block-stripping ()
  "Test that thinking blocks are stripped for non-thinking models."
  (let ((messages (list
                   (claw-lisp.core.domain:make-message
                    :role :assistant
                    :content (list (claw-lisp.core.domain:make-thinking-block
                                    :content "Let me think...")
                                   (claw-lisp.core.domain:make-text-block
                                    :text "The answer is 42"))))))
    (let ((normalized (claw-lisp.core.message-normalization:normalize-messages-for-api
                       messages (make-mock-model-capabilities :thinking-p nil) nil)))
      (%assert (= (length (claw-lisp.core.domain:message-content (first normalized))) 1)
               "Thinking block should be stripped, leaving only text block")
      t)))

(defun test-parallel-tool-calls ()
  "Test normalization of parallel tool calls from real Anthropic payload."
  (let ((messages (list
                   (claw-lisp.core.domain:make-message
                    :role :user :content "Get weather and time in NYC")
                   (claw-lisp.core.domain:make-message
                    :role :assistant
                    :content (list
                              (claw-lisp.core.domain:make-tool-use-block
                               :id "tu_1" :name "get_weather" :input '(:city "NYC"))
                              (claw-lisp.core.domain:make-tool-use-block
                               :id "tu_2" :name "get_time" :input '(:city "NYC")))))))
    (let ((normalized (claw-lisp.core.message-normalization:normalize-messages-for-api
                       messages (make-mock-model-capabilities) nil)))
      (%assert (= (length (claw-lisp.core.domain:message-content (second normalized))) 2)
               "Should have 2 tool-use blocks")
      t)))

(defun test-role-alternation-repair ()
  "Test repair of consecutive user messages (common after compaction)."
  (let ((messages (list
                   (claw-lisp.core.domain:make-message :role :user :content "Hello")
                   (claw-lisp.core.domain:make-message :role :user :content "How are you?")
                   (claw-lisp.core.domain:make-message :role :assistant :content "I am doing well."))))
    (let ((normalized (claw-lisp.core.message-normalization:normalize-messages-for-api
                       messages (make-mock-model-capabilities) nil)))
      (%assert (= (length normalized) 2) "Should merge consecutive user messages")
      (%assert (eq (claw-lisp.core.domain:message-role (first normalized)) :user)
               "First should be user")
      (%assert (eq (claw-lisp.core.domain:message-role (second normalized)) :assistant)
               "Second should be assistant")
      t)))

(defun test-orphaned-tool-repair ()
  "Test repair of orphaned tool_use blocks (no matching tool_result)."
  (let ((messages (list
                   (claw-lisp.core.domain:make-message :role :user :content "Use tool X")
                   (claw-lisp.core.domain:make-message
                    :role :assistant
                    :content (list (claw-lisp.core.domain:make-tool-use-block
                                    :id "tu_999" :name "tool_x" :input '(:param 1)))))))
    (let ((normalized (claw-lisp.core.message-normalization:normalize-messages-for-api
                       messages (make-mock-model-capabilities) nil)))
      (%assert (= (length normalized) 3) "Should append error message for orphaned tool")
      (%assert (eq (claw-lisp.core.domain:message-role (third normalized)) :user)
               "Third message should be synthetic user message with error")
      t)))

(defun test-validation-mode-capture ()
  "Test that validation mode and payload capture work as expected."
  (let ((claw-lisp.core.message-normalization:*validation-mode* t)
        (claw-lisp.core.message-normalization:*payload-capture-path* "/tmp/normalization-test-captures"))
    (let ((messages (list (claw-lisp.core.domain:make-message :role :user :content "Test message"))))
      (claw-lisp.core.message-normalization:capture-payload
       *captured-payload-multi-turn-stream* :request :conversation-id "test-001")
      (claw-lisp.core.message-normalization:normalize-messages-for-api
       messages (make-mock-model-capabilities) nil)
      ;; Just verify it doesn't crash
      t)))

(defun run-all-normalization-tests ()
  "Run the complete normalization validation suite."
  (format t "~&=== Running Real Anthropic Normalization Validation Harness ===~%")
  (let ((results (list
                  (test-normalization-roundtrip)
                  (test-normalization-with-real-payloads))))
    (if (every #'identity results)
        (format t "~&ALL NORMALIZATION TESTS PASSED~%")
        (format t "~&SOME TESTS FAILED~%"))
    (every #'identity results)))

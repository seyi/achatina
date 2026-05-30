;;;; lisp/tests/token-estimation.lisp
;;;;
;;;; Unit tests for token estimation module.
;;;; Self-contained - does not depend on runtime.lisp test helpers.

(in-package #:claw-lisp.tests)

;;; ============================================================
;;; Local Test Helper
;;; ============================================================

(defun %assert (condition format-control &rest format-args)
  "Assert that CONDITION is true. Signals an error with FORMAT-ARGS on failure."
  (unless condition
    (error (apply #'format nil format-control format-args))))

;;; ============================================================
;;; Test Helpers
;;; ============================================================

(defun make-test-message (role content)
  "Create a test message struct."
  (claw-lisp.core.domain:make-message
   :role role
   :content content))

(defun make-test-conversation (&key messages tool-results)
  "Create a test conversation struct."
  (claw-lisp.core.domain:make-conversation
   :id "test-conversation"
   :messages (or messages nil)
   :tool-results (or tool-results nil)))

;;; ============================================================
;;; Token Estimation Tests
;;; ============================================================

(defun test-estimate-empty-string ()
  "Handles NIL and empty strings gracefully."
  (let ((empty (claw-lisp.core.token-estimation:estimate-string-tokens ""))
        (null (claw-lisp.core.token-estimation:estimate-string-tokens nil)))
    (%assert (= 0 empty)
             "Expected 0 for empty string, got ~A" empty)
    (%assert (= 0 null)
             "Expected 0 for NIL string, got ~A" null)))

(defun test-estimate-known-string ()
  "Verifies ceiling math against known length.
   \"Hello, world!\" = 13 chars → ceil(13 / 3.5) = 4 tokens"
  (let ((result (claw-lisp.core.token-estimation:estimate-string-tokens "Hello, world!")))
    (%assert (= 4 result)
             "Expected 4 tokens for 'Hello, world!', got ~A" result)))

(defun test-estimate-message-includes-overhead ()
  "Message token estimate includes structural overhead."
  (let* ((msg (make-test-message :user "Hello"))
         (result (claw-lisp.core.token-estimation:estimate-message-tokens msg)))
    ;; "Hello" = 5 chars → ceil(5/3.5) = 2 tokens + 4 overhead = 6
    (%assert (= 6 result)
             "Expected 6 tokens (2 content + 4 overhead), got ~A" result)))

(defun test-estimate-conversation-scales-linearly ()
  "Estimator scales linearly with message count and applies safety margin."
  (let* ((messages (loop repeat 10
                         collect (make-test-message :user (make-string 100 :initial-element #\a))))
         (conv (make-test-conversation :messages messages))
         (est (claw-lisp.core.token-estimation:estimate-conversation-tokens conv)))
    ;; 10 × (ceil(100/3.5) + 4) ≈ 330, × 1.05 ≈ 346
    (%assert (> est 300)
             "Expected >300 tokens for 10×100-char messages, got ~A" est)
    (%assert (< est 500)
             "Expected <500 tokens for 10×100-char messages, got ~A" est)))

(defun test-estimate-conversation-includes-tool-results ()
  "Tool results are included in conversation token count."
  (let* ((tool-results (list (claw-lisp.core.domain:make-tool-result
                              :call-id "test-1"
                              :tool-name "test_tool"
                              :content (make-string 200 :initial-element #\c))))
         (conv (make-test-conversation :tool-results tool-results))
         (est (claw-lisp.core.token-estimation:estimate-conversation-tokens conv)))
    ;; 1 × ceil(200/3.5) × 1.05 ≈ 60
    (%assert (> est 50)
             "Expected >50 tokens for 200-char tool result, got ~A" est)))

(defun test-estimate-total-includes-system-prompt ()
  "System prompt adds to base conversation estimate."
  (let* ((messages (list (make-test-message :user "Hello")))
         (conv (make-test-conversation :messages messages))
         (system "You are a helpful assistant with extensive knowledge.")
         (conv-tokens (claw-lisp.core.token-estimation:estimate-conversation-tokens conv))
         (total (claw-lisp.core.token-estimation:estimate-total-request-tokens
                 conv :system-prompt system)))
    (%assert (> total conv-tokens)
             "Expected total > conversation-only (~A vs ~A)" total conv-tokens)))

(defun test-estimate-total-includes-tool-definitions ()
  "Tool definitions add to total request estimate."
  (let* ((conv (make-test-conversation :messages nil))
         (tools (list (list :name "test" :description "A test tool"
                            :input-schema (list :type "object"))))
         (total (claw-lisp.core.token-estimation:estimate-total-request-tokens
                 conv :tool-definitions tools)))
    (%assert (> total 0)
             "Expected non-zero estimate for tool definitions, got ~A" total)))

(defun test-estimate-safety-margin-applied ()
  "Safety margin (1.05x) is applied to conversation estimates."
  (let* ((msg (make-test-message :user (make-string 100 :initial-element #\x)))
         (conv (make-test-conversation :messages (list msg)))
         (base (claw-lisp.core.token-estimation:estimate-message-tokens msg))
         (with-margin (claw-lisp.core.token-estimation:estimate-conversation-tokens conv)))
    ;; base = ceil(100/3.5) + 4 = 33, with 1.05 margin = ceil(33 * 1.05) = 35
    (%assert (>= with-margin (ceiling (* base 1.05)))
             "Expected ~A with margin, got ~A" (ceiling (* base 1.05)) with-margin)))

;;; ============================================================
;;; Test Runner
;;; ============================================================

(defun run-token-estimation-tests ()
  "Run all token estimation tests. Returns T if all pass."
  (format t "~&=== Token Estimation Tests ===~%")
  (let ((results (list
                  (test-estimate-empty-string)
                  (test-estimate-known-string)
                  (test-estimate-message-includes-overhead)
                  (test-estimate-conversation-scales-linearly)
                  (test-estimate-conversation-includes-tool-results)
                  (test-estimate-total-includes-system-prompt)
                  (test-estimate-total-includes-tool-definitions)
                  (test-estimate-safety-margin-applied))))
    (if (every #'identity results)
        (progn
          (format t "~&ALL TOKEN ESTIMATION TESTS PASSED~%")
          t)
        (progn
          (format t "~&SOME TOKEN ESTIMATION TESTS FAILED~%")
          nil))))

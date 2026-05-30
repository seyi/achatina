;;;; lisp/core/token-estimation.lisp
;;;;
;;;; Fast, local token estimation for context threshold checks.
;;;; Uses character-based heuristics — no API calls.
;;;;
;;;; Design principle: Slightly overestimate to avoid hitting 413 errors.
;;;; Conservative ratio (3.5 chars/token) + 5% safety margin.

(in-package #:claw-lisp.core.token-estimation)

;;; ============================================================
;;; Tunable Heuristics
;;; ============================================================

(defparameter +chars-per-token+ 3.5d0
  "Conservative character-to-token ratio.
   Real ratio is ~3.8-4.2 for English text with Claude models.
   We use 3.5 to err on the side of overestimating (safer for threshold checks).")

(defparameter +message-overhead-tokens+ 4
  "Estimated token overhead per message for role markers, content-type, and JSON framing.")

(defparameter +safety-margin+ 1.05d0
  "5% safety margin applied to final conversation-level estimates.
   Stacks on top of conservative chars-per-token ratio.")

;;; ============================================================
;;; Core Estimation Functions
;;; ============================================================

(declaim (ftype (function ((or null string)) fixnum) estimate-string-tokens))
(defun estimate-string-tokens (string)
  "Estimate token count for STRING using character-based heuristic.
   Returns 0 for NIL or empty strings."
  (declare (optimize (speed 3) (safety 1) (debug 0)))
  (if (or (null string) (zerop (length string)))
      0
      (ceiling (/ (length string) +chars-per-token+))))

(declaim (ftype (function (t) fixnum) estimate-message-tokens))
(defun estimate-message-tokens (message)
  "Estimate tokens for a single MESSAGE including structural overhead."
  (declare (optimize (speed 3) (safety 1) (debug 0)))
  (let ((text (claw-lisp.core.domain:message-content-text message)))
    (+ +message-overhead-tokens+ (estimate-string-tokens text))))

(declaim (ftype (function (t) fixnum) estimate-conversation-tokens))
(defun estimate-conversation-tokens (conversation)
  "Estimate total tokens for all messages and tool results in CONVERSATION.
   Applies safety margin to final estimate."
  (declare (optimize (speed 3) (safety 1) (debug 0)))
  (let* ((messages (claw-lisp.core.domain:conversation-messages conversation))
         (tool-results (claw-lisp.core.domain:conversation-tool-results conversation))
         (msg-tokens (reduce #'+ messages
                             :key #'estimate-message-tokens
                             :initial-value 0))
         (tool-tokens (reduce #'+ tool-results
                              :key (lambda (r)
                                     (estimate-string-tokens
                                      (claw-lisp.core.domain:tool-result-content r)))
                              :initial-value 0)))
    (ceiling (* +safety-margin+ (+ msg-tokens tool-tokens)))))

;;; ============================================================
;;; Request-Level Aggregation
;;; ============================================================

(defun estimate-system-prompt-tokens (system-prompt)
  "Estimate tokens for the system prompt string."
  (estimate-string-tokens system-prompt))

(defun estimate-tool-definitions-tokens (tool-definitions)
  "Estimate tokens for a list of tool definition objects."
  (if tool-definitions
      (reduce #'+ tool-definitions
              :key (lambda (td)
                     (estimate-string-tokens (princ-to-string td)))
              :initial-value 0)
      0))

(defun estimate-total-request-tokens (conversation
                                      &key system-prompt tool-definitions)
  "Estimate the total token count for a complete API request.
   Includes conversation messages, tool results, system prompt, and tool definitions.

   CONVERSATION is a claw-lisp.core.domain:conversation struct.
   SYSTEM-PROMPT is a string (optional).
   TOOL-DEFINITIONS is a list of tool definition plists (optional).

   Returns estimated token count as a fixnum."
  (declare (optimize (speed 3) (safety 1) (debug 0)))
  (+ (estimate-conversation-tokens conversation)
     (estimate-system-prompt-tokens system-prompt)
     (estimate-tool-definitions-tokens tool-definitions)))

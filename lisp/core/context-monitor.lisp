;;;; lisp/core/context-monitor.lisp
;;;;
;;;; Context usage monitoring and threshold-based decision logic.
;;;;
;;;; This module evaluates context usage against configurable thresholds
;;;; and returns actionable decisions. It does NOT perform compaction —
;;;; callers act on the returned status.
;;;;
;;;; Design principle: Fast, read-only assessment. No side effects.

(in-package #:claw-lisp.core.context-monitor)

;;; ============================================================
;;; Status Data Structure
;;; ============================================================

(defstruct context-status
  "Result of a context usage assessment.
   Callers act on this; this module only computes the decision.

   ACTION — recommended action (:none, :microcompact, :aggressive-microcompact, :full-compaction)
   THRESHOLD-NAME — which threshold triggered the action (:warning, :compact-suggested, :compact-required)
   USAGE-RATIO — current usage as fraction of effective limit (0.0-1.0+)
   ESTIMATED-TOKENS — estimated token count for the request
   CONTEXT-LIMIT — effective context limit (after reserving output tokens)
   HEADROOM-TOKENS — remaining tokens before hitting limit"
  (action :none
   :type (member :none :microcompact :aggressive-microcompact :full-compaction))
  (threshold-name nil :type (or null keyword))
  (usage-ratio 0.0 :type single-float)
  (estimated-tokens 0 :type (integer 0))
  (context-limit 0 :type (integer 0))
  (headroom-tokens 0 :type (integer 0)))

;;; ============================================================
;;; Core Assessment Logic
;;; ============================================================

(defun assess-context-with-limits (config effective-limit estimated-tokens)
  "Assess context usage given pre-computed limits and token estimates.
   Used by tests and by assess-context internally.

   CONFIG — runtime-config with threshold settings
   EFFECTIVE-LIMIT — effective context limit (after reserving output tokens)
   ESTIMATED-TOKENS — estimated token count for the request

   Returns: context-status struct with action recommendation"
  (declare (optimize (speed 3) (safety 1) (debug 0)))
  (let* ((ratio (if (> effective-limit 0)
                    (float (/ estimated-tokens effective-limit))
                    1.0))
         (headroom (max 0 (- effective-limit estimated-tokens)))
         (warning-thresh (claw-lisp.config:runtime-config-context-warning-threshold config))
         (suggested-thresh (claw-lisp.config:runtime-config-context-compact-suggested-threshold config))
         (required-thresh (claw-lisp.config:runtime-config-context-compact-required-threshold config)))
    (cond
      ((>= ratio required-thresh)
       (make-context-status
        :action :full-compaction
        :threshold-name :compact-required
        :usage-ratio ratio
        :estimated-tokens estimated-tokens
        :context-limit effective-limit
        :headroom-tokens headroom))
      ((>= ratio suggested-thresh)
       (make-context-status
        :action :aggressive-microcompact
        :threshold-name :compact-suggested
        :usage-ratio ratio
        :estimated-tokens estimated-tokens
        :context-limit effective-limit
        :headroom-tokens headroom))
      ((>= ratio warning-thresh)
       (make-context-status
        :action :microcompact
        :threshold-name :warning
        :usage-ratio ratio
        :estimated-tokens estimated-tokens
        :context-limit effective-limit
        :headroom-tokens headroom))
      (t
       (make-context-status
        :action :none
        :threshold-name nil
        :usage-ratio ratio
        :estimated-tokens estimated-tokens
        :context-limit effective-limit
        :headroom-tokens headroom)))))

(defun assess-context (config model-registry model-id conversation
                       &key system-prompt tool-definitions)
  "Assess context usage and return a CONTEXT-STATUS with the recommended action.

   Does NOT perform any compaction — callers act on the returned status.
   Reserves output tokens from the context window to prevent late 413 errors.

   CONFIG — runtime-config with threshold settings
   MODEL-REGISTRY — model capability registry
   MODEL-ID — model identifier string
   CONVERSATION — conversation struct to estimate
   SYSTEM-PROMPT — optional system prompt string
   TOOL-DEFINITIONS — optional list of tool definition plists

   Returns: context-status struct with action recommendation"
  (declare (optimize (speed 3) (safety 1) (debug 0)))
  (let* ((caps (claw-lisp.core.model-registry:resolve-model model-registry model-id))
         (context-limit (claw-lisp.core.model-registry:model-capabilities-context-window caps))
         (max-output (claw-lisp.core.model-registry:model-capabilities-max-output-tokens caps))
         ;; Reserve space for the model's response
         (effective-limit (if (and context-limit max-output)
                              (- context-limit max-output)
                              (or context-limit most-positive-fixnum)))
         (estimated (claw-lisp.core.token-estimation:estimate-total-request-tokens
                     conversation
                     :system-prompt system-prompt
                     :tool-definitions tool-definitions)))
    (assess-context-with-limits config effective-limit estimated)))

;;; ============================================================
;;; Warning Formatter
;;; ============================================================

(defun format-context-warning (status)
  "Return a human-readable warning string for STATUS, or NIL if no warning needed.

   STATUS — context-status struct from assess-context

   Returns: warning string or NIL"
  (case (context-status-action status)
    (:none nil)
    (:microcompact
     (format nil "⚠ Context usage at ~,1F% (~:D/~:D tokens). Running cleanup."
             (* 100 (context-status-usage-ratio status))
             (context-status-estimated-tokens status)
             (context-status-context-limit status)))
    (:aggressive-microcompact
     (format nil "⚠⚠ Context usage at ~,1F% (~:D/~:D tokens). Aggressive cleanup in progress."
             (* 100 (context-status-usage-ratio status))
             (context-status-estimated-tokens status)
             (context-status-context-limit status)))
    (:full-compaction
     (format nil "🔴 Context usage at ~,1F% (~:D/~:D tokens). Full compaction required."
             (* 100 (context-status-usage-ratio status))
             (context-status-estimated-tokens status)
             (context-status-context-limit status)))))

;;; ============================================================
;;; Idle-Gap Trigger (Phase 4 Step 5)
;;; ============================================================

(defun idle-gap-microcompact-needed-p (config gap-seconds usage-ratio)
  "Return T when an idle-gap microcompact should trigger.

   Conditions:
   1. Idle gap exceeds configured threshold
   2. Context usage exceeds minimum ratio"
  (declare (optimize (speed 3) (safety 1) (debug 0)))
  (and gap-seconds
       (>= gap-seconds (claw-lisp.config:runtime-config-idle-gap-microcompact-seconds config))
       (>= usage-ratio (claw-lisp.config:runtime-config-idle-gap-minimum-usage-ratio config))))

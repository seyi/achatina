;;;; lisp/tests/phase4-integration.lisp
;;;;
;;;; Integration tests for Phase 4 auto-triggered context management.
;;;; Tests the full flow: token estimation → context monitor → triggers → compaction.

(in-package #:claw-lisp.tests)

;;; ============================================================
;;; Local Test Helper
;;; ============================================================

(defun %assert (condition format-control &rest format-args)
  "Assert that CONDITION is true. Signals an error with FORMAT-ARGS on failure."
  (unless condition
    (error (apply #'format nil format-control format-args))))

;;; ============================================================
;;; Integration Tests
;;; ============================================================

(defun test-token-estimation-accuracy ()
  "Verify token estimation produces reasonable values for known inputs."
  (let* ((short-text "Hello, world!")
         (short-est (claw-lisp.core.token-estimation:estimate-string-tokens short-text))
         (long-text (make-string 1000 :initial-element #\a))
         (long-est (claw-lisp.core.token-estimation:estimate-string-tokens long-text)))
    ;; "Hello, world!" = 13 chars → ceil(13/3.5) = 4 tokens
    (%assert (= 4 short-est)
             "Expected 4 tokens for 'Hello, world!', got ~A" short-est)
    ;; 1000 chars → ceil(1000/3.5) = 286 tokens
    (%assert (= 286 long-est)
             "Expected 286 tokens for 1000-char string, got ~A" long-est)
    t))

(defun test-context-thresholds-correct ()
  "Verify threshold config defaults match Opus 4.6 plan (75%/85%/95%)."
  (let* ((config (claw-lisp.config:make-default-runtime-config))
         (warning (claw-lisp.config:runtime-config-context-warning-threshold config))
         (suggested (claw-lisp.config:runtime-config-context-compact-suggested-threshold config))
         (required (claw-lisp.config:runtime-config-context-compact-required-threshold config)))
    (%assert (= 0.75 warning)
             "Expected warning threshold 0.75, got ~A" warning)
    (%assert (= 0.85 suggested)
             "Expected suggested threshold 0.85, got ~A" suggested)
    (%assert (= 0.95 required)
             "Expected required threshold 0.95, got ~A" required)
    t))

(defun test-idle-gap-config-correct ()
  "Verify idle-gap config defaults (120s gap, 50% usage)."
  (let* ((config (claw-lisp.config:make-default-runtime-config))
         (gap-secs (claw-lisp.config:runtime-config-idle-gap-microcompact-seconds config))
         (min-ratio (claw-lisp.config:runtime-config-idle-gap-minimum-usage-ratio config)))
    (%assert (= 120 gap-secs)
             "Expected idle-gap 120 seconds, got ~A" gap-secs)
    (%assert (= 0.50 min-ratio)
             "Expected idle-gap minimum ratio 0.50, got ~A" min-ratio)
    t))

(defun test-assess-context-with-limits-thresholds ()
  "Verify assess-context-with-limits triggers correct actions at each threshold."
  (let* ((config (claw-lisp.config:make-default-runtime-config))
         (effective-limit 183616) ; 200K - 16384
         ;; Test each threshold boundary
         (below-warning (claw-lisp.core.context-monitor:assess-context-with-limits
                         config effective-limit 100000)) ; ~54%
         (at-warning (claw-lisp.core.context-monitor:assess-context-with-limits
                      config effective-limit 140000)) ; ~76%
         (at-suggested (claw-lisp.core.context-monitor:assess-context-with-limits
                        config effective-limit 160000)) ; ~87%
         (at-required (claw-lisp.core.context-monitor:assess-context-with-limits
                       config effective-limit 178000))) ; ~97%
    (%assert (eq :none (claw-lisp.core.context-monitor:context-status-action below-warning))
             "Expected :none below warning, got ~A" (claw-lisp.core.context-monitor:context-status-action below-warning))
    (%assert (eq :microcompact (claw-lisp.core.context-monitor:context-status-action at-warning))
             "Expected :microcompact at warning, got ~A" (claw-lisp.core.context-monitor:context-status-action at-warning))
    (%assert (eq :aggressive-microcompact (claw-lisp.core.context-monitor:context-status-action at-suggested))
             "Expected :aggressive-microcompact at suggested, got ~A" (claw-lisp.core.context-monitor:context-status-action at-suggested))
    (%assert (eq :full-compaction (claw-lisp.core.context-monitor:context-status-action at-required))
             "Expected :full-compaction at required, got ~A" (claw-lisp.core.context-monitor:context-status-action at-required))
    t))

(defun test-format-context-warning-output ()
  "Verify warning formatter produces readable output."
  (let* ((warning-status (claw-lisp.core.context-monitor:make-context-status
                          :action :microcompact
                          :threshold-name :warning
                          :usage-ratio 0.78
                          :estimated-tokens 143000
                          :context-limit 183616
                          :headroom-tokens 40616))
         (warning-text (claw-lisp.core.context-monitor:format-context-warning warning-status)))
    (%assert (stringp warning-text)
             "Expected string warning, got ~A" warning-text)
    (%assert (search "⚠" warning-text)
             "Expected warning emoji in text")
    (%assert (search "78.0%" warning-text)
             "Expected percentage in warning")
    (%assert (search "143,000" warning-text)
             "Expected token count in warning")
    t))

(defun test-idle-gap-microcompact-needed-predicate ()
  "Verify idle-gap trigger predicate logic."
  (let* ((config (claw-lisp.config:make-default-runtime-config))
         ;; Gap too short (60s < 120s)
         (gap-short (claw-lisp.core.context-monitor:idle-gap-microcompact-needed-p config 60 0.80))
         ;; Usage too low (40% < 50%)
         (usage-low (claw-lisp.core.context-monitor:idle-gap-microcompact-needed-p config 180 0.40))
         ;; Both conditions met
         (both-met (claw-lisp.core.context-monitor:idle-gap-microcompact-needed-p config 180 0.60)))
    (%assert (null gap-short)
             "Expected NIL when gap too short")
    (%assert (null usage-low)
             "Expected NIL when usage too low")
    (%assert (eq t both-met)
             "Expected T when both conditions met")
    t))

;;; ============================================================
;;; Test Runner
;;; ============================================================

(defun run-phase4-integration-tests ()
  "Run all Phase 4 integration tests. Returns T if all pass."
  (format t "~&=== Phase 4 Integration Tests ===~%")
  (let ((results (list
                  (test-token-estimation-accuracy)
                  (test-context-thresholds-correct)
                  (test-idle-gap-config-correct)
                  (test-assess-context-with-limits-thresholds)
                  (test-format-context-warning-output)
                  (test-idle-gap-microcompact-needed-predicate))))
    (if (every #'identity results)
        (progn
          (format t "~&ALL PHASE 4 INTEGRATION TESTS PASSED~%")
          t)
        (progn
          (format t "~&SOME PHASE 4 INTEGRATION TESTS FAILED~%")
          nil))))

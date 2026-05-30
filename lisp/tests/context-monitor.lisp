;;;; lisp/tests/context-monitor.lisp
;;;;
;;;; Unit tests for context monitor module.
;;;; Uses assess-context-with-limits for isolated testing.

(in-package #:claw-lisp.tests)

;;; ============================================================
;;; Local Test Helper
;;; ============================================================

(defun %assert (condition format-control &rest format-args)
  "Assert that CONDITION is true. Signals an error with FORMAT-ARGS on failure."
  (unless condition
    (error (apply #'format nil format-control format-args))))

;;; ============================================================
;;; Test Fixtures (Isolated Mocks)
;;; ============================================================

(defstruct test-config
  "Mock config with threshold settings."
  (context-warning-threshold 0.75 :type single-float)
  (context-compact-suggested-threshold 0.85 :type single-float)
  (context-compact-required-threshold 0.95 :type single-float))

;;; ============================================================
;;; Context Monitor Tests
;;; ============================================================

(defun test-assess-below-all-thresholds ()
  "Usage < warning threshold → action :none"
  (let* ((config (make-test-config))
         ;; Effective limit = 200000 - 16384 = 183616. 50000 tokens = ~27%
         (status (claw-lisp.core.context-monitor:assess-context-with-limits
                  config 183616 50000)))
    (%assert (eq :none (claw-lisp.core.context-monitor:context-status-action status))
             "Expected :none action, got ~A" (claw-lisp.core.context-monitor:context-status-action status))
    (%assert (< (claw-lisp.core.context-monitor:context-status-usage-ratio status) 0.75)
             "Expected ratio < 0.75, got ~A" (claw-lisp.core.context-monitor:context-status-usage-ratio status))))

(defun test-assess-at-warning-threshold ()
  "Usage >= 0.75 of effective limit → action :microcompact"
  (let* ((config (make-test-config))
         ;; 75% of 183616 = ~137712
         (status (claw-lisp.core.context-monitor:assess-context-with-limits
                  config 183616 140000)))
    (%assert (eq :microcompact (claw-lisp.core.context-monitor:context-status-action status))
             "Expected :microcompact action, got ~A" (claw-lisp.core.context-monitor:context-status-action status))
    (%assert (eq :warning (claw-lisp.core.context-monitor:context-status-threshold-name status))
             "Expected :warning threshold, got ~A" (claw-lisp.core.context-monitor:context-status-threshold-name status))))

(defun test-assess-at-required-threshold ()
  "Usage >= 0.95 of effective limit → action :full-compaction"
  (let* ((config (make-test-config))
         ;; 95% of 183616 = ~174435
         (status (claw-lisp.core.context-monitor:assess-context-with-limits
                  config 183616 178000)))
    (%assert (eq :full-compaction (claw-lisp.core.context-monitor:context-status-action status))
             "Expected :full-compaction action, got ~A" (claw-lisp.core.context-monitor:context-status-action status))
    (%assert (eq :compact-required (claw-lisp.core.context-monitor:context-status-threshold-name status))
             "Expected :compact-required threshold, got ~A" (claw-lisp.core.context-monitor:context-status-threshold-name status))))

(defun test-effective-limit-accounts-for-output-tokens ()
  "Output reservation shrinks effective limit, pushing usage ratio higher"
  (let* ((config (make-test-config))
         ;; Small window: effective = 4096, 3500 tokens = ~85.4%
         (status (claw-lisp.core.context-monitor:assess-context-with-limits
                  config 4096 3500)))
    (%assert (eq :aggressive-microcompact (claw-lisp.core.context-monitor:context-status-action status))
             "Expected :aggressive-microcompact for small window, got ~A" (claw-lisp.core.context-monitor:context-status-action status))))

(defun test-format-warning-returns-nil-for-none ()
  "No warning string when action is :none"
  (let ((status (claw-lisp.core.context-monitor:make-context-status :action :none)))
    (%assert (null (claw-lisp.core.context-monitor:format-context-warning status))
             "Expected NIL for :none action, got ~A" (claw-lisp.core.context-monitor:format-context-warning status))))

(defun test-format-warning-returns-string-for-actions ()
  "Warning string contains percentage and token counts"
  (let* ((status (claw-lisp.core.context-monitor:make-context-status
                  :action :full-compaction
                  :usage-ratio 0.96
                  :estimated-tokens 175000
                  :context-limit 183616))
         (warning (claw-lisp.core.context-monitor:format-context-warning status)))
    (%assert (stringp warning)
             "Expected string warning, got ~A" warning)
    (%assert (search "96.0%" warning)
             "Expected percentage in warning, got ~A" warning)
    (%assert (search "175,000" warning)
             "Expected token count in warning, got ~A" warning)))

;;; ============================================================
;;; Test Runner
;;; ============================================================

(defun run-context-monitor-tests ()
  "Run all context monitor tests. Returns T if all pass."
  (format t "~&=== Context Monitor Tests ===~%")
  (let ((results (list
                  (test-assess-below-all-thresholds)
                  (test-assess-at-warning-threshold)
                  (test-assess-at-required-threshold)
                  (test-effective-limit-accounts-for-output-tokens)
                  (test-format-warning-returns-nil-for-none)
                  (test-format-warning-returns-string-for-actions))))
    (if (every #'identity results)
        (progn
          (format t "~&ALL CONTEXT MONITOR TESTS PASSED~%")
          t)
        (progn
          (format t "~&SOME CONTEXT MONITOR TESTS FAILED~%")
          nil))))

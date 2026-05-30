(in-package #:claw-lisp.core.conditions)

;; --- Condition Hierarchy ---
;;
;; All errors signaled by the runtime derive from claw-error.
;; This enables callers to catch all Claw Lisp errors with a single
;; handler-case, or to catch specific subtypes for targeted recovery.

(define-condition claw-error (error)
  ((message :initarg :message :reader claw-error-message))
  (:report (lambda (condition stream)
             (format stream "Claw Lisp error: ~A" (claw-error-message condition)))))

;; --- Provider Errors ---

(define-condition provider-error (claw-error)
  ((provider :initarg :provider :reader provider-error-provider)
   (status :initarg :status :reader provider-error-status)
   (response-body :initarg :response-body :reader provider-error-response-body :initform nil))
  (:report (lambda (condition stream)
             (format stream "Provider ~A error [status ~A]: ~A"
                     (provider-error-provider condition)
                     (provider-error-status condition)
                     (claw-error-message condition)))))

(define-condition rate-limit-error (provider-error)
  ((retry-after :initarg :retry-after :reader rate-limit-retry-after :initform nil))
  (:report (lambda (condition stream)
             (format stream "Rate limited by ~A [status ~A]~@[ retry after ~As~]: ~A"
                     (provider-error-provider condition)
                     (provider-error-status condition)
                     (rate-limit-retry-after condition)
                     (claw-error-message condition)))))

(define-condition auth-error (provider-error)
  ()
  (:report (lambda (condition stream)
             (format stream "Authentication failure for ~A [status ~A]: ~A"
                     (provider-error-provider condition)
                     (provider-error-status condition)
                     (claw-error-message condition)))))

(define-condition context-exceeded-error (provider-error)
  ()
  (:report (lambda (condition stream)
             (format stream "Context window exceeded for ~A [status ~A]: ~A"
                     (provider-error-provider condition)
                     (provider-error-status condition)
                     (claw-error-message condition)))))

;; --- Config Errors ---

(define-condition config-error (claw-error)
  ((path :initarg :path :reader config-error-path :initform nil))
  (:report (lambda (condition stream)
             (format stream "Configuration error~@[ in ~A~]: ~A"
                     (config-error-path condition)
                     (claw-error-message condition)))))

;; --- Tool Errors ---

(define-condition tool-error (claw-error)
  ((tool-name :initarg :tool-name :reader tool-error-tool-name))
  (:report (lambda (condition stream)
             (format stream "Tool ~A error: ~A"
                     (tool-error-tool-name condition)
                     (claw-error-message condition)))))

(define-condition permission-error (claw-error)
  ((tool-name :initarg :tool-name :reader permission-error-tool-name)
   (path :initarg :path :reader permission-error-path :initform nil))
  (:report (lambda (condition stream)
             (format stream "Permission denied for tool ~A~@[ on path ~A~]: ~A"
                     (permission-error-tool-name condition)
                     (permission-error-path condition)
                     (claw-error-message condition)))))

;; --- Storage Errors ---

(define-condition storage-error (claw-error)
  ()
  (:report (lambda (condition stream)
             (format stream "Storage error: ~A" (claw-error-message condition)))))

;; --- Compaction Errors ---

(define-condition compaction-error (claw-error)
  ()
  (:report (lambda (condition stream)
             (format stream "Compaction error: ~A" (claw-error-message condition)))))

;; --- Orchestration Errors ---

(define-condition orchestration-error (claw-error)
  ((session-id :initarg :session-id :reader orchestration-error-session-id :initform nil))
  (:report (lambda (condition stream)
             (format stream "Orchestration error~@[ in session ~A~]: ~A"
                     (orchestration-error-session-id condition)
                     (claw-error-message condition)))))

;; --- Supervisor / Child Agent Errors ---

(define-condition child-cancelled-error (orchestration-error)
  ((child-id :initarg :child-id :reader child-cancelled-error-child-id)
   (reason :initarg :reason :reader child-cancelled-error-reason :initform nil))
  (:report (lambda (condition stream)
             (format stream "Child agent ~A cancelled~@[: ~A~]"
                     (child-cancelled-error-child-id condition)
                     (child-cancelled-error-reason condition)))))

(define-condition child-timeout-error (orchestration-error)
  ((child-id :initarg :child-id :reader child-timeout-error-child-id)
   (timeout-seconds :initarg :timeout-seconds :reader child-timeout-error-timeout-seconds :initform nil))
  (:report (lambda (condition stream)
             (format stream "Child agent ~A timed out~@[ after ~As~]"
                     (child-timeout-error-child-id condition)
                     (child-timeout-error-timeout-seconds condition)))))

(define-condition child-supervisor-restart-limit-error (orchestration-error)
  ((child-id :initarg :child-id :reader child-supervisor-restart-limit-error-child-id)
   (restart-count :initarg :restart-count :reader child-supervisor-restart-limit-error-restart-count :initform 0)
   (max-restarts :initarg :max-restarts :reader child-supervisor-restart-limit-error-max-restarts :initform 0))
  (:report (lambda (condition stream)
             (format stream "Child agent ~A exceeded restart limit (~D/~D)"
                     (child-supervisor-restart-limit-error-child-id condition)
                     (child-supervisor-restart-limit-error-restart-count condition)
                     (child-supervisor-restart-limit-error-max-restarts condition)))))

;; --- Helper: classify HTTP status to condition type ---

(defun http-status->error-type (status)
  "Return the condition type appropriate for HTTP STATUS, or NIL for success.
   Returns a symbol suitable for use in handler-case."
  (cond
    ((< status 400) nil)
    ((= status 429) 'rate-limit-error)
    ((or (= status 401) (= status 403)) 'auth-error)
    ((= status 413) 'context-exceeded-error)
    (t 'provider-error)))

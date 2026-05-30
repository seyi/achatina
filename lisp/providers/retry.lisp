;;;; lisp/providers/retry.lisp
;;;;
;;;; Retry with exponential backoff and integrated rate-limit awareness.

(in-package #:claw-lisp.providers.retry)

;;; -------------------------------------------------------------------------
;;; Parameters
;;; -------------------------------------------------------------------------

(defparameter +default-max-retries+ 3
  "Maximum number of retry attempts for retryable errors.")

(defparameter +default-base-delay-seconds+ 1
  "Base delay in seconds for exponential backoff.")

(defparameter +default-max-delay-seconds+ 60
  "Maximum delay cap for exponential backoff.")

(defparameter +retryable-status-codes+ '(429 500 502 503 504 529)
  "HTTP status codes that warrant a retry.")

;;; -------------------------------------------------------------------------
;;; Backoff helpers
;;; -------------------------------------------------------------------------

(defun exponential-delay (attempt base-delay max-delay)
  "Return the delay in seconds for the given ATTEMPT (0-based)."
  (let* ((raw    (* base-delay (expt 2 attempt)))
         (jitter (* raw 0.1 (random 1.0))))
    (min max-delay (+ raw jitter))))

(defun retryable-status-p (status)
  "Return T if HTTP STATUS is a retryable error code."
  (member status +retryable-status-codes+ :test #'=))

;;; -------------------------------------------------------------------------
;;; Rate-limit integration helpers
;;; -------------------------------------------------------------------------

(defun rate-limit-wait-time (rl-state attempt base-delay max-delay)
  "Return the number of seconds to sleep before the next attempt."
  (if rl-state
      (let ((rl-wait (claw-lisp.providers.rate-limit:check-rate-limit rl-state)))
        (if (and rl-wait (> rl-wait 0))
            rl-wait
            (exponential-delay attempt base-delay max-delay)))
      (exponential-delay attempt base-delay max-delay)))

(defun maybe-warn-rate-limit (rl-state)
  "Emit a warning when RL-STATE indicates the rate limit is approaching."
  (when (and rl-state
             (claw-lisp.providers.rate-limit:rate-limit-warning-p rl-state))
    (warn "Approaching rate limit: ~A"
          (claw-lisp.providers.rate-limit:rate-limit-summary rl-state))))

;;; -------------------------------------------------------------------------
;;; Main retry loop
;;; -------------------------------------------------------------------------

(defun call-with-retry (fn &key (max-retries +default-max-retries+)
                             (base-delay +default-base-delay-seconds+)
                             (max-delay +default-max-delay-seconds+)
                             (on-retry nil)
                             (rate-limit-state nil))
  "Call FN with no arguments, retrying on retryable errors.

   FN must return two values: (status body).

   Retry behaviour:
   - 500 errors retry immediately (transient server fault; no sleep).
   - 429/502/503/504/529 use exponential backoff, but if RATE-LIMIT-STATE
     is provided and CHECK-RATE-LIMIT returns a positive wait time,
     that value is used instead of the backoff calculation.
   - Non-retryable statuses and errors propagate immediately.
   - When retries are exhausted, returns (values last-status last-body).

   Returns (values status body) from the last successful or final call."
  (loop
    with last-status = nil
    with last-body   = nil
    with last-error  = nil
    for attempt from 0 to max-retries
    do
       ;; Check rate-limit state before each attempt
       (when (and rate-limit-state (> attempt 0))
         (maybe-warn-rate-limit rate-limit-state))

       (handler-case
           (multiple-value-bind (status body) (funcall fn)
             (setf last-status status
                   last-body   body)
             (cond
               ;; Non-retryable: return immediately
               ((not (retryable-status-p status))
                (return (values status body)))

               ;; Retries exhausted
               ((= attempt max-retries)
                ;; Fall through to FINALLY
                )

               ;; 500: immediate retry, no sleep
               ((= status 500)
                (when on-retry
                  (funcall on-retry attempt status nil)))

               ;; Other retryable (429, 502...): sleep then retry
               (t
                (when on-retry
                  (funcall on-retry attempt status nil))
                (let ((wait (rate-limit-wait-time rate-limit-state
                                                  attempt
                                                  base-delay
                                                  max-delay)))
                  (when (> wait 0)
                    (sleep wait)))
                ;; Consume the retry-after value so it is not reused
                (when rate-limit-state
                  (claw-lisp.providers.rate-limit:clear-retry-after
                   rate-limit-state)))))

         (error (e)
           (setf last-error e)
           (when on-retry
             (funcall on-retry attempt last-status e))
           (when (< attempt max-retries)
             (let ((wait (rate-limit-wait-time rate-limit-state
                                               attempt
                                               base-delay
                                               max-delay)))
               (when (> wait 0)
                 (sleep wait)))
             (when rate-limit-state
               (claw-lisp.providers.rate-limit:clear-retry-after
                rate-limit-state)))))

    finally
       (if last-error
           (error last-error)
           (return (values last-status last-body)))))

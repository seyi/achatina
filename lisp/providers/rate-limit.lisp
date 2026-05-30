;;;; lisp/providers/rate-limit.lisp
;;;;
;;;; Rate-limit header tracking for Claw Lisp.
;;;;
;;;; Parses standard rate-limit headers (x-ratelimit-*, ratelimit-*, Retry-After),
;;;; maintains per-provider state, and provides advisory functions for callers
;;;; to decide whether to pause or warn before issuing the next request.

(in-package #:claw-lisp.providers.rate-limit)

;;; -------------------------------------------------------------------------
;;; Constants
;;; -------------------------------------------------------------------------

(defparameter +warning-threshold-fraction+ 0.10
  "Fraction of the total limit below which we emit a warning.
   E.g. 0.10 means warn when fewer than 10 % of requests remain.")

(defparameter +warning-threshold-absolute+ 10
  "Absolute floor for the warning threshold.
   Used when no limit header is present so we cannot compute a fraction.")

(defparameter +epoch-threshold+ 1000000000
  "Values above this are treated as Unix epoch timestamps; below are delta-seconds.
   Unix epoch 'now' is ~1.7 billion; deltas are typically < 86400 (1 day).")

(defparameter +max-sensible-retry-after+ 3600
  "Cap on Retry-After / reset-time values we will actually sleep for (seconds).
   Values larger than this are almost certainly malformed headers.")

(defparameter +unix-epoch-offset+ 2208988800
  "Seconds between CL universal-time epoch (1900-01-01) and Unix epoch (1970-01-01).
   Used to convert Unix timestamps to CL universal-time.")

;;; -------------------------------------------------------------------------
;;; State struct
;;; -------------------------------------------------------------------------

(defstruct (rate-limit-state
            (:constructor %make-rate-limit-state)
            (:copier nil))
  "Tracks rate-limit state parsed from HTTP response headers.

   REMAINING     — requests remaining in the current window (or NIL).
   LIMIT         — total requests allowed per window (or NIL).
   RESET-TIME    — universal-time at which the window resets (or NIL).
   RETRY-AFTER   — seconds to wait as specified by a Retry-After header (or NIL).
   LAST-UPDATED  — universal-time of the last update (or NIL).
   PROVIDER      — keyword identifying the provider, e.g. :anthropic or :bedrock."
  (remaining    nil :type (or null integer))
  (limit        nil :type (or null integer))
  (reset-time   nil :type (or null integer))
  (retry-after  nil :type (or null integer))
  (last-updated nil :type (or null integer))
  (provider     nil :type (or null keyword)))

(defun make-rate-limit-state (&key provider)
  "Create a fresh RATE-LIMIT-STATE for PROVIDER (a keyword, e.g. :anthropic)."
  (%make-rate-limit-state :provider provider))

;;; -------------------------------------------------------------------------
;;; Thread-safety stub
;;; -------------------------------------------------------------------------

(defmacro with-state-lock (&body body)
  "Execute BODY with the state lock held.
   Currently a no-op; replace with a real lock when threading is added."
  `(progn ,@body))

;;; -------------------------------------------------------------------------
;;; Header parsing helpers
;;; -------------------------------------------------------------------------

(defun parse-header-integer (value)
  "Parse VALUE (a string or NIL) as a non-negative integer."
  (when (and value (stringp value))
    (handler-case
        (let ((n (parse-integer (string-trim '(#\Space #\Tab) value))))
          (when (>= n 0) n))
      (error () nil))))

(defun header-value (headers &rest names)
  "Return the first non-NIL value found in HEADERS for any of NAMES."
  (when headers
    (loop for name in names
          for value = (gethash name headers)
          when value return value)))

(defun parse-reset-header (raw-value)
  "Convert a raw x-ratelimit-reset header value to a CL universal-time.

   Anthropic sends the number of *seconds until* the window resets (a delta).
   Some providers send a Unix epoch timestamp instead.

   Heuristic: if the value is >= +epoch-threshold+ we treat it as an epoch
   timestamp and convert to universal-time; otherwise it's a delta and we
   add it to the current time.

   Returns NIL on parse failure."
  (let ((n (parse-header-integer raw-value)))
    (when n
      (if (>= n +epoch-threshold+)
          ;; Epoch timestamp: convert Unix epoch → CL universal-time
          (+ n +unix-epoch-offset+)
          ;; Delta: seconds from now
          (+ (get-universal-time) n)))))

;;; -------------------------------------------------------------------------
;;; Core update function
;;; -------------------------------------------------------------------------

(defun update-rate-limit-state (state headers)
  "Destructively update STATE from HTTP response HEADERS.

   HEADERS should be a hash-table with lower-case string keys (dexador format).
   Returns STATE so callers can chain calls.
   
   Note: last-updated is only stamped when headers are non-NIL and at least
   one field was successfully parsed. This allows callers to distinguish
   'no update attempted' from 'update attempted but no headers present'."
  (with-state-lock
    (when headers
      (let ((updated-p nil))
        ;; remaining
        (let ((raw (header-value headers "x-ratelimit-remaining" "ratelimit-remaining")))
          (let ((n (parse-header-integer raw)))
            (when n
              (setf (rate-limit-state-remaining state) n)
              (setf updated-p t))))

        ;; limit
        (let ((raw (header-value headers "x-ratelimit-limit" "ratelimit-limit")))
          (let ((n (parse-header-integer raw)))
            (when n
              (setf (rate-limit-state-limit state) n)
              (setf updated-p t))))

        ;; reset-time
        (let ((raw (header-value headers "x-ratelimit-reset" "ratelimit-reset")))
          (let ((t* (parse-reset-header raw)))
            (when t*
              (setf (rate-limit-state-reset-time state) t*)
              (setf updated-p t))))

        ;; retry-after
        (let ((raw (header-value headers "retry-after")))
          (let ((n (parse-header-integer raw)))
            (when n
              (setf (rate-limit-state-retry-after state)
                    (min n +max-sensible-retry-after+))
              (setf updated-p t))))

        ;; Stamp the update time only if we actually parsed something
        (when updated-p
          (setf (rate-limit-state-last-updated state) (get-universal-time))))))
  state)

;;; -------------------------------------------------------------------------
;;; Advisory predicates and queries
;;; -------------------------------------------------------------------------

(defun rate-limit-exhausted-p (state)
  "Return T when the remaining-requests counter is known and has reached zero."
  (let ((r (rate-limit-state-remaining state)))
    (and r (<= r 0))))

(defun rate-limit-warning-p (state)
  "Return T when the rate limit is approaching exhaustion."
  (let ((remaining (rate-limit-state-remaining state))
        (limit     (rate-limit-state-limit     state)))
    (when remaining
      (let ((threshold
             (if limit
                 (max +warning-threshold-absolute+
                      (floor (* limit +warning-threshold-fraction+)))
                 +warning-threshold-absolute+)))
        (<= remaining threshold)))))

(defun seconds-until-reset (state)
  "Return the number of seconds until the rate-limit window resets.
   
   Note: This function acquires the state lock to ensure thread-safe reads
   of the reset-time field. When concurrency is added, this will prevent
   reading a partially-updated value."
  (with-state-lock
    (let ((reset (rate-limit-state-reset-time state)))
      (when reset
        (max 0 (- reset (get-universal-time)))))))

(defun check-rate-limit (state)
  "Return the number of seconds the caller should sleep before the next request.
   Returns NIL when no pause is needed."
  (with-state-lock
    ;; Priority 1: explicit Retry-After directive
    (let ((ra (rate-limit-state-retry-after state)))
      (when (and ra (> ra 0))
        (return-from check-rate-limit ra)))

    ;; Priority 2: exhausted window with a known reset time
    (when (rate-limit-exhausted-p state)
      (let ((secs (seconds-until-reset state)))
        (when (and secs (> secs 0))
          (return-from check-rate-limit secs))))

    nil))

(defun clear-retry-after (state)
  "Clear the RETRY-AFTER field after it has been consumed."
  (with-state-lock
    (setf (rate-limit-state-retry-after state) nil))
  state)

(defun rate-limit-summary (state)
  "Return a human-readable summary string of STATE for logging."
  (format nil "provider=~A remaining=~A/~A reset-in=~As retry-after=~As"
          (or (rate-limit-state-provider state) "unknown")
          (or (rate-limit-state-remaining state) "?")
          (or (rate-limit-state-limit state) "?")
          (or (seconds-until-reset state) "?")
          (or (rate-limit-state-retry-after state) "none")))

;;;; lisp/tests/rate-limit.lisp
;;;;
;;;; Test suite for rate-limit header tracking and retry integration.
;;;; All tests are self-contained — no network calls, only mock data.

(in-package #:claw-lisp.tests)

;;; Note: %assert function is defined in runtime.lisp (line 3)
;;; It signals an error when condition fails.

;;; -------------------------------------------------------------------------
;;; Test helpers
;;; -------------------------------------------------------------------------

(defun make-mock-headers (&rest pairs)
  "Create a hash-table from alternating key/value PAIRS.
   Keys should be lower-case strings matching HTTP header conventions."
  (let ((ht (make-hash-table :test 'equal)))
    (loop for (k v) on pairs by #'cddr
          do (setf (gethash k ht) v))
    ht))

;;; -------------------------------------------------------------------------
;;; parse-header-integer tests
;;; -------------------------------------------------------------------------

(defun test-parse-header-integer-valid ()
  "parse-header-integer returns an integer for valid non-negative strings."
  (let ((result (claw-lisp.providers.rate-limit::parse-header-integer "42")))
    (%assert (eql 42 result)
             "Expected 42, got ~A" result))
  (let ((result (claw-lisp.providers.rate-limit::parse-header-integer "0")))
    (%assert (eql 0 result)
             "Expected 0, got ~A" result))
  ;; With surrounding whitespace
  (let ((result (claw-lisp.providers.rate-limit::parse-header-integer "  100  ")))
    (%assert (eql 100 result)
             "Expected 100 with whitespace, got ~A" result)))

(defun test-parse-header-integer-invalid ()
  "parse-header-integer returns NIL for invalid or negative strings."
  (%assert (null (claw-lisp.providers.rate-limit::parse-header-integer nil))
           "Expected NIL for NIL input")
  (%assert (null (claw-lisp.providers.rate-limit::parse-header-integer ""))
           "Expected NIL for empty string")
  (%assert (null (claw-lisp.providers.rate-limit::parse-header-integer "abc"))
           "Expected NIL for non-numeric string")
  (%assert (null (claw-lisp.providers.rate-limit::parse-header-integer "-5"))
           "Expected NIL for negative integer")
  (%assert (null (claw-lisp.providers.rate-limit::parse-header-integer "12.5"))
           "Expected NIL for float string"))

;;; -------------------------------------------------------------------------
;;; parse-reset-header tests
;;; -------------------------------------------------------------------------

(defun test-parse-reset-header-delta ()
  "parse-reset-header treats small values as delta-seconds from now."
  (let ((result (claw-lisp.providers.rate-limit::parse-reset-header "30")))
    (%assert (not (null result))
             "Expected non-NIL for delta value '30'")
    ;; Measure 'now' after the call to avoid timing failures
    (let ((now (get-universal-time)))
      ;; Allow 5s tolerance for test execution overhead
      (let ((diff (abs (- result (+ now 30)))))
        (%assert (<= diff 5)
                 "Delta reset time off by ~A seconds (expected <5s tolerance)"
                 diff)))))

(defun test-parse-reset-header-epoch ()
  "parse-reset-header treats large values as Unix epoch timestamps."
  ;; Use a known Unix timestamp: 1700000000 = 2023-11-14T22:13:20Z
  ;; CL universal-time = Unix + 2208988800
  (let ((result (claw-lisp.providers.rate-limit::parse-reset-header "1700000000")))
    (%assert (not (null result))
             "Expected non-NIL for epoch value")
    (let ((expected (+ 1700000000 2208988800)))
      (%assert (eql expected result)
               "Expected ~A for epoch conversion, got ~A" expected result))))

(defun test-parse-reset-header-nil ()
  "parse-reset-header returns NIL for NIL or invalid input."
  (%assert (null (claw-lisp.providers.rate-limit::parse-reset-header nil))
           "Expected NIL for NIL input")
  (%assert (null (claw-lisp.providers.rate-limit::parse-reset-header "not-a-number"))
           "Expected NIL for non-numeric input"))

;;; -------------------------------------------------------------------------
;;; header-value tests
;;; -------------------------------------------------------------------------

(defun test-header-value-found ()
  "header-value returns the first matching value from the hash-table."
  (let ((headers (make-mock-headers "x-ratelimit-remaining" "50"
                                    "ratelimit-remaining" "40")))
    (let ((val (claw-lisp.providers.rate-limit::header-value
                headers "x-ratelimit-remaining" "ratelimit-remaining")))
      (%assert (string= "50" val)
               "Expected '50' (first match), got ~A" val))))

(defun test-header-value-fallback ()
  "header-value falls back to the second name when the first is absent."
  (let ((headers (make-mock-headers "ratelimit-remaining" "40")))
    (let ((val (claw-lisp.providers.rate-limit::header-value
                headers "x-ratelimit-remaining" "ratelimit-remaining")))
      (%assert (string= "40" val)
               "Expected '40' (fallback), got ~A" val))))

(defun test-header-value-missing ()
  "header-value returns NIL when no matching key exists."
  (let ((headers (make-mock-headers "content-type" "application/json")))
    (let ((val (claw-lisp.providers.rate-limit::header-value
                headers "x-ratelimit-remaining" "ratelimit-remaining")))
      (%assert (null val)
               "Expected NIL for missing headers, got ~A" val))))

(defun test-header-value-nil-headers ()
  "header-value returns NIL when headers is NIL."
  (let ((val (claw-lisp.providers.rate-limit::header-value
              nil "x-ratelimit-remaining")))
    (%assert (null val)
             "Expected NIL for NIL headers, got ~A" val)))

;;; -------------------------------------------------------------------------
;;; update-rate-limit-state tests
;;; -------------------------------------------------------------------------

(defun test-update-state-complete-headers ()
  "update-rate-limit-state correctly parses a full set of rate-limit headers."
  (let ((state (claw-lisp.providers.rate-limit:make-rate-limit-state
                :provider :anthropic))
        (headers (make-mock-headers
                  "x-ratelimit-remaining" "95"
                  "x-ratelimit-limit" "1000"
                  "x-ratelimit-reset" "60"
                  "retry-after" "5")))
    (claw-lisp.providers.rate-limit:update-rate-limit-state state headers)
    (%assert (eql 95 (claw-lisp.providers.rate-limit::rate-limit-state-remaining state))
             "Expected remaining=95, got ~A"
             (claw-lisp.providers.rate-limit::rate-limit-state-remaining state))
    (%assert (eql 1000 (claw-lisp.providers.rate-limit::rate-limit-state-limit state))
             "Expected limit=1000, got ~A"
             (claw-lisp.providers.rate-limit::rate-limit-state-limit state))
    (%assert (not (null (claw-lisp.providers.rate-limit::rate-limit-state-reset-time state)))
             "Expected non-NIL reset-time")
    (%assert (eql 5 (claw-lisp.providers.rate-limit::rate-limit-state-retry-after state))
             "Expected retry-after=5, got ~A"
             (claw-lisp.providers.rate-limit::rate-limit-state-retry-after state))
    (%assert (not (null (claw-lisp.providers.rate-limit::rate-limit-state-last-updated state)))
             "Expected non-NIL last-updated")))

(defun test-update-state-partial-headers ()
  "update-rate-limit-state handles partial headers, leaving other fields NIL."
  (let ((state (claw-lisp.providers.rate-limit:make-rate-limit-state
                :provider :bedrock))
        (headers (make-mock-headers "x-ratelimit-remaining" "10")))
    (claw-lisp.providers.rate-limit:update-rate-limit-state state headers)
    (%assert (eql 10 (claw-lisp.providers.rate-limit::rate-limit-state-remaining state))
             "Expected remaining=10, got ~A"
             (claw-lisp.providers.rate-limit::rate-limit-state-remaining state))
    (%assert (null (claw-lisp.providers.rate-limit::rate-limit-state-limit state))
             "Expected limit=NIL for missing header")
    (%assert (null (claw-lisp.providers.rate-limit::rate-limit-state-retry-after state))
             "Expected retry-after=NIL for missing header")))

(defun test-update-state-malformed-headers ()
  "update-rate-limit-state gracefully ignores malformed header values."
  (let ((state (claw-lisp.providers.rate-limit:make-rate-limit-state
                :provider :anthropic))
        (headers (make-mock-headers
                  "x-ratelimit-remaining" "not-a-number"
                  "x-ratelimit-limit" ""
                  "retry-after" "abc")))
    (claw-lisp.providers.rate-limit:update-rate-limit-state state headers)
    (%assert (null (claw-lisp.providers.rate-limit::rate-limit-state-remaining state))
             "Expected remaining=NIL for malformed value")
    (%assert (null (claw-lisp.providers.rate-limit::rate-limit-state-limit state))
             "Expected limit=NIL for empty value")
    (%assert (null (claw-lisp.providers.rate-limit::rate-limit-state-retry-after state))
             "Expected retry-after=NIL for malformed value")))

(defun test-update-state-retry-after-capped ()
  "update-rate-limit-state caps retry-after at +max-sensible-retry-after+."
  (let ((state (claw-lisp.providers.rate-limit:make-rate-limit-state
                :provider :anthropic))
        (headers (make-mock-headers "retry-after" "999999")))
    (claw-lisp.providers.rate-limit:update-rate-limit-state state headers)
    (%assert (eql claw-lisp.providers.rate-limit::+max-sensible-retry-after+
                  (claw-lisp.providers.rate-limit::rate-limit-state-retry-after state))
             "Expected retry-after capped at ~A, got ~A"
             claw-lisp.providers.rate-limit::+max-sensible-retry-after+
             (claw-lisp.providers.rate-limit::rate-limit-state-retry-after state))))

;;; -------------------------------------------------------------------------
;;; Predicate tests
;;; -------------------------------------------------------------------------

(defun test-rate-limit-exhausted-p ()
  "rate-limit-exhausted-p returns T when remaining is zero or negative."
  (let ((state (claw-lisp.providers.rate-limit:make-rate-limit-state
                :provider :anthropic)))
    ;; NIL remaining → not exhausted
    (%assert (not (claw-lisp.providers.rate-limit:rate-limit-exhausted-p state))
             "Expected not exhausted when remaining is NIL")
    ;; remaining = 5 → not exhausted
    (setf (claw-lisp.providers.rate-limit::rate-limit-state-remaining state) 5)
    (%assert (not (claw-lisp.providers.rate-limit:rate-limit-exhausted-p state))
             "Expected not exhausted when remaining=5")
    ;; remaining = 0 → exhausted
    (setf (claw-lisp.providers.rate-limit::rate-limit-state-remaining state) 0)
    (%assert (claw-lisp.providers.rate-limit:rate-limit-exhausted-p state)
             "Expected exhausted when remaining=0")))

(defun test-rate-limit-warning-p ()
  "rate-limit-warning-p returns T when remaining is below the threshold."
  (let ((state (claw-lisp.providers.rate-limit:make-rate-limit-state
                :provider :anthropic)))
    ;; NIL remaining → no warning
    (%assert (not (claw-lisp.providers.rate-limit:rate-limit-warning-p state))
             "Expected no warning when remaining is NIL")
    ;; remaining=5, no limit → threshold is +warning-threshold-absolute+ (10)
    (setf (claw-lisp.providers.rate-limit::rate-limit-state-remaining state) 5)
    (%assert (claw-lisp.providers.rate-limit:rate-limit-warning-p state)
             "Expected warning when remaining=5 < absolute threshold 10")
    ;; remaining=15, no limit → above absolute threshold
    (setf (claw-lisp.providers.rate-limit::rate-limit-state-remaining state) 15)
    (%assert (not (claw-lisp.providers.rate-limit:rate-limit-warning-p state))
             "Expected no warning when remaining=15 > absolute threshold 10")
    ;; remaining=90, limit=1000 → threshold = max(10, 100) = 100
    (setf (claw-lisp.providers.rate-limit::rate-limit-state-remaining state) 90)
    (setf (claw-lisp.providers.rate-limit::rate-limit-state-limit state) 1000)
    (%assert (claw-lisp.providers.rate-limit:rate-limit-warning-p state)
             "Expected warning when remaining=90 < 10% of 1000 (100)")
    ;; remaining=150, limit=1000 → above threshold
    (setf (claw-lisp.providers.rate-limit::rate-limit-state-remaining state) 150)
    (%assert (not (claw-lisp.providers.rate-limit:rate-limit-warning-p state))
             "Expected no warning when remaining=150 > 100")))

(defun test-seconds-until-reset ()
  "seconds-until-reset returns correct values for past and future reset times."
  (let ((state (claw-lisp.providers.rate-limit:make-rate-limit-state
                :provider :anthropic)))
    ;; NIL reset-time → NIL
    (%assert (null (claw-lisp.providers.rate-limit:seconds-until-reset state))
             "Expected NIL when reset-time is NIL")
    ;; Future reset: 60 seconds from now
    (let ((now (get-universal-time)))
      (setf (claw-lisp.providers.rate-limit::rate-limit-state-reset-time state)
            (+ now 60))
      (let ((secs (claw-lisp.providers.rate-limit:seconds-until-reset state)))
        (%assert (not (null secs))
                 "Expected non-NIL for future reset")
        ;; Allow wider tolerance for test execution
        (%assert (and (>= secs 55) (<= secs 65))
                 "Expected ~60 seconds until reset, got ~A" secs)))
    ;; Past reset: 10 seconds ago → should return 0
    (let ((now (get-universal-time)))
      (setf (claw-lisp.providers.rate-limit::rate-limit-state-reset-time state)
            (- now 10))
      (let ((secs (claw-lisp.providers.rate-limit:seconds-until-reset state)))
        (%assert (eql 0 secs)
                 "Expected 0 for past reset time, got ~A" secs)))))

;;; -------------------------------------------------------------------------
;;; check-rate-limit tests
;;; -------------------------------------------------------------------------

(defun test-check-rate-limit-retry-after ()
  "check-rate-limit returns retry-after value when set (highest priority)."
  (let ((state (claw-lisp.providers.rate-limit:make-rate-limit-state
                :provider :anthropic)))
    (setf (claw-lisp.providers.rate-limit::rate-limit-state-retry-after state) 30)
    (let ((wait (claw-lisp.providers.rate-limit:check-rate-limit state)))
      (%assert (eql 30 wait)
               "Expected check-rate-limit to return 30, got ~A" wait))))

(defun test-check-rate-limit-exhausted-with-reset ()
  "check-rate-limit returns seconds-until-reset when exhausted."
  (let ((state (claw-lisp.providers.rate-limit:make-rate-limit-state
                :provider :anthropic)))
    (setf (claw-lisp.providers.rate-limit::rate-limit-state-remaining state) 0)
    ;; Measure 'now' after setting state to avoid timing issues
    (let ((now (get-universal-time)))
      (setf (claw-lisp.providers.rate-limit::rate-limit-state-reset-time state)
            (+ now 45))
      (let ((wait (claw-lisp.providers.rate-limit:check-rate-limit state)))
        (%assert (not (null wait))
                 "Expected non-NIL wait time when exhausted with reset")
        ;; Allow wider tolerance for test execution
        (%assert (and (>= wait 40) (<= wait 50))
                 "Expected ~45 seconds wait, got ~A" wait)))))

(defun test-check-rate-limit-no-wait-needed ()
  "check-rate-limit returns NIL when no pause is needed."
  (let ((state (claw-lisp.providers.rate-limit:make-rate-limit-state
                :provider :anthropic)))
    ;; Fresh state — nothing set
    (%assert (null (claw-lisp.providers.rate-limit:check-rate-limit state))
             "Expected NIL for fresh state")
    ;; Remaining > 0, no retry-after
    (setf (claw-lisp.providers.rate-limit::rate-limit-state-remaining state) 50)
    (%assert (null (claw-lisp.providers.rate-limit:check-rate-limit state))
             "Expected NIL when remaining=50")))

(defun test-check-rate-limit-retry-after-priority ()
  "check-rate-limit prefers retry-after over exhausted+reset."
  (let ((state (claw-lisp.providers.rate-limit:make-rate-limit-state
                :provider :anthropic))
        (now (get-universal-time)))
    ;; Both retry-after and exhausted+reset are set
    (setf (claw-lisp.providers.rate-limit::rate-limit-state-retry-after state) 10)
    (setf (claw-lisp.providers.rate-limit::rate-limit-state-remaining state) 0)
    (setf (claw-lisp.providers.rate-limit::rate-limit-state-reset-time state)
          (+ now 120))
    (let ((wait (claw-lisp.providers.rate-limit:check-rate-limit state)))
      (%assert (eql 10 wait)
               "Expected retry-after (10) to take priority, got ~A" wait))))

;;; -------------------------------------------------------------------------
;;; clear-retry-after tests
;;; -------------------------------------------------------------------------

(defun test-clear-retry-after ()
  "clear-retry-after sets retry-after to NIL and returns state."
  (let ((state (claw-lisp.providers.rate-limit:make-rate-limit-state
                :provider :anthropic)))
    (setf (claw-lisp.providers.rate-limit::rate-limit-state-retry-after state) 30)
    (let ((result (claw-lisp.providers.rate-limit:clear-retry-after state)))
      (%assert (eq state result)
               "Expected clear-retry-after to return the same state object")
      (%assert (null (claw-lisp.providers.rate-limit::rate-limit-state-retry-after state))
               "Expected retry-after to be NIL after clearing"))))

(defun test-clear-retry-after-prevents-repeated-waits ()
  "clear-retry-after ensures check-rate-limit returns NIL after clearing."
  (let ((state (claw-lisp.providers.rate-limit:make-rate-limit-state
                :provider :anthropic)))
    ;; Set retry-after
    (setf (claw-lisp.providers.rate-limit::rate-limit-state-retry-after state) 30)
    ;; First check should return 30
    (%assert (eql 30 (claw-lisp.providers.rate-limit:check-rate-limit state))
             "Expected check-rate-limit to return 30 before clearing")
    ;; Clear it
    (claw-lisp.providers.rate-limit:clear-retry-after state)
    ;; Second check should return NIL (no wait needed)
    (%assert (null (claw-lisp.providers.rate-limit:check-rate-limit state))
             "Expected check-rate-limit to return NIL after clearing")))

;;; -------------------------------------------------------------------------
;;; rate-limit-summary tests
;;; -------------------------------------------------------------------------

(defun test-rate-limit-summary ()
  "rate-limit-summary returns a human-readable string."
  (let ((state (claw-lisp.providers.rate-limit:make-rate-limit-state
                :provider :anthropic)))
    (setf (claw-lisp.providers.rate-limit::rate-limit-state-remaining state) 42)
    (setf (claw-lisp.providers.rate-limit::rate-limit-state-limit state) 1000)
    (let ((summary (claw-lisp.providers.rate-limit:rate-limit-summary state)))
      (%assert (search "ANTHROPIC" (string-upcase summary))
               "Expected provider name in summary: ~A" summary)
      (%assert (search "42" summary)
               "Expected remaining count in summary: ~A" summary)
      (%assert (search "1000" summary)
               "Expected limit count in summary: ~A" summary))))

(defun test-rate-limit-summary-fresh-state ()
  "rate-limit-summary handles a fresh state with all NIL fields."
  (let ((state (claw-lisp.providers.rate-limit:make-rate-limit-state)))
    (let ((summary (claw-lisp.providers.rate-limit:rate-limit-summary state)))
      (%assert (stringp summary)
               "Expected a string, got ~A" (type-of summary))
      (%assert (search "unknown" summary)
               "Expected 'unknown' provider in summary: ~A" summary))))

;;; -------------------------------------------------------------------------
;;; Integration: retry with rate-limit state (mock)
;;; -------------------------------------------------------------------------

(defun test-retry-with-rate-limit-state ()
  "call-with-retry uses rate-limit state for wait-time decisions."
  (let ((state (claw-lisp.providers.rate-limit:make-rate-limit-state
                :provider :anthropic))
        (call-count 0)
        (retries-seen nil))
    ;; Simulate: first call returns 429, second returns 200
    (let ((result-status
            (claw-lisp.providers.retry:call-with-retry
             (lambda ()
               (incf call-count)
               (if (= call-count 1)
                   (values 429 "rate limited")
                   (values 200 "ok")))
             :max-retries 3
             :base-delay 0    ; no actual sleeping in tests
             :max-delay 0
             :on-retry (lambda (attempt status err)
                         (declare (ignore err))
                         (push (list attempt status) retries-seen))
             :rate-limit-state state)))
      (%assert (eql 200 result-status)
               "Expected final status 200, got ~A" result-status)
      (%assert (= 2 call-count)
               "Expected 2 calls (1 retry), got ~A" call-count)
      (%assert (= 1 (length retries-seen))
               "Expected 1 retry callback, got ~A" (length retries-seen)))))

(defun test-retry-exhausted-with-rate-limit ()
  "call-with-retry exhausts retries and returns last status."
  (let ((state (claw-lisp.providers.rate-limit:make-rate-limit-state
                :provider :anthropic))
        (call-count 0))
    ;; Always return 503
    (let ((result-status
            (claw-lisp.providers.retry:call-with-retry
             (lambda ()
               (incf call-count)
               (values 503 "service unavailable"))
             :max-retries 2
             :base-delay 0
             :max-delay 0
             :rate-limit-state state)))
      (%assert (eql 503 result-status)
               "Expected final status 503, got ~A" result-status)
      ;; 1 initial + 2 retries = 3 calls
      (%assert (= 3 call-count)
               "Expected 3 calls (initial + 2 retries), got ~A" call-count))))

(defun test-retry-non-retryable-returns-immediately ()
  "call-with-retry returns immediately for non-retryable status codes."
  (let ((call-count 0))
    (multiple-value-bind (status body)
        (claw-lisp.providers.retry:call-with-retry
         (lambda ()
           (incf call-count)
           (values 400 "bad request"))
         :max-retries 3
         :base-delay 0
         :max-delay 0)
      (%assert (eql 400 status)
               "Expected status 400, got ~A" status)
      (%assert (string= "bad request" body)
               "Expected body 'bad request', got ~A" body)
      (%assert (= 1 call-count)
               "Expected exactly 1 call for non-retryable status, got ~A"
               call-count))))

(defun test-retry-success-returns-immediately ()
  "call-with-retry returns immediately on success."
  (let ((call-count 0))
    (multiple-value-bind (status body)
        (claw-lisp.providers.retry:call-with-retry
         (lambda ()
           (incf call-count)
           (values 200 "success"))
         :max-retries 3
         :base-delay 0
         :max-delay 0)
      (%assert (eql 200 status)
               "Expected status 200, got ~A" status)
      (%assert (string= "success" body)
               "Expected body 'success', got ~A" body)
      (%assert (= 1 call-count)
               "Expected exactly 1 call for success, got ~A" call-count))))

;;; -------------------------------------------------------------------------
;;; Backoff helper tests
;;; -------------------------------------------------------------------------

(defun test-exponential-delay-bounds ()
  "exponential-delay respects the max-delay cap."
  ;; With base=1, attempt=10, raw = 1024 which exceeds max-delay=60
  (let ((delay (claw-lisp.providers.retry::exponential-delay 10 1 60)))
    (%assert (<= delay 60)
             "Expected delay <= 60, got ~A" delay))
  ;; Attempt 0 should be close to base-delay
  (let ((delay (claw-lisp.providers.retry::exponential-delay 0 1 60)))
    (%assert (and (>= delay 1.0) (<= delay 1.2))
             "Expected delay ~1.0-1.1 for attempt 0, got ~A" delay)))

(defun test-retryable-status-p ()
  "retryable-status-p correctly identifies retryable HTTP status codes."
  ;; Retryable codes
  (%assert (claw-lisp.providers.retry:retryable-status-p 429)
           "429 should be retryable")
  (%assert (claw-lisp.providers.retry:retryable-status-p 500)
           "500 should be retryable")
  (%assert (claw-lisp.providers.retry:retryable-status-p 502)
           "502 should be retryable")
  (%assert (claw-lisp.providers.retry:retryable-status-p 503)
           "503 should be retryable")
  (%assert (claw-lisp.providers.retry:retryable-status-p 504)
           "504 should be retryable")
  (%assert (claw-lisp.providers.retry:retryable-status-p 529)
           "529 should be retryable")
  ;; Non-retryable codes
  (%assert (not (claw-lisp.providers.retry:retryable-status-p 200))
           "200 should not be retryable")
  (%assert (not (claw-lisp.providers.retry:retryable-status-p 400))
           "400 should not be retryable")
  (%assert (not (claw-lisp.providers.retry:retryable-status-p 401))
           "401 should not be retryable")
  (%assert (not (claw-lisp.providers.retry:retryable-status-p 404))
           "404 should not be retryable"))

(defun run-rate-limit-tests ()
  "Run all rate-limit tests and return T if all pass."
  (format t "~&=== Rate-Limit Test Suite ===~%")
  (flet ((run-test (name thunk)
           (handler-case
               (progn
                 (funcall thunk)
                 t)
             (error (condition)
               (format t "~&RATE-LIMIT TEST FAILED: ~A~%  ~A~%" name condition)
               nil))))
    (let ((results (list
                    ;; Header parsing tests
                    (run-test 'test-parse-header-integer-valid #'test-parse-header-integer-valid)
                    (run-test 'test-parse-header-integer-invalid #'test-parse-header-integer-invalid)
                    (run-test 'test-parse-reset-header-delta #'test-parse-reset-header-delta)
                    (run-test 'test-parse-reset-header-epoch #'test-parse-reset-header-epoch)
                    (run-test 'test-parse-reset-header-nil #'test-parse-reset-header-nil)
                    ;; Header extraction tests
                    (run-test 'test-header-value-found #'test-header-value-found)
                    (run-test 'test-header-value-fallback #'test-header-value-fallback)
                    (run-test 'test-header-value-missing #'test-header-value-missing)
                    (run-test 'test-header-value-nil-headers #'test-header-value-nil-headers)
                    ;; State update tests
                    (run-test 'test-update-state-complete-headers #'test-update-state-complete-headers)
                    (run-test 'test-update-state-partial-headers #'test-update-state-partial-headers)
                    (run-test 'test-update-state-malformed-headers #'test-update-state-malformed-headers)
                    (run-test 'test-update-state-retry-after-capped #'test-update-state-retry-after-capped)
                    ;; Predicate tests
                    (run-test 'test-rate-limit-exhausted-p #'test-rate-limit-exhausted-p)
                    (run-test 'test-rate-limit-warning-p #'test-rate-limit-warning-p)
                    (run-test 'test-seconds-until-reset #'test-seconds-until-reset)
                    ;; Wait time calculation tests
                    (run-test 'test-check-rate-limit-retry-after #'test-check-rate-limit-retry-after)
                    (run-test 'test-check-rate-limit-exhausted-with-reset #'test-check-rate-limit-exhausted-with-reset)
                    (run-test 'test-check-rate-limit-no-wait-needed #'test-check-rate-limit-no-wait-needed)
                    (run-test 'test-check-rate-limit-retry-after-priority #'test-check-rate-limit-retry-after-priority)
                    ;; Utility tests
                    (run-test 'test-clear-retry-after #'test-clear-retry-after)
                    (run-test 'test-clear-retry-after-prevents-repeated-waits #'test-clear-retry-after-prevents-repeated-waits)
                    (run-test 'test-rate-limit-summary #'test-rate-limit-summary)
                    (run-test 'test-rate-limit-summary-fresh-state #'test-rate-limit-summary-fresh-state)
                    ;; Integration tests
                    (run-test 'test-retry-with-rate-limit-state #'test-retry-with-rate-limit-state)
                    (run-test 'test-retry-exhausted-with-rate-limit #'test-retry-exhausted-with-rate-limit)
                    (run-test 'test-retry-non-retryable-returns-immediately #'test-retry-non-retryable-returns-immediately)
                    (run-test 'test-retry-success-returns-immediately #'test-retry-success-returns-immediately)
                    ;; Backoff helper tests
                    (run-test 'test-exponential-delay-bounds #'test-exponential-delay-bounds)
                    (run-test 'test-retryable-status-p #'test-retryable-status-p))))
      (if (every #'identity results)
        (progn
          (format t "~&ALL RATE-LIMIT TESTS PASSED~%")
          t)
        (progn
          (format t "~&SOME RATE-LIMIT TESTS FAILED~%")
          nil)))))

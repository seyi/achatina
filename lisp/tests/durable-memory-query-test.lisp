;;;; lisp/tests/durable-memory-query-test.lisp
;;;;
;;;; Phase 7 Task 6 — Unit Tests for Configuration & Query Utilities
;;;;
;;;; Run with: (claw-lisp.tests:run-durable-memory-query-tests)

(in-package #:claw-lisp.tests)

;;; ============================================================
;;; Local Test Helper
;;; ============================================================

(defun %assert (condition format-control &rest format-args)
  "Assert that CONDITION is true. Signals an error with FORMAT-ARGS on failure."
  (unless condition
    (error (apply #'format nil format-control format-args))))

;;; ============================================================
;;; Configuration Struct Tests
;;; ============================================================

(defun test-dmq-config-defaults ()
  "Test that durable-memory-query-config has correct default values."
  (let ((config (claw-lisp.config:make-default-dmq-config)))
    ;; Query parameters
    (%assert (= (claw-lisp.config:dmq-config-max-results config) 5)
             "max-results should default to 5, got ~A"
             (claw-lisp.config:dmq-config-max-results config))
    (%assert (= (claw-lisp.config:dmq-config-min-relevance-score config) 0.3s0)
             "min-relevance-score should default to 0.3, got ~A"
             (claw-lisp.config:dmq-config-min-relevance-score config))
    (%assert (eq (claw-lisp.config:dmq-config-default-query-mode config) :hybrid)
             "default-query-mode should be :hybrid, got ~A"
             (claw-lisp.config:dmq-config-default-query-mode config))
    
    ;; Score blending
    (%assert (assoc :user (claw-lisp.config:dmq-config-semantic-weight-by-kind config))
             "semantic-weight-by-kind should have :user entry")
    (%assert (= (cdr (assoc :user (claw-lisp.config:dmq-config-semantic-weight-by-kind config))) 0.8s0)
             ":user weight should be 0.8")
    (%assert (= (cdr (assoc :project (claw-lisp.config:dmq-config-semantic-weight-by-kind config))) 0.65s0)
             ":project weight should be 0.65")
    
    ;; Context injection
    (%assert (= (claw-lisp.config:dmq-config-max-injection-chars config) 1000)
             "max-injection-chars should default to 1000")
    (%assert (eq (claw-lisp.config:dmq-config-injection-enabled config) t)
             "injection-enabled should default to T")
    ;; Dedup
    (%assert (= (claw-lisp.config:dmq-config-dedup-window-normal config) 5)
             "dedup-window-normal should default to 5")
    (%assert (= (claw-lisp.config:dmq-config-dedup-window-important config) 20)
             "dedup-window-important should default to 20")
    (%assert (= (claw-lisp.config:dmq-config-importance-threshold config) 0.85s0)
             "importance-threshold should default to 0.85")
    (%assert (member :project (claw-lisp.config:dmq-config-evergreen-kinds config))
             "evergreen-kinds should include :project")
    
    ;; Circuit breaker
    (%assert (= (claw-lisp.config:dmq-config-embedding-failure-threshold config) 3)
             "embedding-failure-threshold should default to 3")
    (%assert (= (claw-lisp.config:dmq-config-embedding-cooldown-seconds config) 120)
             "embedding-cooldown-seconds should default to 120")
    
    (format t "~%✓ DMQ config defaults test passed~%")
    t))

(defun test-dmq-config-custom-values ()
  "Test creating durable-memory-query-config with custom values."
  (let ((config (claw-lisp.config:make-durable-memory-query-config
                 :max-results 10
                 :min-relevance-score 0.5s0
                 :default-query-mode :semantic
                 :max-injection-chars 2000
                 :dedup-window-normal 10
                 :injection-enabled nil)))
    (%assert (= (claw-lisp.config:dmq-config-max-results config) 10)
             "max-results should be 10")
    (%assert (= (claw-lisp.config:dmq-config-min-relevance-score config) 0.5s0)
             "min-relevance-score should be 0.5")
    (%assert (eq (claw-lisp.config:dmq-config-default-query-mode config) :semantic)
             "default-query-mode should be :semantic")
    (%assert (= (claw-lisp.config:dmq-config-max-injection-chars config) 2000)
             "max-injection-chars should be 2000")
    (%assert (eq (claw-lisp.config:dmq-config-injection-enabled config) nil)
             "injection-enabled should be NIL")
    (%assert (= (claw-lisp.config:dmq-config-dedup-window-normal config) 10)
             "dedup-window-normal should be 10")
    
    (format t "~%✓ DMQ config custom values test passed~%")
    t))

(defun test-dmq-config-runtime-integration ()
  "Test that durable-memory-query-config integrates with runtime-config."
  (let ((runtime-config (claw-lisp.config:make-default-runtime-config)))
    ;; Check the slot exists and has a default value
    (%assert (typep (claw-lisp.config:runtime-config-durable-memory-query-config runtime-config)
                    'claw-lisp.config:durable-memory-query-config)
             "runtime-config should have durable-memory-query-config slot")
    
    ;; Check defaults are applied
    (let ((dmq-config (claw-lisp.config:runtime-config-durable-memory-query-config runtime-config)))
      (%assert (= (claw-lisp.config:dmq-config-max-results dmq-config) 5)
               "Default max-results should be 5"))
    
    (format t "~%✓ DMQ config runtime integration test passed~%")
    t))

;;; ============================================================
;;; current-dmq-config Tests
;;; ============================================================

(defun test-current-dmq-config-without-session ()
  "Test current-dmq-config returns default when no session provided."
  (let ((*dmq-active-config* nil))
    (let ((config (claw-lisp.config:current-dmq-config)))
      (%assert (typep config 'claw-lisp.config:durable-memory-query-config)
               "Should return durable-memory-query-config")
      (%assert (= (claw-lisp.config:dmq-config-max-results config) 5)
               "Should return default config"))
    (format t "~%✓ current-dmq-config without session test passed~%")
    t))

(defun test-current-dmq-config-with-active-config ()
  "Test current-dmq-config respects *dmq-active-config* dynamic binding."
  (let ((custom-config (claw-lisp.config:make-durable-memory-query-config
                        :max-results 42)))
    (let ((claw-lisp.config:*dmq-active-config* custom-config))
      (let ((config (claw-lisp.config:current-dmq-config)))
        (%assert (eq config custom-config)
                 "current-dmq-config should return the active config, not a default")
        (%assert (= (claw-lisp.config:dmq-config-max-results config) 42)
                 "max-results should be 42 from active config, got ~A"
                 (claw-lisp.config:dmq-config-max-results config)))))
  (format t "~%✓ current-dmq-config with active config test passed~%")
  t)

;;; ============================================================
;;; Circuit Breaker Tests
;;; ============================================================

(defun test-embedding-available-initially ()
  "Test that embedding is available by default (circuit closed)."
  ;; Reset circuit breaker state
  (setf claw-lisp.storage.durable-memory-search:*dmq-embedding-failures* 0)
  (setf claw-lisp.storage.durable-memory-search:*dmq-circuit-open-until* nil)
  
  (%assert (claw-lisp.storage.durable-memory-search:embedding-available-p)
           "Embedding should be available initially")
  
  (format t "~%✓ Embedding available initially test passed~%")
  t)

(defun test-circuit-breaker-opens-after-threshold ()
  "Test that circuit opens after consecutive failures."
  (unwind-protect
      (let ((claw-lisp.config:*dmq-active-config*
              (claw-lisp.config:make-durable-memory-query-config
               :embedding-failure-threshold 3)))
        ;; Reset
        (setf claw-lisp.storage.durable-memory-search:*dmq-embedding-failures* 0)
        (setf claw-lisp.storage.durable-memory-search:*dmq-circuit-open-until* nil)
        ;; Record 2 failures (should still be closed)
        (claw-lisp.storage.durable-memory-search:record-embedding-failure)
        (claw-lisp.storage.durable-memory-search:record-embedding-failure)
        (%assert (claw-lisp.storage.durable-memory-search:embedding-available-p)
                 "Circuit should still be closed after 2 failures")
        ;; Record 3rd failure (should open)
        (claw-lisp.storage.durable-memory-search:record-embedding-failure)
        (%assert (not (claw-lisp.storage.durable-memory-search:embedding-available-p))
                 "Circuit should be open after 3 failures"))
    (setf claw-lisp.storage.durable-memory-search:*dmq-embedding-failures* 0)
    (setf claw-lisp.storage.durable-memory-search:*dmq-circuit-open-until* nil))
  (format t "~%✓ Circuit breaker opens after threshold test passed~%")
  t)

(defun test-circuit-breaker-cooldown ()
  "Test that circuit closes after cooldown period."
  (unwind-protect
      (let ((claw-lisp.config:*dmq-active-config*
              (claw-lisp.config:make-durable-memory-query-config
               :embedding-failure-threshold 2
               :embedding-cooldown-seconds 60)))
        ;; Reset state
        (setf claw-lisp.storage.durable-memory-search:*dmq-embedding-failures* 0)
        (setf claw-lisp.storage.durable-memory-search:*dmq-circuit-open-until* nil)
        ;; Trip the breaker
        (claw-lisp.storage.durable-memory-search:record-embedding-failure)
        (claw-lisp.storage.durable-memory-search:record-embedding-failure)
        (%assert (not (claw-lisp.storage.durable-memory-search:embedding-available-p))
                 "Circuit should be open after reaching threshold")
        ;; Simulate cooldown expiry by backdating the open-until timestamp
        (setf claw-lisp.storage.durable-memory-search:*dmq-circuit-open-until*
              (- (get-universal-time) 1))
        (%assert (claw-lisp.storage.durable-memory-search:embedding-available-p)
                 "Circuit should be closed after cooldown expires"))
    ;; Cleanup
    (setf claw-lisp.storage.durable-memory-search:*dmq-embedding-failures* 0)
    (setf claw-lisp.storage.durable-memory-search:*dmq-circuit-open-until* nil))
  (format t "~%✓ Circuit breaker cooldown test passed~%")
  t)

(defun test-embedding-success-resets-circuit ()
  "Test that recording success resets the circuit breaker."
  (unwind-protect
      (let ((claw-lisp.config:*dmq-active-config*
              (claw-lisp.config:make-durable-memory-query-config
               :embedding-failure-threshold 3)))
        ;; Reset state
        (setf claw-lisp.storage.durable-memory-search:*dmq-embedding-failures* 0)
        (setf claw-lisp.storage.durable-memory-search:*dmq-circuit-open-until* nil)
        ;; Accumulate some failures (but don't trip)
        (claw-lisp.storage.durable-memory-search:record-embedding-failure)
        (claw-lisp.storage.durable-memory-search:record-embedding-failure)
        (%assert (= claw-lisp.storage.durable-memory-search:*dmq-embedding-failures* 2)
                 "Should have 2 failures recorded")
        ;; Success should reset
        (claw-lisp.storage.durable-memory-search:record-embedding-success)
        (%assert (= claw-lisp.storage.durable-memory-search:*dmq-embedding-failures* 0)
                 "Failures should be reset to 0 after success, got ~A"
                 claw-lisp.storage.durable-memory-search:*dmq-embedding-failures*)
        (%assert (null claw-lisp.storage.durable-memory-search:*dmq-circuit-open-until*)
                 "Circuit open-until should be nil after success")
        (%assert (claw-lisp.storage.durable-memory-search:embedding-available-p)
                 "Embedding should be available after success"))
    ;; Cleanup
    (setf claw-lisp.storage.durable-memory-search:*dmq-embedding-failures* 0)
    (setf claw-lisp.storage.durable-memory-search:*dmq-circuit-open-until* nil))
  (format t "~%✓ Embedding success resets circuit test passed~%")
  t)

(defun test-reset-circuit-breaker ()
  "Test manual circuit breaker reset."
  (unwind-protect
      (let ((claw-lisp.config:*dmq-active-config*
              (claw-lisp.config:make-durable-memory-query-config
               :embedding-failure-threshold 2)))
        ;; Reset state
        (setf claw-lisp.storage.durable-memory-search:*dmq-embedding-failures* 0)
        (setf claw-lisp.storage.durable-memory-search:*dmq-circuit-open-until* nil)
        ;; Trip the breaker
        (claw-lisp.storage.durable-memory-search:record-embedding-failure)
        (claw-lisp.storage.durable-memory-search:record-embedding-failure)
        (%assert (not (claw-lisp.storage.durable-memory-search:embedding-available-p))
                 "Circuit should be open after threshold")
        ;; Manual reset
        (claw-lisp.storage.durable-memory-search:reset-embedding-circuit-breaker)
        (%assert (= claw-lisp.storage.durable-memory-search:*dmq-embedding-failures* 0)
                 "Failures should be 0 after reset")
        (%assert (null claw-lisp.storage.durable-memory-search:*dmq-circuit-open-until*)
                 "Circuit open-until should be nil after reset")
        (%assert (claw-lisp.storage.durable-memory-search:embedding-available-p)
                 "Embedding should be available after reset"))
    ;; Cleanup
    (setf claw-lisp.storage.durable-memory-search:*dmq-embedding-failures* 0)
    (setf claw-lisp.storage.durable-memory-search:*dmq-circuit-open-until* nil))
  (format t "~%✓ Reset circuit breaker test passed~%")
  t)

;;; ============================================================
;;; Test Runner
;;; ============================================================

(defun run-durable-memory-query-tests ()
  "Run all Task 6 Step 1 unit tests. Returns T if all pass."
  (format t "~%&=== Phase 7 Task 6 Step 1 — Unit Tests ===~%")
  (let ((results (list
                  ;; Config tests
                  (test-dmq-config-defaults)
                  (test-dmq-config-custom-values)
                  (test-dmq-config-runtime-integration)
                  ;; current-dmq-config tests
                  (test-current-dmq-config-without-session)
                  (test-current-dmq-config-with-active-config)
                  ;; Circuit breaker tests
                  (test-embedding-available-initially)
                  (test-circuit-breaker-opens-after-threshold)
                  (test-circuit-breaker-cooldown)
                  (test-embedding-success-resets-circuit)
                  (test-reset-circuit-breaker))))
    (if (every #'identity results)
        (progn
          (format t "~%&ALL TASK 6 STEP 1 TESTS PASSED~%")
          t)
        (progn
          (format t "~%&SOME TASK 6 STEP 1 TESTS FAILED~%")
          nil))))

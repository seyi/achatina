;;;; lisp/tests/phase7-e2e-fixtures.lisp
;;;;
;;;; Phase 7 Task 7 — E2E Test Fixtures
;;;;
;;;; Provides test infrastructure for durable memory E2E tests.
;;;; Run with: (claw-lisp.tests:run-phase7-e2e-tests)

(in-package #:claw-lisp.tests)

;;; ============================================================
;;; Local Test Helper
;;; ============================================================

(defun %assert (condition format-control &rest format-args)
  "Assert that CONDITION is true. Signals an error with FORMAT-ARGS on failure."
  (unless condition
    (error (apply #'format nil format-control format-args))))

;;; ============================================================
;;; Mock Embedding Provider
;;; ============================================================

(defun %mock-embedding-vector (seed &optional (dim 384))
  "Return a deterministic pseudo-random embedding vector for SEED.

   Different seeds produce vectors with known cosine distances.
   This allows testing that retrieval ranks results correctly."
  (let ((vec (make-array dim :element-type 'single-float)))
    ;; Deterministic fill using seed — no dependency on non-standard
    ;; make-random-state keywords.
    (let ((s (coerce seed 'double-float)))
      (dotimes (i dim)
        (setf s (mod (+ (* s 1103515245.0d0) 12345.0d0) (expt 2.0d0 31)))
        (setf (aref vec i)
              (coerce (- (/ s (expt 2.0d0 31)) 0.5) 'single-float))))
    vec))

(defun %mock-query-embedding (target-seed &optional (noise 0.1) (dim 384))
  "Return an embedding vector similar to TARGET-SEED's vector.

   Useful for testing that queries retrieve the correct memory."
  (let ((base (%mock-embedding-vector target-seed dim))
        (perturbation (%mock-embedding-vector (1+ target-seed) dim)))
    (dotimes (i dim)
      (setf (aref base i)
            (+ (aref base i) (* noise (aref perturbation i)))))
    base))

;;; ============================================================
;;; Test Memory Store Setup
;;; ============================================================

(defun %create-test-memory (&key (id "test-1")
                                 (kind :user)
                                 (subject-id "test-user")
                                 (title "Test Memory")
                                 (content "Test content")
                                 (importance 0.5)
                                 (embedding-seed 1))
  "Create a single test memory record with deterministic embedding."
  (claw-lisp.storage.durable-memory:make-durable-memory-record
   :id id
   :kind kind
   :subject-id subject-id
   :title title
   :content content
   :importance-score importance
   :embedding (coerce (%mock-embedding-vector embedding-seed) 'list)))

(defun %populate-test-memory-store (&key (count 10)
                                         (kinds '(:user :project)))
  "Create N test memories and save to disk."
  (let ((memories '()))
    ;; Create memories
    (dotimes (i count)
      (let* ((kind (nth (mod i (length kinds)) kinds))
             (memory (%create-test-memory
                      :id (format nil "test-mem-~D" i)
                      :kind kind
                      :subject-id "test-user"
                      :title (format nil "Test Memory ~D" i)
                      :content (format nil "Test content for memory ~D" i)
                      :importance (coerce (/ (1+ i) (1+ count)) 'single-float)
                      :embedding-seed (coerce (+ i 0.5) 'single-float))))
        (push memory memories)
        ;; Save to disk
        (claw-lisp.storage.durable-memory:save-durable-memory-record memory)))

    (nreverse memories)))

(defmacro %with-temp-memory-store ((&key (count 5) (kinds nil))
                                   &body body)
  "Execute BODY with an isolated temporary memory store.

   Uses a unique temp directory per invocation and dynamic binding
   to avoid test collisions. Cleans up on exit."
  (let ((temp-var (gensym "TEMP-DIR-")))
    `(let* ((,temp-var (merge-pathnames
                        (format nil "phase7-test-~A-~A/"
                                (sb-posix:getpid)
                                (get-internal-real-time))
                        #P"/tmp/"))
            (claw-lisp.storage.durable-memory:*durable-memory-storage-root* ,temp-var))
       (unwind-protect
           (progn
             (ensure-directories-exist (merge-pathnames "user/" ,temp-var))
             (ensure-directories-exist (merge-pathnames "project/" ,temp-var))
             (ensure-directories-exist (merge-pathnames "feedback/" ,temp-var))
             (ensure-directories-exist (merge-pathnames "reference/" ,temp-var))
             (%populate-test-memory-store :count ,count
                                          ,@(when kinds `(:kinds ,kinds)))
             ,@body)
         (when (probe-file ,temp-var)
           (uiop:delete-directory-tree (pathname ,temp-var) :validate t))))))

;;; ============================================================
;;; Test Session Factory
;;; ============================================================

(defun %create-test-session (&key (config nil)
                                  (dedup-log nil)
                                  (turn-id 0))
  "Create a test session with optional configuration."
  (let ((session (claw-lisp.core.domain:make-agent-session
                  :id (format nil "test-session-~D" (get-universal-time))
                  :provider "test"
                  :model "test-model"
                  :conversation (claw-lisp.core.domain:make-conversation
                                 :messages nil)
                  :state nil)))
    ;; Set config
    (when config
      (setf (getf (claw-lisp.core.domain:agent-session-state session)
                  :dmq-config)
            config))
    ;; Set dedup log
    (when dedup-log
      (setf (claw-lisp.core.domain:session-memory-injection-log session)
            dedup-log))
    ;; Set turn ID
    (setf (claw-lisp.core.domain:session-current-turn-id session)
          turn-id)
    session))

;;; ============================================================
;;; Conversation Simulator
;;; ============================================================

(defun %simulate-turn (session user-content &key (expected-injection-p t))
  "Run the runtime pre-turn pipeline with memory injection.

   This simulates a user turn by:
   1. Creating a user message
   2. Calling inject-durable-memory-context
   3. Returning the augmented message list"
  (declare (ignore expected-injection-p))
  (let* ((user-message (claw-lisp.core.domain:make-message
                        :role :user
                        :content user-content))
         (messages (list user-message))
         (turn (claw-lisp.core.domain:make-agent-turn
                :content user-content
                :messages messages
                :tool-results nil
                :metadata nil)))
    ;; Call injection
    (multiple-value-bind (injected-p count)
        (claw-lisp.storage.durable-memory-search:inject-durable-memory-context
         session
         turn
         :pass :initial
         :force-refresh nil)
      (declare (ignore injected-p))
      ;; Get augmented messages
      (let ((augmented-messages (claw-lisp.core.domain:agent-turn-messages turn)))
        ;; Build injection metadata
        (let ((metadata (list :count count
                              :turn-id (claw-lisp.core.domain:session-current-turn-id session))))
          (values augmented-messages metadata))))))

;;; ============================================================
;;; Assertion Fixtures
;;; ============================================================

(defun %assert-injected-memory-count (metadata expected-count &optional (message nil))
  "Assert that injection metadata shows EXPECTED-COUNT memories were injected."
  (let ((actual-count (getf metadata :count)))
    (%assert (= actual-count expected-count)
             (or message
                 "Expected ~D injected memories, got ~D")
             expected-count
             actual-count)))

(defun %assert-injected-memory-kind (messages expected-kind &optional (message nil))
  "Assert that injected memory has EXPECTED-KIND."
  ;; Find memory context message
  (let ((memory-msg (find-if (lambda (msg)
                               (let ((content (claw-lisp.core.domain:message-content msg)))
                                 (and (stringp content)
                                      (search "[MEMORY CONTEXT]" content))))
                             messages)))
    (%assert memory-msg
             (or message "No memory context message found in messages"))
    ;; Check kind is present
    (let ((content (claw-lisp.core.domain:message-content memory-msg)))
      (%assert (search (format nil "[~(~A~)]" expected-kind) content :test #'char-equal)
               (or message "Expected kind ~A not found in memory context")
               expected-kind))))

(defun %assert-no-injection (metadata &optional (message nil))
  "Assert that no memory injection occurred."
  (%assert-injected-memory-count metadata 0 message))

;;; ============================================================
;;; Circuit Breaker Test Fixtures
;;; ============================================================

(defun %make-failing-embedding-provider (&key (fail-count 3)
                                              (error-type 'simple-error))
  "Return an embedding provider that fails FAIL-COUNT times then succeeds."
  (let ((calls 0))
    (lambda (text)
      (incf calls)
      (if (<= calls fail-count)
          (error error-type :format-control "Simulated embedding failure ~D"
                            :format-arguments (list calls))
          (%mock-embedding-vector (sxhash text))))))

(defmacro %with-circuit-breaker-config ((&key (cooldown-seconds 1)
                                              (failure-threshold 3))
                                        &body body)
  "Execute BODY with custom circuit breaker configuration.
   Uses dynamic binding — automatically restored on exit."
  `(let ((claw-lisp.config:*dmq-active-config*
           (claw-lisp.config:make-durable-memory-query-config
            :embedding-cooldown-seconds ,cooldown-seconds
            :embedding-failure-threshold ,failure-threshold)))
     ,@body))

;;; ============================================================
;;; State Cleanup Fixtures
;;; ============================================================

(defun %reset-circuit-breaker ()
  "Reset circuit breaker to initial state."
  (setf claw-lisp.storage.durable-memory-search:*dmq-embedding-failures* 0)
  (setf claw-lisp.storage.durable-memory-search:*dmq-circuit-open-until* nil))

(defmacro %with-clean-state (&body body)
  "Execute BODY with all Phase 7 global state reset.
   
   Resets:
   - Circuit breaker
   - Active config"
  `(unwind-protect
       (progn ,@body)
     (%reset-circuit-breaker)
     (setf claw-lisp.config:*dmq-active-config* nil)))

;;; ============================================================
;;; Performance Test Utilities
;;; ============================================================

(defun %measure-execution-time-ms (fn)
  "Measure execution time of FN in milliseconds."
  (let* ((start (get-internal-real-time))
         (result (funcall fn))
         (end (get-internal-real-time))
         (elapsed-ms (/ (* 1000 (- end start))
                        internal-time-units-per-second)))
    (values elapsed-ms result)))

;;; ============================================================
;;; Smoke Test (Run FIRST)
;;; ============================================================

(defun run-phase7-smoke-test ()
  "Run a single smoke test to validate fixture chain.
   
   This should be run FIRST before any other E2E tests.
   If this fails, the fixtures are broken and all tests will fail.
   
   Returns T on success, NIL on failure."
  (format t "~%&=== Phase 7 E2E Smoke Test ===~%")
  (handler-case
      (%with-clean-state
        (%with-temp-memory-store (:count 3)
          (let* ((session (%create-test-session))
                 (user-msg "What are my preferences?"))
            ;; Simulate a turn
            (multiple-value-bind (messages metadata)
                (%simulate-turn session user-msg)
              ;; Verify injection occurred
              (%assert-injected-memory-count metadata 1)
              (%assert-injected-memory-kind messages :user)
              (format t "~%✓ Smoke test passed~%")
              t))))
    (error (e)
      (format t "~%✗ Smoke test FAILED: ~A~%" e)
      nil)))

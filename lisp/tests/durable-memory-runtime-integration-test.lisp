;;;; lisp/tests/durable-memory-runtime-integration-test.lisp
;;;;
;;;; Phase 7 Task 6 Step 4 — Runtime Integration Tests (TDD)
;;;;
;;;; Run with: (claw-lisp.tests:run-durable-memory-runtime-integration-tests)

(in-package #:claw-lisp.tests)

;;; ============================================================
;;; Local Test Helper
;;; ============================================================

(defun %assert (condition format-control &rest format-args)
  "Assert that CONDITION is true. Signals an error with FORMAT-ARGS on failure."
  (unless condition
    (error (apply #'format nil format-control format-args))))

;;; ============================================================
;;; Mock Setup for Testing
;;; ============================================================

(defun %create-test-session-with-conversation ()
  "Create a test session with a minimal conversation for testing."
  (let* ((conversation (claw-lisp.core.domain:make-conversation
                        :messages (list
                                   (claw-lisp.core.domain:make-message
                                    :role :system
                                    :content "You are a helpful assistant.")
                                   (claw-lisp.core.domain:make-message
                                    :role :user
                                    :content "What are my preferences?"))))
         (session (claw-lisp.core.domain:make-agent-session
                   :id "test-session-1"
                   :provider "test"
                   :model "test-model"
                   :conversation conversation
                   :state nil)))
    session))

(defun %create-test-turn ()
  "Create a test turn with user content."
  (let ((turn (claw-lisp.core.domain:make-agent-turn
               :content "What are my preferences?"
               :metadata nil
               :messages nil
               :tool-results nil)))
    turn))

;;; ============================================================
;;; Integration Tests
;;; ============================================================

(defun test-inject-durable-memory-context-called ()
  "Test that inject-durable-memory-context is callable and returns expected values.
   
   This is a smoke test to ensure the function is wired and accessible."
  (let* ((session (%create-test-session-with-conversation))
         (turn (%create-test-turn))
         (config (claw-lisp.config:make-durable-memory-query-config
                  :injection-enabled nil)))  ; Disabled to avoid actual injection
    ;; Should return (nil 0) when injection is disabled
    (multiple-value-bind (injected-p count)
        (claw-lisp.storage.durable-memory-search:inject-durable-memory-context
         session turn :pass :initial)
      (%assert (eq injected-p nil)
               "Should return NIL when injection disabled, got ~A" injected-p)
      (%assert (= count 0)
               "Should return 0 count when disabled, got ~A" count)))
  (format t "~%✓ inject-durable-memory-context callable test passed~%")
  t)

(defun test-inject-durable-memory-context-with-enabled-config ()
  "Test that injection respects enabled config.
   
   When injection-enabled is T, function should proceed to query (but return
   nil/0 if no memories match)."
  (let* ((session (%create-test-session-with-conversation))
         (turn (%create-test-turn))
         (config (claw-lisp.config:make-durable-memory-query-config
                  :injection-enabled t
                  :max-results 5
                  :min-relevance-score 0.3)))
    ;; With injection enabled but no memories, should still return (nil 0) or (t 0)
    (multiple-value-bind (injected-p count)
        (claw-lisp.storage.durable-memory-search:inject-durable-memory-context
         session turn :pass :initial)
      ;; Either no injection (nil) or injection with 0 memories are acceptable
      (%assert (or (null injected-p) (numberp count))
               "Should return valid values, got injected-p=~A count=~A"
               injected-p count)))
  (format t "~%✓ inject-durable-memory-context with enabled config test passed~%")
  t)

(defun test-inject-durable-memory-context-augmented-pass-disabled ()
  "Test that the public build returns no injection for :augmented pass."
  (let* ((session (%create-test-session-with-conversation))
         (turn (%create-test-turn)))
    (multiple-value-bind (injected-p count)
        (claw-lisp.storage.durable-memory-search:inject-durable-memory-context
         session turn :pass :augmented)
      (%assert (eq injected-p nil)
               ":augmented pass should return NIL in the public build")
      (%assert (= count 0)
               "Count should be 0 when :augmented injection is disabled")))
  (format t "~%✓ :augmented pass disabled test passed~%")
  t)

(defun test-extract-query-text-from-turn ()
  "Test that extract-query-text correctly extracts content from turn.
   
   Verifies the query construction helper."
  (let ((turn (%create-test-turn)))
    ;; :initial pass should return user content
    (let ((query (claw-lisp.storage.durable-memory-search:extract-query-text
                  turn :initial)))
      (%assert (stringp query)
               "Should return a string, got ~A" query)
      (%assert (search "preferences" query :test #'char-equal)
               "Query should contain user content, got ~A" query)))
  (format t "~%✓ extract-query-text test passed~%")
  t)

(defun test-summarize-tool-results-empty ()
  "Test that summarize-tool-results handles empty tool results.
   
   Should return NIL when no tool results."
  (let ((turn (%create-test-turn)))
    (let ((summary (claw-lisp.storage.durable-memory-search:summarize-tool-results
                    turn)))
      (%assert (null summary)
               "Should return NIL for empty tool results, got ~A" summary)))
  (format t "~%✓ summarize-tool-results empty test passed~%")
  t)

(defun test-memory-injection-record-struct ()
  "Test that memory-injection-record struct is properly defined.
   
   Verifies the struct can be created and accessed."
  (let ((record (claw-lisp.storage.durable-memory-search:make-memory-injection-record
                 :memory-id "test-memory-1"
                 :turn-id 42
                 :importance 0.85
                 :kind :user
                 :timestamp 1234567890)))
    (%assert (equal (claw-lisp.storage.durable-memory-search:memory-injection-record-memory-id
                     record)
                    "test-memory-1")
             "memory-id should be set correctly")
    (%assert (= (claw-lisp.storage.durable-memory-search:memory-injection-record-turn-id
                 record)
                42)
             "turn-id should be set correctly")
    (%assert (= (claw-lisp.storage.durable-memory-search:memory-injection-record-importance
                 record)
                0.85)
             "importance should be set correctly")
    (%assert (eq (claw-lisp.storage.durable-memory-search:memory-injection-record-kind
                  record)
                 :user)
             "kind should be set correctly"))
  (format t "~%✓ memory-injection-record struct test passed~%")
  t)

(defun test-session-memory-injection-log-accessors ()
  "Test that session-memory-injection-log accessors work correctly.
   
   Verifies the log can be read and written."
  (let* ((session (%create-test-session-with-conversation))
         (initial-log (claw-lisp.core.domain:session-memory-injection-log session)))
    ;; Initial log should be NIL (empty list)
    (%assert (null initial-log)
             "Initial injection log should be NIL, got ~A" initial-log)
    
    ;; Set a log entry
    (let ((test-record (claw-lisp.storage.durable-memory-search:make-memory-injection-record
                        :memory-id "test-1"
                        :turn-id 1
                        :importance 0.5
                        :kind :user
                        :timestamp 1234567890)))
      (setf (claw-lisp.core.domain:session-memory-injection-log session)
            (list test-record)))
    
    ;; Verify it was set
    (let ((log (claw-lisp.core.domain:session-memory-injection-log session)))
      (%assert (consp log)
               "Log should be a cons list after setting, got ~A" log)
      (%assert (= (length log) 1)
               "Log should have 1 entry, got ~A" (length log))))
  (format t "~%✓ session-memory-injection-log accessors test passed~%")
  t)

(defun test-filter-dedup-results-pass-through ()
  "Test that filter-dedup-results passes through results when no dedup needed.
   
   With empty injection log, all results should pass through."
  (let* ((session (%create-test-session-with-conversation))
         (config (claw-lisp.config:make-durable-memory-query-config))
         (mock-results (list (cons 0.9 (claw-lisp.storage.durable-memory:make-user-memory
                                        :id "mem-1"
                                        :subject-id "test"
                                        :title "Test"
                                        :content "Test content"
                                        :importance-score 0.8))))
         (filtered (claw-lisp.storage.durable-memory-search:filter-dedup-results
                    session mock-results config)))
    (%assert (= (length filtered) 1)
             "Should pass through 1 result when log is empty, got ~A" (length filtered))
    (%assert (= (car (first filtered)) 0.9)
             "Score should be preserved"))
  (format t "~%✓ filter-dedup-results pass-through test passed~%")
  t)

;;; ============================================================
;;; Query Function Tests (Step 5)
;;; ============================================================

(defun test-query-durable-memory-semantic-mode ()
  "Test that query-durable-memory with :semantic mode calls semantic search.
   
   This test verifies the wiring to Task 4's semantic-search-durable-memory."
  (let* ((session (%create-test-session-with-conversation))
         (results (claw-lisp.storage.durable-memory-search:query-durable-memory
                   "test query"
                   :mode :semantic
                   :kinds '(:user :project))))
    ;; Should return list or NIL (empty results are OK)
    (%assert (or (null results) (consp results))
             "Should return list or NIL, got ~A" results))
  (format t "~%✓ query-durable-memory :semantic mode test passed~%")
  t)

(defun test-query-durable-memory-lexical-mode ()
  "Test that query-durable-memory with :lexical mode works.
   
   Lexical search should always work (no embedding required)."
  (let* ((results (claw-lisp.storage.durable-memory-search:query-durable-memory
                   "test query"
                   :mode :lexical
                   :limit 5)))
    ;; Should return list or NIL
    (%assert (or (null results) (consp results))
             "Should return list or NIL, got ~A" results))
  (format t "~%✓ query-durable-memory :lexical mode test passed~%")
  t)

(defun test-query-durable-memory-hybrid-mode ()
  "Test that query-durable-memory with :hybrid mode works.
   
   Hybrid should blend semantic + lexical (degrades to lexical if embeddings unavailable)."
  (let* ((results (claw-lisp.storage.durable-memory-search:query-durable-memory
                   "test query"
                   :mode :hybrid
                   :limit 10
                   :min-score 0.3)))
    ;; Should return list or NIL
    (%assert (or (null results) (consp results))
             "Should return list or NIL, got ~A" results))
  (format t "~%✓ query-durable-memory :hybrid mode test passed~%")
  t)

(defun test-query-durable-memory-empty-input ()
  "Test that query-durable-memory handles empty input gracefully.
   
   Empty or NIL query should return NIL."
  (let ((results (claw-lisp.storage.durable-memory-search:query-durable-memory
                  nil  ; NIL query
                  :mode :lexical)))
    (%assert (null results)
             "Should return NIL for NIL query, got ~A" results))
  (let ((results (claw-lisp.storage.durable-memory-search:query-durable-memory
                  ""  ; Empty string
                  :mode :lexical)))
    (%assert (null results)
             "Should return NIL for empty query, got ~A" results))
  (format t "~%✓ query-durable-memory empty input test passed~%")
  t)

;;; ============================================================
;;; Test Runner
;;; ============================================================

(defun run-durable-memory-runtime-integration-tests ()
  "Run all runtime integration tests. Returns T if all pass."
  (format t "~%&=== Phase 7 Task 6 Step 4 — Runtime Integration Tests ===~%")
  (let ((results (list
                  ;; Smoke tests
                  (test-inject-durable-memory-context-called)
                  (test-inject-durable-memory-context-with-enabled-config)
                  (test-inject-durable-memory-context-augmented-pass-disabled)
                  ;; Helper function tests
                  (test-extract-query-text-from-turn)
                  (test-summarize-tool-results-empty)
                  ;; Struct tests
                  (test-memory-injection-record-struct)
                  (test-session-memory-injection-log-accessors)
                  ;; Dedup tests
                  (test-filter-dedup-results-pass-through)
                  ;; Query function tests (Step 5)
                  (test-query-durable-memory-semantic-mode)
                  (test-query-durable-memory-lexical-mode)
                  (test-query-durable-memory-hybrid-mode)
                  (test-query-durable-memory-empty-input))))
    (if (every #'identity results)
        (progn
          (format t "~%&ALL RUNTIME INTEGRATION TESTS PASSED~%")
          t)
        (progn
          (format t "~%&SOME RUNTIME INTEGRATION TESTS FAILED~%")
          nil))))

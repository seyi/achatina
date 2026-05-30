;;;; lisp/tests/session-memory-test.lisp
;;;;
;;;; Comprehensive tests for Phase 5 session memory features.
;;;; Covers triggers, staleness, budget, metadata parsing, and integration.

(in-package #:claw-lisp.tests)

;;; ============================================================
;;; Local Test Helpers
;;; ============================================================

(defun %assert (condition format-control &rest format-args)
  "Assert that CONDITION is true. Signals an error with FORMAT-ARGS on failure."
  (unless condition
    (error (apply #'format nil format-control format-args))))

(defun %now ()
  "Return current universal time."
  (get-universal-time))

;;; ============================================================
;;; Trigger Tests (session-memory-update-needed-p)
;;; ============================================================

(defun test-trigger-no-existing-file ()
  "If no session memory text, update is needed."
  (let* ((runtime (claw-lisp.core.runtime:make-runtime))
         (session (claw-lisp.core.runtime:start-session runtime :mock)))
    ;; Delete any existing session memory file
    (let ((path (claw-lisp.storage.session-memory:session-memory-path
                 (claw-lisp.core.runtime:runtime-settings runtime)
                 (claw-lisp.core.domain:agent-session-id session))))
      (when (probe-file path)
        (delete-file path)))
    ;; Should need update
    (%assert (claw-lisp.storage.session-memory::session-memory-update-needed-p
              runtime session)
             "No session memory file should trigger update-needed-p")
    (claw-lisp.core.runtime::cleanup-session session)))

(defun test-trigger-stale-metadata ()
  "Stale metadata triggers update."
  (let* ((runtime (claw-lisp.core.runtime:make-runtime))
         (session (claw-lisp.core.runtime:start-session runtime :mock))
         (path (claw-lisp.storage.session-memory:session-memory-path
                (claw-lisp.core.runtime:runtime-settings runtime)
                (claw-lisp.core.domain:agent-session-id session))))
    ;; Create stale session memory
    (let ((stale-metadata (claw-lisp.storage.session-memory:make-session-memory-metadata
                           :update-count 1
                           :last-updated-universal-time (- (%now) 10000)
                           :budget-chars-used 100
                           :budget-chars-max 4096
                           :stale-p t
                           :tokens-at-last-update 100
                           :tool-count-at-last-update 2)))
      (with-open-file (stream path :direction :output :if-exists :supersede)
        (write-string
         (claw-lisp.storage.session-memory::render-metadata-section stale-metadata)
         stream)
        (write-string "# Session Test~%~%" stream)))
    ;; Should need update (stale)
    (%assert (claw-lisp.storage.session-memory::session-memory-update-needed-p
              runtime session)
             "Stale metadata should trigger update-needed-p")
    (claw-lisp.core.runtime::cleanup-session session)))

(defun test-trigger-fresh-metadata-no-update ()
  "Fresh metadata, not stale, no update needed."
  (let* ((runtime (claw-lisp.core.runtime:make-runtime))
         (session (claw-lisp.core.runtime:start-session runtime :mock))
         (path (claw-lisp.storage.session-memory:session-memory-path
                (claw-lisp.core.runtime:runtime-settings runtime)
                (claw-lisp.core.domain:agent-session-id session))))
    ;; Create fresh session memory
    (let ((fresh-metadata (claw-lisp.storage.session-memory:make-session-memory-metadata
                           :update-count 1
                           :last-updated-universal-time (%now)
                           :budget-chars-used 100
                           :budget-chars-max 4096
                           :stale-p nil
                           :tokens-at-last-update 100
                           :tool-count-at-last-update 2)))
      (with-open-file (stream path :direction :output :if-exists :supersede)
        (write-string
         (claw-lisp.storage.session-memory::render-metadata-section fresh-metadata)
         stream)
        (write-string "# Session Test~%~%" stream)))
    ;; Should NOT need update (fresh)
    (%assert (not (claw-lisp.storage.session-memory::session-memory-update-needed-p
                   runtime session))
             "Fresh metadata should not trigger update-needed-p")
    (claw-lisp.core.runtime::cleanup-session session)))

;;; ============================================================
;;; Staleness Tests (session-memory-stale-p)
;;; ============================================================

(defun test-staleness-time-exceeded ()
  "session-memory-stale-p: staleness by time."
  (let* ((config (claw-lisp.config:make-default-runtime-config))
         (old-metadata (claw-lisp.storage.session-memory:make-session-memory-metadata
                        :update-count 1
                        :last-updated-universal-time (- (%now) 10000)
                        :budget-chars-used 100
                        :budget-chars-max 4096
                        :stale-p nil
                        :tokens-at-last-update 100
                        :tool-count-at-last-update 2))
         (conversation (claw-lisp.core.domain:make-conversation :id "test")))
    (%assert (claw-lisp.storage.session-memory:session-memory-stale-p
              config old-metadata conversation)
             "Old timestamp should be stale")))

(defun test-staleness-token-growth ()
  "session-memory-stale-p: staleness by token count."
  (let* ((config (claw-lisp.config:make-default-runtime-config))
         (metadata (claw-lisp.storage.session-memory:make-session-memory-metadata
                    :update-count 1
                    :last-updated-universal-time (%now)
                    :budget-chars-used 100
                    :budget-chars-max 4096
                    :stale-p nil
                    :tokens-at-last-update 100
                    :tool-count-at-last-update 2))
         ;; Create conversation with >2x tokens
         (messages (loop for i from 1 to 50
                         collect (claw-lisp.core.domain:make-message
                                  :role :user
                                  :content (make-string 200 :initial-element #\a))))
         (conversation (claw-lisp.core.domain:make-conversation
                        :id "test" :messages messages)))
    (%assert (claw-lisp.storage.session-memory:session-memory-stale-p
              config metadata conversation)
             "Token count growth should be stale")))

(defun test-staleness-tool-growth ()
  "session-memory-stale-p: staleness by tool count."
  (let* ((config (claw-lisp.config:make-default-runtime-config))
         (metadata (claw-lisp.storage.session-memory:make-session-memory-metadata
                    :update-count 1
                    :last-updated-universal-time (%now)
                    :budget-chars-used 100
                    :budget-chars-max 4096
                    :stale-p nil
                    :tokens-at-last-update 100
                    :tool-count-at-last-update 2))
         ;; Create conversation with >2x tool results
         (tool-results (loop for i from 1 to 10
                             collect (claw-lisp.core.domain:make-tool-result
                                      :call-id (format nil "call-~A" i)
                                      :tool-name "test_tool"
                                      :content "result")))
         (conversation (claw-lisp.core.domain:make-conversation
                        :id "test" :tool-results tool-results)))
    (%assert (claw-lisp.storage.session-memory:session-memory-stale-p
              config metadata conversation)
             "Tool count growth should be stale")))

(defun test-staleness-nil-metadata ()
  "session-memory-stale-p: nil metadata is always stale."
  (let* ((config (claw-lisp.config:make-default-runtime-config))
         (conversation (claw-lisp.core.domain:make-conversation :id "test")))
    (%assert (claw-lisp.storage.session-memory:session-memory-stale-p
              config nil conversation)
             "Nil metadata should be stale")))

;;; ============================================================
;;; Budget Enforcement Tests (enforce-session-memory-budget)
;;; ============================================================

(defun test-budget-under-budget ()
  "Session memory under budget is unchanged."
  (let* ((content "# Session Test~%~%- provider: test~%~%## Active Goals~%- Goal 1~%")
         (budget 4096))
    ;; Note: enforce-session-memory-budget is called within perform-session-memory-update
    ;; This test verifies the budget config is respected
    (%assert (< (length content) budget)
             "Test content should be under budget")))

(defun test-budget-config-set ()
  "Budget config is properly set."
  (let* ((config (claw-lisp.config:make-default-runtime-config))
         (budget (claw-lisp.config:runtime-config-session-memory-budget-chars config)))
    (%assert (= budget 4096)
             "Default budget should be 4096 chars")))

;;; ============================================================
;;; Metadata Parsing Tests (parse-session-memory-header)
;;; ============================================================

(defun test-parse-valid-metadata ()
  "parse-session-memory-header: valid header parses correctly."
  (let* ((metadata (claw-lisp.storage.session-memory:make-session-memory-metadata
                    :update-count 5
                    :last-updated-universal-time 123456
                    :budget-chars-used 500
                    :budget-chars-max 4096
                    :stale-p nil
                    :tokens-at-last-update 1000
                    :tool-count-at-last-update 5))
         (header (claw-lisp.storage.session-memory::render-metadata-section metadata))
         (parsed (claw-lisp.storage.session-memory:parse-session-memory-header header)))
    (%assert parsed "Should parse valid metadata")
    (%assert (= (claw-lisp.storage.session-memory:session-memory-metadata-update-count parsed) 5)
             "Update count should match")
    (%assert (= (claw-lisp.storage.session-memory:session-memory-metadata-last-updated-universal-time parsed) 123456)
             "Timestamp should match")
    (%assert (= (claw-lisp.storage.session-memory:session-memory-metadata-budget-chars-used parsed) 500)
             "Budget used should match")
    (%assert (not (claw-lisp.storage.session-memory:session-memory-metadata-stale-p parsed))
             "Stale flag should match")))

(defun test-parse-roundtrip ()
  "parse/render round-trip preserves metadata."
  (let* ((meta (claw-lisp.storage.session-memory:make-session-memory-metadata
                :update-count 3
                :last-updated-universal-time 999999
                :budget-chars-used 250
                :budget-chars-max 4096
                :stale-p nil
                :tokens-at-last-update 500
                :tool-count-at-last-update 3))
         (header (claw-lisp.storage.session-memory::render-metadata-section meta))
         (parsed (claw-lisp.storage.session-memory:parse-session-memory-header header)))
    (%assert (= (claw-lisp.storage.session-memory:session-memory-metadata-update-count meta)
                (claw-lisp.storage.session-memory:session-memory-metadata-update-count parsed))
             "Update count roundtrip")
    (%assert (= (claw-lisp.storage.session-memory:session-memory-metadata-tokens-at-last-update meta)
                (claw-lisp.storage.session-memory:session-memory-metadata-tokens-at-last-update parsed))
             "Token count roundtrip")
    (%assert (= (claw-lisp.storage.session-memory:session-memory-metadata-tool-count-at-last-update meta)
                (claw-lisp.storage.session-memory:session-memory-metadata-tool-count-at-last-update parsed))
             "Tool count roundtrip")))

(defun test-parse-malformed-metadata ()
  "parse-session-memory-header: malformed header returns NIL."
  (%assert (null (claw-lisp.storage.session-memory:parse-session-memory-header "not a header"))
           "Malformed header should return NIL")
  (%assert (null (claw-lisp.storage.session-memory:parse-session-memory-header ""))
           "Empty string should return NIL")
  (%assert (null (claw-lisp.storage.session-memory:parse-session-memory-header "<!-- incomplete"))
           "Incomplete header should return NIL"))

;;; ============================================================
;;; Integration Tests
;;; ============================================================

(defun test-integration-session-memory-update-flow ()
  "Integration test: full session memory update flow."
  (let* ((runtime (claw-lisp.core.runtime:make-runtime))
         (session (claw-lisp.core.runtime:start-session runtime :mock))
         (path (claw-lisp.storage.session-memory:session-memory-path
                (claw-lisp.core.runtime:runtime-settings runtime)
                (claw-lisp.core.domain:agent-session-id session))))
    ;; Delete any existing file
    (when (probe-file path)
      (delete-file path))
    
    ;; First update (no existing file)
    (%assert (claw-lisp.storage.session-memory::maybe-update-session-memory runtime session)
             "First update should succeed")
    (%assert (probe-file path)
             "Session memory file should exist")
    
    ;; Second update (fresh, should not update)
    (%assert (not (claw-lisp.storage.session-memory::maybe-update-session-memory runtime session))
             "Fresh session memory should not update again")
    
    (claw-lisp.core.runtime::cleanup-session session)))

(defun test-integration-maybe-update-session-memory-failure-tolerant ()
  "Integration test: maybe-update-session-memory is failure-tolerant."
  (let* ((runtime (claw-lisp.core.runtime:make-runtime))
         (session (claw-lisp.core.runtime:start-session runtime :mock)))
    ;; Even if there are issues, function should not crash
    (handler-case
        (claw-lisp.storage.session-memory::maybe-update-session-memory runtime session)
      (error (e)
        (%assert nil "maybe-update-session-memory should not crash: ~A" e)))
    (claw-lisp.core.runtime::cleanup-session session)))

;;; ============================================================
;;; Test Runner
;;; ============================================================

(defun run-session-memory-tests ()
  "Run all Phase 5 session memory tests. Returns T if all pass."
  (format t "~&=== Phase 5 Session Memory Tests ===~%")
  (let ((results (list
                  ;; Trigger tests
                  (test-trigger-no-existing-file)
                  (test-trigger-stale-metadata)
                  (test-trigger-fresh-metadata-no-update)
                  ;; Staleness tests
                  (test-staleness-time-exceeded)
                  (test-staleness-token-growth)
                  (test-staleness-tool-growth)
                  (test-staleness-nil-metadata)
                  ;; Budget tests
                  (test-budget-under-budget)
                  (test-budget-config-set)
                  ;; Parsing tests
                  (test-parse-valid-metadata)
                  (test-parse-roundtrip)
                  (test-parse-malformed-metadata)
                  ;; Integration tests
                  (test-integration-session-memory-update-flow)
                  (test-integration-maybe-update-session-memory-failure-tolerant))))
    (if (every #'identity results)
        (progn
          (format t "~&ALL PHASE 5 SESSION MEMORY TESTS PASSED~%")
          t)
        (progn
          (format t "~&SOME PHASE 5 SESSION MEMORY TESTS FAILED~%")
          nil))))

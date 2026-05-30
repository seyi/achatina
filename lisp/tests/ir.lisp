(in-package #:claw-lisp.tests)

(defun %make-test-achatina-semantic-graph ()
  (claw-lisp.ir.schema:make-semantic-ir
   :id "workflow-achatina-1"
   :nodes (list (claw-lisp.ir.schema:make-ir-node
                 :id "node-b"
                 :kind :tool-call
                 :payload '(:tool "echo")
                 :metadata '(:priority 2))
                (claw-lisp.ir.schema:make-ir-node
                 :id "node-a"
                 :kind :input
                 :payload '(:text "hello")
                 :metadata '(:priority 1)))
   :edges (list (claw-lisp.ir.schema:make-ir-edge
                 :from-id "node-a"
                 :to-id "node-b"
                 :kind :flow)
                (claw-lisp.ir.schema:make-ir-edge
                 :from-id "node-a"
                 :to-id "node-b"
                 :kind :flow))
   :metadata '(:artifact-identity "workflow/support-triage"
               :pipeline-stage :semantic-ir
               :capabilities ("web-search" "model-call")
               :policies (:require-review :no-shell)
               :tools ("echo" "file-read")
               :memory-scopes (:session-memory :durable-memory))))

(defun test-achatina-surface-form-compiles-to-semantic-ir ()
  (let* ((form '(:workflow-id "workflow/local-proof"
                 :input-text "hello workflow"
                 :tool-name "echo"
                 :capabilities ("model-call")
                 :policies (:require-review)
                 :memory-scopes (:session-memory)))
         (graph (claw-lisp.ir.surface-form:compile-minimal-workflow-form form))
         (context (claw-lisp.ir.surface-form:make-workflow-form-context form))
         (edge-kinds (sort (copy-list (mapcar #'claw-lisp.ir.schema:ir-edge-kind
                                              (claw-lisp.ir.schema:ir-graph-edges graph)))
                           #'string<
                           :key #'symbol-name)))
    (%assert (string= "workflow/local-proof"
                      (claw-lisp.ir.schema:ir-graph-id graph))
             "Expected compiled graph id to match workflow id")
    (%assert (equal '(:control :flow) edge-kinds)
             "Expected surface-form compilation to emit both flow and control edges")
    (%assert (string= "workflow/local-proof"
                      (claw-lisp.ir.expander:ir-pass-context-identity context))
             "Expected workflow-form context identity to match workflow id")
    (%assert (equal '("echo")
                    (claw-lisp.ir.expander:ir-pass-context-tools context))
             "Expected workflow-form context to carry the authored tool list"))
  (format t "~&+ test-achatina-surface-form-compiles-to-semantic-ir passed~%")
  t)

(defun test-achatina-surface-form-runs-locally-through-cas-and-plan ()
  (%with-temp-cas-artifact-roots (cas-root ref-root)
    (let* ((temp-root (uiop:temporary-directory))
           (data-root (merge-pathnames
                       (format nil "claw-lisp-achatina-local-proof-~D-~D/"
                               (get-universal-time)
                               (get-internal-real-time))
                       temp-root))
           (transcripts-root (merge-pathnames "transcripts/" data-root))
           (artifacts-root (merge-pathnames "artifacts/" data-root))
           (memory-root (merge-pathnames "memory/" data-root))
           (config (make-test-runtime-config
                    :cas-objects-root (namestring cas-root)
                    :cas-ref-root (namestring ref-root)))
           runtime)
      (setf (claw-lisp.config:runtime-config-data-root config) (namestring data-root))
      (setf (claw-lisp.config:runtime-config-transcripts-root config) (namestring transcripts-root))
      (setf (claw-lisp.config:runtime-config-artifacts-root config) (namestring artifacts-root))
      (setf (claw-lisp.config:runtime-config-memory-root config) (namestring memory-root))
      (unwind-protect
           (progn
             (ensure-directories-exist (uiop:ensure-directory-pathname data-root))
             (ensure-directories-exist (uiop:ensure-directory-pathname transcripts-root))
             (ensure-directories-exist (uiop:ensure-directory-pathname artifacts-root))
             (ensure-directories-exist (uiop:ensure-directory-pathname memory-root))
             (setf runtime (make-runtime :config config))
             (let* ((result (claw-lisp.ir.local-execution:run-workflow-form-locally
                             runtime
                             '(:workflow-id "workflow/local-proof"
                               :input-text "hello workflow"
                               :tool-name "echo"
                               :capabilities ("model-call")
                               :policies (:require-review)
                               :memory-scopes (:session-memory))
                             :semantic-ref-name "ir/workflows/local-proof/semantic/current"
                             :execution-plan-ref-name "ir/workflows/local-proof/execution/current"
                             :execution-plan-manifest-ref-name "manifests/workflows/local-proof/execution/current"
                             :register-default-tools-p t))
                    (semantic-artifact (getf result :semantic-artifact))
                    (optimized-artifact (getf result :optimized-artifact))
                    (plan-bundle (getf result :execution-plan-bundle))
                    (plan-artifact (getf plan-bundle :graph))
                    (loaded-plan (getf result :loaded-plan))
                    (execution (getf result :execution))
                    (tool-results (getf execution :tool-results))
                    (first-result (first tool-results))
                    (plan-metadata (claw-lisp.ir.schema:ir-graph-metadata loaded-plan)))
               (%assert (claw-lisp.storage.cas:valid-versioned-hash-p
                         (claw-lisp.core.domain:artifact-cas-hash semantic-artifact))
                        "Expected semantic IR artifact to persist into CAS")
               (%assert (claw-lisp.storage.cas:valid-versioned-hash-p
                         (claw-lisp.core.domain:artifact-cas-hash plan-artifact))
                        "Expected execution-plan artifact to persist into CAS")
               (%assert (eq :execution-plan
                            (claw-lisp.ir.schema:ir-graph-node-type loaded-plan))
                        "Expected loaded plan to be execution-plan IR")
               (%assert (string=
                         (claw-lisp.core.domain:artifact-cas-hash optimized-artifact)
                         (getf plan-metadata :parent-ir-hash))
                        "Expected execution plan provenance to point at the optimized IR artifact")
               (%assert (= 1 (length tool-results))
                        "Expected local execution proof to produce exactly one tool result")
               (%assert (string= "hello workflow"
                                 (claw-lisp.core.domain:tool-result-content first-result))
                        "Expected local echo execution to round-trip the authored input text")))
        (when (and data-root (uiop:directory-exists-p data-root))
          (uiop:delete-directory-tree (uiop:ensure-directory-pathname data-root)
                                      :validate t)))))
  (format t "~&+ test-achatina-surface-form-runs-locally-through-cas-and-plan passed~%")
  t)

(defun test-achatina-human-review-ir-runs-locally-through-cas-and-plan ()
  (%with-temp-cas-artifact-roots (cas-root ref-root)
    (let* ((temp-root (uiop:temporary-directory))
           (data-root (merge-pathnames
                       (format nil "claw-lisp-achatina-human-review-proof-~D-~D/"
                               (get-universal-time)
                               (get-internal-real-time))
                       temp-root))
           (transcripts-root (merge-pathnames "transcripts/" data-root))
           (artifacts-root (merge-pathnames "artifacts/" data-root))
           (memory-root (merge-pathnames "memory/" data-root))
           (config (make-test-runtime-config
                    :cas-objects-root (namestring cas-root)
                    :cas-ref-root (namestring ref-root)))
           runtime)
      (setf (claw-lisp.config:runtime-config-data-root config) (namestring data-root))
      (setf (claw-lisp.config:runtime-config-transcripts-root config) (namestring transcripts-root))
      (setf (claw-lisp.config:runtime-config-artifacts-root config) (namestring artifacts-root))
      (setf (claw-lisp.config:runtime-config-memory-root config) (namestring memory-root))
      (unwind-protect
           (progn
             (ensure-directories-exist (uiop:ensure-directory-pathname data-root))
             (ensure-directories-exist (uiop:ensure-directory-pathname transcripts-root))
             (ensure-directories-exist (uiop:ensure-directory-pathname artifacts-root))
             (ensure-directories-exist (uiop:ensure-directory-pathname memory-root))
             (setf runtime (make-runtime :config config))
             (claw-lisp.core.runtime:register-default-tools runtime)
             (let* ((semantic-graph
                      (claw-lisp.ir.schema:make-semantic-ir
                       :id "workflow/local-human-review-proof"
                       :nodes (list (claw-lisp.ir.schema:make-ir-node
                                     :id "node-a"
                                     :kind :input
                                     :payload '(:text "hello workflow"))
                                    (claw-lisp.ir.schema:make-ir-node
                                     :id "node-b"
                                     :kind :human-review
                                     :payload '(:prompt "Approve this response?"
                                                :review-id "review-1"))
                                    (claw-lisp.ir.schema:make-ir-node
                                     :id "node-c"
                                     :kind :tool-call
                                     :payload '(:tool "echo")))
                       :edges (list (claw-lisp.ir.schema:make-ir-edge
                                     :from-id "node-a"
                                     :to-id "node-c"
                                     :kind :flow)
                                    (claw-lisp.ir.schema:make-ir-edge
                                     :from-id "node-a"
                                     :to-id "node-b"
                                     :kind :control)
                                    (claw-lisp.ir.schema:make-ir-edge
                                     :from-id "node-b"
                                     :to-id "node-c"
                                     :kind :control))
                       :metadata '(:artifact-identity "workflow/local-human-review-proof"
                                   :pipeline-stage :semantic-ir
                                   :tools ("echo")
                                   :capabilities ("model-call")
                                   :policies (:require-review))))
                    (context
                      (claw-lisp.ir.expander:make-ir-pass-context
                       :identity "workflow/local-human-review-proof"
                       :tools '("echo")
                       :capabilities '("model-call")
                       :policies '(:require-review)))
                    (semantic-artifact
                      (first (multiple-value-list
                              (claw-lisp.ir.cas-bridge:materialize-ir-to-cas
                               runtime
                               semantic-graph
                               :ref-name "ir/workflows/local-human-review-proof/semantic/current"
                               :metadata '(:pipeline-stage :semantic-ir)))))
                    (validated
                      (claw-lisp.ir.expander:prepare-validated-ir semantic-graph context))
                    (validated-artifact
                      (getf (claw-lisp.ir.expander:persist-ir-pass-result
                             runtime
                             validated
                             :parent-artifact semantic-artifact
                             :ref-name nil
                             :manifest-ref-name nil)
                            :graph))
                    (optimized-run
                      (claw-lisp.ir.optimize:optimize-validated-ir-pipeline
                       (claw-lisp.ir.expander:ir-pass-result-graph validated)
                       context
                       :runtime runtime
                       :initial-parent-artifact validated-artifact))
                    (optimized-result
                      (claw-lisp.ir.expander:ir-pipeline-run-final-result optimized-run))
                    (optimized-artifact
                      (claw-lisp.ir.expander:ir-pipeline-run-final-graph-artifact optimized-run))
                    (plan-result-and-bundle
                      (multiple-value-list
                       (claw-lisp.ir.execution-plan:persist-execution-plan
                        runtime
                        (claw-lisp.ir.expander:ir-pass-result-graph optimized-result)
                        context
                        :parent-artifact optimized-artifact
                        :ref-name "ir/workflows/local-human-review-proof/execution/current"
                        :manifest-ref-name "manifests/workflows/local-human-review-proof/execution/current")))
                    (plan-bundle (second plan-result-and-bundle))
                    (plan-artifact (getf plan-bundle :graph))
                    (loaded-plan
                      (claw-lisp.ir.cas-bridge:load-ir-from-cas
                       runtime
                       (claw-lisp.core.domain:artifact-cas-hash plan-artifact)))
                    (execution
                      (claw-lisp.ir.local-execution:execute-execution-plan-locally
                       runtime loaded-plan))
                    (node-results (getf execution :node-results))
                    (review-node
                      (find :plan-human-review
                            (claw-lisp.ir.schema:ir-graph-nodes loaded-plan)
                            :key #'claw-lisp.ir.schema:ir-node-kind))
                    (review-result (and review-node
                                        (gethash (claw-lisp.ir.schema:ir-node-id review-node)
                                                 node-results))))
               (%assert review-node
                        "Expected end-to-end semantic IR proof to include a :plan-human-review node")
               (%assert (getf review-result :human-review-required)
                        "Expected local execution to record a human-review marker result")
               (%assert (stringp (getf review-result :review-id))
                        "Expected human-review marker to carry a string review id")
               (%assert (stringp (getf review-result :prompt))
                        "Expected human-review marker to carry a string prompt")))
        (when (and data-root (uiop:directory-exists-p data-root))
          (uiop:delete-directory-tree (uiop:ensure-directory-pathname data-root)
                                      :validate t)))))
  (format t "~&+ test-achatina-human-review-ir-runs-locally-through-cas-and-plan passed~%")
  t)

(defmacro %with-temp-local-runtime ((runtime) &body body)
  `(let* ((temp-root (uiop:temporary-directory))
          (data-root (merge-pathnames
                      (format nil "claw-lisp-achatina-local-runtime-~D-~D/"
                              (get-universal-time)
                              (get-internal-real-time))
                      temp-root))
          (transcripts-root (merge-pathnames "transcripts/" data-root))
          (artifacts-root (merge-pathnames "artifacts/" data-root))
          (memory-root (merge-pathnames "memory/" data-root))
          (config (make-test-runtime-config))
          ,runtime)
     (setf (claw-lisp.config:runtime-config-data-root config) (namestring data-root))
     (setf (claw-lisp.config:runtime-config-transcripts-root config) (namestring transcripts-root))
     (setf (claw-lisp.config:runtime-config-artifacts-root config) (namestring artifacts-root))
     (setf (claw-lisp.config:runtime-config-memory-root config) (namestring memory-root))
     (unwind-protect
          (progn
            (ensure-directories-exist (uiop:ensure-directory-pathname data-root))
            (ensure-directories-exist (uiop:ensure-directory-pathname transcripts-root))
            (ensure-directories-exist (uiop:ensure-directory-pathname artifacts-root))
            (ensure-directories-exist (uiop:ensure-directory-pathname memory-root))
            (setf ,runtime (make-runtime :config config))
            ,@body)
       (when (uiop:directory-exists-p data-root)
         (uiop:delete-directory-tree (uiop:ensure-directory-pathname data-root)
                                     :validate t)))))

(defun test-achatina-local-execution-rejects-cycles ()
  (%with-temp-local-runtime (runtime)
    (let* ((plan (claw-lisp.ir.schema:make-execution-plan-ir
                  :id "workflow-local-cycle"
                  :nodes (list (claw-lisp.ir.schema:make-ir-node
                                :id "node-a"
                                :kind :plan-input
                                :payload '(:text "hello"))
                               (claw-lisp.ir.schema:make-ir-node
                                :id "node-b"
                                :kind :plan-tool-call
                                :payload '(:tool "echo")))
                  :edges (list (claw-lisp.ir.schema:make-ir-edge
                                :from-id "node-a"
                                :to-id "node-b"
                                :kind :plan-flow)
                               (claw-lisp.ir.schema:make-ir-edge
                                :from-id "node-b"
                                :to-id "node-a"
                                :kind :plan-control))
                  :metadata '(:artifact-identity "workflow/local-cycle"
                              :pipeline-stage :execution-plan)))
           (signaled nil))
      (claw-lisp.core.runtime:register-default-tools runtime)
      (handler-case
          (claw-lisp.ir.local-execution:execute-execution-plan-locally runtime plan)
        (claw-lisp.ir.conditions:ir-validation-error ()
          (setf signaled t)))
      (%assert signaled
               "Expected cyclic execution plans to signal ir-validation-error")))
  (format t "~&+ test-achatina-local-execution-rejects-cycles passed~%")
  t)

(defun test-achatina-local-execution-rejects-ambiguous-flow-input ()
  (%with-temp-local-runtime (runtime)
    (let* ((plan (claw-lisp.ir.schema:make-execution-plan-ir
                  :id "workflow-local-ambiguous"
                  :nodes (list (claw-lisp.ir.schema:make-ir-node
                                :id "node-a"
                                :kind :plan-input
                                :payload '(:text "hello"))
                               (claw-lisp.ir.schema:make-ir-node
                                :id "node-b"
                                :kind :plan-input
                                :payload '(:text "goodbye"))
                               (claw-lisp.ir.schema:make-ir-node
                                :id "node-c"
                                :kind :plan-tool-call
                                :payload '(:tool "echo")))
                  :edges (list (claw-lisp.ir.schema:make-ir-edge
                                :from-id "node-a"
                                :to-id "node-c"
                                :kind :plan-flow)
                               (claw-lisp.ir.schema:make-ir-edge
                                :from-id "node-b"
                                :to-id "node-c"
                                :kind :plan-flow))
                  :metadata '(:artifact-identity "workflow/local-ambiguous"
                              :pipeline-stage :execution-plan)))
           (signaled nil))
      (claw-lisp.core.runtime:register-default-tools runtime)
      (handler-case
          (claw-lisp.ir.local-execution:execute-execution-plan-locally runtime plan)
        (claw-lisp.ir.conditions:ir-validation-error ()
          (setf signaled t)))
      (%assert signaled
               "Expected ambiguous predecessor-derived tool input to signal ir-validation-error")))
  (format t "~&+ test-achatina-local-execution-rejects-ambiguous-flow-input passed~%")
  t)

(defun test-achatina-local-execution-rejects-missing-tool-input ()
  (%with-temp-local-runtime (runtime)
    (let* ((plan (claw-lisp.ir.schema:make-execution-plan-ir
                  :id "workflow-local-missing-input"
                  :nodes (list (claw-lisp.ir.schema:make-ir-node
                                :id "node-a"
                                :kind :plan-tool-call
                                :payload '(:tool "echo")))
                  :edges nil
                  :metadata '(:artifact-identity "workflow/local-missing-input"
                              :pipeline-stage :execution-plan)))
           (signaled nil))
      (claw-lisp.core.runtime:register-default-tools runtime)
      (handler-case
          (claw-lisp.ir.local-execution:execute-execution-plan-locally runtime plan)
        (claw-lisp.ir.conditions:ir-validation-error ()
          (setf signaled t)))
      (%assert signaled
               "Expected missing local execution tool input to signal ir-validation-error")))
  (format t "~&+ test-achatina-local-execution-rejects-missing-tool-input passed~%")
  t)

(defun test-achatina-local-execution-rejects-unsupported-node-kind ()
  (%with-temp-local-runtime (runtime)
    (let* ((plan (claw-lisp.ir.schema:make-execution-plan-ir
                  :id "workflow-local-unsupported-node"
                  :nodes (list (claw-lisp.ir.schema:make-ir-node
                                :id "node-a"
                                :kind :plan-hitl
                                :payload '(:prompt "review me")))
                  :edges nil
                  :metadata '(:artifact-identity "workflow/local-unsupported-node"
                              :pipeline-stage :execution-plan)))
           (signaled nil))
      (handler-case
          (claw-lisp.ir.local-execution:execute-execution-plan-locally runtime plan)
        (claw-lisp.ir.conditions:ir-validation-error ()
          (setf signaled t)))
      (%assert signaled
               "Expected unsupported execution-plan node kinds to signal ir-validation-error")))
  (format t "~&+ test-achatina-local-execution-rejects-unsupported-node-kind passed~%")
  t)

(defun test-achatina-local-execution-rejects-plan-model-call-nodes ()
  (%with-temp-local-runtime (runtime)
    (let* ((plan (claw-lisp.ir.schema:make-execution-plan-ir
                  :id "workflow-local-model-call"
                  :nodes (list (claw-lisp.ir.schema:make-ir-node
                                :id "node-a"
                                :kind :plan-model-call
                                :payload '(:provider "mock"
                                           :model "mock-model"
                                           :prompt "hello model")))
                  :edges nil
                  :metadata '(:artifact-identity "workflow/local-model-call"
                              :pipeline-stage :execution-plan)))
           (signaled nil))
      (handler-case
          (claw-lisp.ir.local-execution:execute-execution-plan-locally runtime plan)
        (claw-lisp.ir.conditions:ir-validation-error ()
          (setf signaled t)))
      (%assert signaled
               "Expected local execution to reject :plan-model-call until its runtime contract is implemented")))
  (format t "~&+ test-achatina-local-execution-rejects-plan-model-call-nodes passed~%")
  t)

(defun test-achatina-local-execution-rejects-plan-branch-nodes ()
  (%with-temp-local-runtime (runtime)
    (let* ((plan (claw-lisp.ir.schema:make-execution-plan-ir
                  :id "workflow-local-branch"
                  :nodes (list (claw-lisp.ir.schema:make-ir-node
                                :id "node-a"
                                :kind :plan-branch
                                :payload '(:condition "has-user-approval"
                                           :true-target "node-b"
                                           :false-target "node-c")))
                  :edges nil
                  :metadata '(:artifact-identity "workflow/local-branch"
                              :pipeline-stage :execution-plan)))
           (signaled nil))
      (handler-case
          (claw-lisp.ir.local-execution:execute-execution-plan-locally runtime plan)
        (claw-lisp.ir.conditions:ir-validation-error ()
          (setf signaled t)))
      (%assert signaled
               "Expected local execution to reject :plan-branch until its runtime contract is implemented")))
  (format t "~&+ test-achatina-local-execution-rejects-plan-branch-nodes passed~%")
  t)

(defun test-achatina-local-execution-supports-plan-human-review-nodes ()
  (%with-temp-local-runtime (runtime)
    (let* ((plan (claw-lisp.ir.schema:make-execution-plan-ir
                  :id "workflow-local-human-review"
                  :nodes (list (claw-lisp.ir.schema:make-ir-node
                                :id "node-a"
                                :kind :plan-human-review
                                :payload '(:prompt "Approve this response?"
                                           :review-id "review-1")))
                  :edges nil
                  :metadata '(:artifact-identity "workflow/local-human-review"
                              :pipeline-stage :execution-plan)))
           (execution (claw-lisp.ir.local-execution:execute-execution-plan-locally runtime plan))
           (node-results (getf execution :node-results))
           (review-result (gethash "node-a" node-results)))
      (%assert (getf review-result :human-review-required)
               "Expected local execution to mark human review as required")
      (%assert (equal "review-1" (getf review-result :review-id))
               "Expected local execution to retain the human-review id")
      (%assert (equal "Approve this response?" (getf review-result :prompt))
               "Expected local execution to retain the human-review prompt")
      (%assert (equal '(:prompt "Approve this response?"
                        :review-id "review-1")
                      (getf review-result :payload))
               "Expected local execution to retain the original human-review payload")))
  (format t "~&+ test-achatina-local-execution-supports-plan-human-review-nodes passed~%")
  t)

(defun test-achatina-local-execution-rejects-invalid-human-review-payload ()
  (%with-temp-local-runtime (runtime)
    (dolist (payload '((:prompt "Approve this response?")
                       (:review-id "review-1")
                       (:prompt 42 :review-id "review-1")
                       (:prompt "Approve this response?" :review-id 42)))
      (let* ((plan (claw-lisp.ir.schema:make-execution-plan-ir
                    :id "workflow-local-human-review-invalid"
                    :nodes (list (claw-lisp.ir.schema:make-ir-node
                                  :id "node-a"
                                  :kind :plan-human-review
                                  :payload payload))
                    :edges nil
                    :metadata '(:artifact-identity "workflow/local-human-review-invalid"
                                :pipeline-stage :execution-plan)))
             (signaled nil))
        (handler-case
            (claw-lisp.ir.local-execution:execute-execution-plan-locally runtime plan)
          (claw-lisp.ir.conditions:ir-validation-error ()
            (setf signaled t)))
        (%assert signaled
                 "Expected malformed human-review payload ~S to signal ir-validation-error"
                 payload))))
  (format t "~&+ test-achatina-local-execution-rejects-invalid-human-review-payload passed~%")
  t)

(defun test-achatina-local-execution-human-review-preserves-sequencing ()
  (%with-temp-local-runtime (runtime)
    (let* ((plan (claw-lisp.ir.schema:make-execution-plan-ir
                  :id "workflow-local-human-review-sequencing"
                  :nodes (list (claw-lisp.ir.schema:make-ir-node
                                :id "node-a"
                                :kind :plan-memory-write
                                :payload '(:scope :session-memory
                                           :content "before review"))
                               (claw-lisp.ir.schema:make-ir-node
                                :id "node-b"
                                :kind :plan-human-review
                                :payload '(:prompt "Approve checkpoint?"
                                           :review-id "review-boundary"))
                               (claw-lisp.ir.schema:make-ir-node
                                :id "node-c"
                                :kind :plan-memory-read
                                :payload '(:scope :session-memory
                                           :query "after review")))
                  :edges (list (claw-lisp.ir.schema:make-ir-edge
                                :from-id "node-a"
                                :to-id "node-b"
                                :kind :plan-control)
                               (claw-lisp.ir.schema:make-ir-edge
                                :from-id "node-b"
                                :to-id "node-c"
                                :kind :plan-control))
                  :metadata '(:artifact-identity "workflow/local-human-review-sequencing"
                              :pipeline-stage :execution-plan)))
           (execution (claw-lisp.ir.local-execution:execute-execution-plan-locally runtime plan))
           (node-results (getf execution :node-results))
           (review-result (gethash "node-b" node-results))
           (read-result (gethash "node-c" node-results)))
      (%assert (equal "review-boundary" (getf review-result :review-id))
               "Expected the human-review marker to retain its review identifier")
      (%assert (equal '(:scope :session-memory
                        :query "after review"
                        :content "before review")
                      read-result)
               "Expected :plan-human-review to preserve execution ordering without altering later local node results")))
  (format t "~&+ test-achatina-local-execution-human-review-preserves-sequencing passed~%")
  t)

(defun test-achatina-local-execution-supports-plan-child-agent-nodes ()
  (%with-temp-local-runtime (runtime)
    (let* ((plan (claw-lisp.ir.schema:make-execution-plan-ir
                  :id "workflow-local-child-agent"
                  :nodes (list (claw-lisp.ir.schema:make-ir-node
                                :id "node-a"
                                :kind :plan-child-agent
                                :payload '(:agent "worker"
                                           :prompt "Investigate this issue"
                                           :handoff "summary")))
                  :edges nil
                  :metadata '(:artifact-identity "workflow/local-child-agent"
                              :pipeline-stage :execution-plan)))
           (execution (claw-lisp.ir.local-execution:execute-execution-plan-locally runtime plan))
           (node-results (getf execution :node-results))
           (child-result (gethash "node-a" node-results)))
      (%assert (getf child-result :child-agent-required)
               "Expected local execution to mark child-agent execution as required")
      (%assert (equal "worker" (getf child-result :agent))
               "Expected local execution to retain the child-agent name")
      (%assert (equal "Investigate this issue" (getf child-result :prompt))
               "Expected local execution to retain the child-agent prompt")
      (%assert (equal "summary" (getf child-result :handoff))
               "Expected local execution to retain the child-agent handoff")
      (%assert (equal '(:agent "worker"
                        :prompt "Investigate this issue"
                        :handoff "summary")
                      (getf child-result :payload))
               "Expected local execution to retain the original child-agent payload")))
  (format t "~&+ test-achatina-local-execution-supports-plan-child-agent-nodes passed~%")
  t)

(defun test-achatina-local-execution-supports-plan-child-agent-without-handoff ()
  (%with-temp-local-runtime (runtime)
    (let* ((plan (claw-lisp.ir.schema:make-execution-plan-ir
                  :id "workflow-local-child-agent-no-handoff"
                  :nodes (list (claw-lisp.ir.schema:make-ir-node
                                :id "node-a"
                                :kind :plan-child-agent
                                :payload '(:agent "worker"
                                           :prompt "Investigate this issue")))
                  :edges nil
                  :metadata '(:artifact-identity "workflow/local-child-agent-no-handoff"
                              :pipeline-stage :execution-plan)))
           (execution (claw-lisp.ir.local-execution:execute-execution-plan-locally runtime plan))
           (node-results (getf execution :node-results))
           (child-result (gethash "node-a" node-results)))
      (%assert (getf child-result :child-agent-required)
               "Expected local execution to mark child-agent execution as required")
      (%assert (equal "worker" (getf child-result :agent))
               "Expected local execution to retain the child-agent name")
      (%assert (equal "Investigate this issue" (getf child-result :prompt))
               "Expected local execution to retain the child-agent prompt")
      (%assert (null (getf child-result :handoff))
               "Expected local execution to allow omitted :handoff and surface it as NIL")))
  (format t "~&+ test-achatina-local-execution-supports-plan-child-agent-without-handoff passed~%")
  t)

(defun test-achatina-local-execution-rejects-invalid-child-agent-payload ()
  (%with-temp-local-runtime (runtime)
    (dolist (payload '((:prompt "Investigate this issue")
                       (:agent "worker")
                       (:agent 42 :prompt "Investigate this issue")
                       (:agent "worker" :prompt 42)
                       (:agent "worker" :prompt "Investigate this issue" :handoff 42)))
      (let* ((plan (claw-lisp.ir.schema:make-execution-plan-ir
                    :id "workflow-local-child-agent-invalid"
                    :nodes (list (claw-lisp.ir.schema:make-ir-node
                                  :id "node-a"
                                  :kind :plan-child-agent
                                  :payload payload))
                    :edges nil
                    :metadata '(:artifact-identity "workflow/local-child-agent-invalid"
                                :pipeline-stage :execution-plan)))
             (signaled nil))
        (handler-case
            (claw-lisp.ir.local-execution:execute-execution-plan-locally runtime plan)
          (claw-lisp.ir.conditions:ir-validation-error ()
            (setf signaled t)))
        (%assert signaled
                 "Expected malformed child-agent payload ~S to signal ir-validation-error"
                 payload))))
  (format t "~&+ test-achatina-local-execution-rejects-invalid-child-agent-payload passed~%")
  t)

(defun test-achatina-local-execution-child-agent-preserves-sequencing ()
  (%with-temp-local-runtime (runtime)
    (let* ((plan (claw-lisp.ir.schema:make-execution-plan-ir
                  :id "workflow-local-child-agent-sequencing"
                  :nodes (list (claw-lisp.ir.schema:make-ir-node
                                :id "node-a"
                                :kind :plan-memory-write
                                :payload '(:scope :session-memory
                                           :content "before child"))
                               (claw-lisp.ir.schema:make-ir-node
                                :id "node-b"
                                :kind :plan-child-agent
                                :payload '(:agent "worker"
                                           :prompt "Investigate this issue"
                                           :handoff "summary"))
                               (claw-lisp.ir.schema:make-ir-node
                                :id "node-c"
                                :kind :plan-memory-read
                                :payload '(:scope :session-memory
                                           :query "after child")))
                  :edges (list (claw-lisp.ir.schema:make-ir-edge
                                :from-id "node-a"
                                :to-id "node-b"
                                :kind :plan-control)
                               (claw-lisp.ir.schema:make-ir-edge
                                :from-id "node-b"
                                :to-id "node-c"
                                :kind :plan-control))
                  :metadata '(:artifact-identity "workflow/local-child-agent-sequencing"
                              :pipeline-stage :execution-plan)))
           (execution (claw-lisp.ir.local-execution:execute-execution-plan-locally runtime plan))
           (node-results (getf execution :node-results))
           (child-result (gethash "node-b" node-results))
           (read-result (gethash "node-c" node-results)))
      (%assert (equal "worker" (getf child-result :agent))
               "Expected the child-agent marker to retain its agent name")
      (%assert (equal '(:scope :session-memory
                        :query "after child"
                        :content "before child")
                      read-result)
               "Expected :plan-child-agent to preserve execution ordering without altering later local node results")))
  (format t "~&+ test-achatina-local-execution-child-agent-preserves-sequencing passed~%")
  t)

(defun test-achatina-local-execution-supports-plan-await-nodes ()
  (%with-temp-local-runtime (runtime)
    (let* ((plan (claw-lisp.ir.schema:make-execution-plan-ir
                  :id "workflow-local-await"
                  :nodes (list (claw-lisp.ir.schema:make-ir-node
                                :id "node-a"
                                :kind :plan-await
                                :payload '(:checkpoint "before-review")))
                  :edges nil
                  :metadata '(:artifact-identity "workflow/local-await"
                              :pipeline-stage :execution-plan)))
           (execution (claw-lisp.ir.local-execution:execute-execution-plan-locally runtime plan))
           (node-results (getf execution :node-results))
           (await-result (gethash "node-a" node-results)))
      (%assert (equal '(:awaited t
                        :checkpoint "before-review"
                        :payload (:checkpoint "before-review"))
                      await-result)
               "Expected local execution to treat :plan-await as an explicit checkpoint marker")))
  (format t "~&+ test-achatina-local-execution-supports-plan-await-nodes passed~%")
  t)

(defun test-achatina-local-execution-await-preserves-sequencing ()
  (%with-temp-local-runtime (runtime)
    (let* ((plan (claw-lisp.ir.schema:make-execution-plan-ir
                  :id "workflow-local-await-sequencing"
                  :nodes (list (claw-lisp.ir.schema:make-ir-node
                                :id "node-a"
                                :kind :plan-memory-write
                                :payload '(:scope :session-memory
                                           :content "before checkpoint"))
                               (claw-lisp.ir.schema:make-ir-node
                                :id "node-b"
                                :kind :plan-await
                                :payload '(:checkpoint "boundary"))
                               (claw-lisp.ir.schema:make-ir-node
                                :id "node-c"
                                :kind :plan-memory-read
                                :payload '(:scope :session-memory
                                           :query "after checkpoint")))
                  :edges (list (claw-lisp.ir.schema:make-ir-edge
                                :from-id "node-a"
                                :to-id "node-b"
                                :kind :plan-control)
                               (claw-lisp.ir.schema:make-ir-edge
                                :from-id "node-b"
                                :to-id "node-c"
                                :kind :plan-control))
                  :metadata '(:artifact-identity "workflow/local-await-sequencing"
                              :pipeline-stage :execution-plan)))
           (execution (claw-lisp.ir.local-execution:execute-execution-plan-locally runtime plan))
           (node-results (getf execution :node-results))
           (await-result (gethash "node-b" node-results))
           (read-result (gethash "node-c" node-results)))
      (%assert (equal "boundary" (getf await-result :checkpoint))
               "Expected the await marker to retain its checkpoint label")
      (%assert (equal '(:scope :session-memory
                        :query "after checkpoint"
                        :content "before checkpoint")
                      read-result)
               "Expected :plan-await to preserve execution ordering without altering later local node results")))
  (format t "~&+ test-achatina-local-execution-await-preserves-sequencing passed~%")
  t)

(defun test-achatina-local-execution-supports-session-memory-read-nodes ()
  (%with-temp-local-runtime (runtime)
    (let* ((plan (claw-lisp.ir.schema:make-execution-plan-ir
                  :id "workflow-local-memory-read"
                  :nodes (list (claw-lisp.ir.schema:make-ir-node
                                :id "node-a"
                                :kind :plan-memory-read
                                :payload '(:scope :session-memory
                                           :query "recent findings")))
                  :edges nil
                  :metadata '(:artifact-identity "workflow/local-memory-read"
                              :pipeline-stage :execution-plan)))
           (session (claw-lisp.core.runtime:start-session
                     runtime
                     :session-id "workflow-local-memory-read-session"))
           (memory-path (claw-lisp.core.runtime:session-memory-path-for-session runtime session))
           execution
           node-results
           memory-result)
      (ensure-directories-exist memory-path)
      (with-open-file (stream memory-path
                              :direction :output
                              :if-exists :supersede
                              :if-does-not-exist :create)
        (write-string "Recent findings: memory retrieval works." stream))
      (setf execution
            (claw-lisp.ir.local-execution:execute-execution-plan-locally
             runtime plan :session session))
      (setf node-results (getf execution :node-results))
      (setf memory-result (gethash "node-a" node-results))
      (%assert (equal '(:scope :session-memory
                        :query "recent findings"
                        :content "Recent findings: memory retrieval works.")
                      memory-result)
               "Expected local execution to load session-memory text into the memory-read node result")))
  (format t "~&+ test-achatina-local-execution-supports-session-memory-read-nodes passed~%")
  t)

(defun test-achatina-local-execution-rejects-unsupported-memory-read-scope ()
  (%with-temp-local-runtime (runtime)
    (let* ((plan (claw-lisp.ir.schema:make-execution-plan-ir
                  :id "workflow-local-memory-read-unsupported"
                  :nodes (list (claw-lisp.ir.schema:make-ir-node
                                :id "node-a"
                                :kind :plan-memory-read
                                :payload '(:scope :durable-memory
                                           :query "recent findings")))
                  :edges nil
                  :metadata '(:artifact-identity "workflow/local-memory-read-unsupported"
                              :pipeline-stage :execution-plan)))
           (reason nil))
      (handler-case
          (claw-lisp.ir.local-execution:execute-execution-plan-locally runtime plan)
        (claw-lisp.ir.conditions:ir-validation-error (condition)
          (setf reason (claw-lisp.ir.conditions:ir-validation-error-reason condition))))
      (%assert reason
               "Expected unsupported memory-read scopes to signal ir-validation-error")
      (%assert (search "not yet supported" reason :test #'char-equal)
               "Expected unsupported memory-read scope rejection to explain that the runtime contract is not implemented yet")))
  (format t "~&+ test-achatina-local-execution-rejects-unsupported-memory-read-scope passed~%")
  t)

(defun test-achatina-local-execution-supports-session-memory-write-nodes ()
  (%with-temp-local-runtime (runtime)
    (let* ((plan (claw-lisp.ir.schema:make-execution-plan-ir
                  :id "workflow-local-memory-write"
                  :nodes (list (claw-lisp.ir.schema:make-ir-node
                                :id "node-a"
                                :kind :plan-memory-write
                                :payload '(:scope :session-memory
                                           :content "store this insight"))
                               (claw-lisp.ir.schema:make-ir-node
                                :id "node-b"
                                :kind :plan-memory-read
                                :payload '(:scope :session-memory
                                           :query "confirm write")))
                  :edges (list (claw-lisp.ir.schema:make-ir-edge
                                :from-id "node-a"
                                :to-id "node-b"
                                :kind :plan-control))
                  :metadata '(:artifact-identity "workflow/local-memory-write"
                              :pipeline-stage :execution-plan)))
           (session (claw-lisp.core.runtime:start-session
                     runtime
                     :session-id "workflow-local-memory-write-session"))
           (memory-path (claw-lisp.core.runtime:session-memory-path-for-session runtime session))
           execution
           node-results
           write-result
           read-result)
      (setf execution
            (claw-lisp.ir.local-execution:execute-execution-plan-locally
             runtime plan :session session))
      (setf node-results (getf execution :node-results))
      (setf write-result (gethash "node-a" node-results))
      (setf read-result (gethash "node-b" node-results))
      (%assert (equal :session-memory (getf write-result :scope))
               "Expected memory-write result to preserve the supported session-memory scope")
      (%assert (string= "store this insight" (getf write-result :content))
               "Expected memory-write result to preserve the written content")
      (%assert (probe-file (getf write-result :path))
               "Expected memory-write to return a path that exists on disk")
      (%assert (string= (namestring memory-path) (getf write-result :path))
               "Expected memory-write result path to match the runtime session-memory path")
      (with-open-file (stream memory-path)
        (%assert (string= "store this insight"
                          (let ((text (make-string (file-length stream))))
                            (read-sequence text stream)
                            text))
                 "Expected the session-memory file to contain the written content"))
      (%assert (equal '(:scope :session-memory
                        :query "confirm write"
                        :content "store this insight")
                      read-result)
               "Expected a subsequent local memory-read to observe the written session-memory content")))
  (format t "~&+ test-achatina-local-execution-supports-session-memory-write-nodes passed~%")
  t)

(defun test-achatina-local-execution-memory-write-overwrites-prior-content ()
  (%with-temp-local-runtime (runtime)
    (let* ((plan (claw-lisp.ir.schema:make-execution-plan-ir
                  :id "workflow-local-memory-write-overwrite"
                  :nodes (list (claw-lisp.ir.schema:make-ir-node
                                :id "node-a"
                                :kind :plan-memory-write
                                :payload '(:scope :session-memory
                                           :content "first"))
                               (claw-lisp.ir.schema:make-ir-node
                                :id "node-b"
                                :kind :plan-memory-write
                                :payload '(:scope :session-memory
                                           :content "second"))
                               (claw-lisp.ir.schema:make-ir-node
                                :id "node-c"
                                :kind :plan-memory-read
                                :payload '(:scope :session-memory
                                           :query "confirm overwrite")))
                  :edges (list (claw-lisp.ir.schema:make-ir-edge
                                :from-id "node-a"
                                :to-id "node-b"
                                :kind :plan-control)
                               (claw-lisp.ir.schema:make-ir-edge
                                :from-id "node-b"
                                :to-id "node-c"
                                :kind :plan-control))
                  :metadata '(:artifact-identity "workflow/local-memory-write-overwrite"
                              :pipeline-stage :execution-plan)))
           (session (claw-lisp.core.runtime:start-session
                     runtime
                     :session-id "workflow-local-memory-write-overwrite-session"))
           execution
           node-results
           second-write
           final-read)
      (setf execution
            (claw-lisp.ir.local-execution:execute-execution-plan-locally
             runtime plan :session session))
      (setf node-results (getf execution :node-results))
      (setf second-write (gethash "node-b" node-results))
      (setf final-read (gethash "node-c" node-results))
      (%assert (string= "second" (getf second-write :content))
               "Expected the second memory-write to preserve its written content")
      (%assert (equal '(:scope :session-memory
                        :query "confirm overwrite"
                        :content "second")
                      final-read)
               "Expected later session-memory writes to overwrite prior session-memory content")))
  (format t "~&+ test-achatina-local-execution-memory-write-overwrites-prior-content passed~%")
  t)

(defun test-achatina-local-execution-memory-write-supports-implicit-session ()
  (%with-temp-local-runtime (runtime)
    (let* ((plan (claw-lisp.ir.schema:make-execution-plan-ir
                  :id "workflow/local-memory-write-implicit"
                  :nodes (list (claw-lisp.ir.schema:make-ir-node
                                :id "node-a"
                                :kind :plan-memory-write
                                :payload '(:scope :session-memory
                                           :content "implicit session write"))
                               (claw-lisp.ir.schema:make-ir-node
                                :id "node-b"
                                :kind :plan-memory-read
                                :payload '(:scope :session-memory
                                           :query "confirm implicit")))
                  :edges (list (claw-lisp.ir.schema:make-ir-edge
                                :from-id "node-a"
                                :to-id "node-b"
                                :kind :plan-control))
                  :metadata '(:artifact-identity "workflow/local-memory-write-implicit"
                              :pipeline-stage :execution-plan)))
           (execution (claw-lisp.ir.local-execution:execute-execution-plan-locally runtime plan))
           (session (getf execution :session))
           (node-results (getf execution :node-results))
           (read-result (gethash "node-b" node-results)))
      (%assert session
               "Expected local execution to create a session when none is provided")
      (%assert (search "workflow-local-memory-write-implicit-local"
                       (claw-lisp.core.domain:agent-session-id session)
                       :test #'char-equal)
               "Expected the implicit local session id to be derived from the workflow identity")
      (%assert (equal '(:scope :session-memory
                        :query "confirm implicit"
                        :content "implicit session write")
                      read-result)
               "Expected implicit-session execution to support session-memory write then readback")))
  (format t "~&+ test-achatina-local-execution-memory-write-supports-implicit-session passed~%")
  t)

(defun test-achatina-local-execution-rejects-unsupported-memory-write-scope ()
  (%with-temp-local-runtime (runtime)
    (let* ((plan (claw-lisp.ir.schema:make-execution-plan-ir
                  :id "workflow-local-memory-write-unsupported"
                  :nodes (list (claw-lisp.ir.schema:make-ir-node
                                :id "node-a"
                                :kind :plan-memory-write
                                :payload '(:scope :durable-memory
                                           :content "store this insight")))
                  :edges nil
                  :metadata '(:artifact-identity "workflow/local-memory-write-unsupported"
                              :pipeline-stage :execution-plan)))
           (reason nil))
      (handler-case
          (claw-lisp.ir.local-execution:execute-execution-plan-locally runtime plan)
        (claw-lisp.ir.conditions:ir-validation-error (condition)
          (setf reason (claw-lisp.ir.conditions:ir-validation-error-reason condition))))
      (%assert reason
               "Expected unsupported memory-write scopes to signal ir-validation-error")
      (%assert (search "not yet supported" reason :test #'char-equal)
               "Expected unsupported memory-write scope rejection to explain that the runtime contract is not implemented yet")))
  (format t "~&+ test-achatina-local-execution-rejects-unsupported-memory-write-scope passed~%")
  t)

(defun test-achatina-local-execution-rejects-invalid-memory-write-content ()
  (%with-temp-local-runtime (runtime)
    (let* ((missing-content-plan
             (claw-lisp.ir.schema:make-execution-plan-ir
              :id "workflow-local-memory-write-missing-content"
              :nodes (list (claw-lisp.ir.schema:make-ir-node
                            :id "node-a"
                            :kind :plan-memory-write
                            :payload '(:scope :session-memory)))
              :edges nil
              :metadata '(:artifact-identity "workflow/local-memory-write-missing-content"
                          :pipeline-stage :execution-plan)))
           (non-string-content-plan
             (claw-lisp.ir.schema:make-execution-plan-ir
              :id "workflow-local-memory-write-non-string-content"
              :nodes (list (claw-lisp.ir.schema:make-ir-node
                            :id "node-a"
                            :kind :plan-memory-write
                            :payload '(:scope :session-memory
                                       :content (:not "a string"))))
              :edges nil
              :metadata '(:artifact-identity "workflow/local-memory-write-non-string-content"
                          :pipeline-stage :execution-plan)))
           (missing-reason nil)
           (non-string-reason nil))
      (handler-case
          (claw-lisp.ir.local-execution:execute-execution-plan-locally runtime missing-content-plan)
        (claw-lisp.ir.conditions:ir-validation-error (condition)
          (setf missing-reason (claw-lisp.ir.conditions:ir-validation-error-reason condition))))
      (handler-case
          (claw-lisp.ir.local-execution:execute-execution-plan-locally runtime non-string-content-plan)
        (claw-lisp.ir.conditions:ir-validation-error (condition)
          (setf non-string-reason (claw-lisp.ir.conditions:ir-validation-error-reason condition))))
      (%assert missing-reason
               "Expected missing memory-write content to signal ir-validation-error")
      (%assert (search "requires string :content" missing-reason :test #'char-equal)
               "Expected missing memory-write content rejection to mention string :content")
      (%assert non-string-reason
               "Expected non-string memory-write content to signal ir-validation-error")
      (%assert (search "requires string :content" non-string-reason :test #'char-equal)
               "Expected non-string memory-write content rejection to mention string :content")))
  (format t "~&+ test-achatina-local-execution-rejects-invalid-memory-write-content passed~%")
  t)

(defun test-achatina-local-execution-supports-plan-side-effect-nodes ()
  (%with-temp-local-runtime (runtime)
    (let* ((plan (claw-lisp.ir.schema:make-execution-plan-ir
                  :id "workflow-local-side-effect"
                  :nodes (list (claw-lisp.ir.schema:make-ir-node
                                :id "node-a"
                                :kind :plan-side-effect
                                :payload '(:effect :notify
                                           :channel "ops"
                                           :message "workflow completed")))
                  :edges nil
                  :metadata '(:artifact-identity "workflow/local-side-effect"
                              :pipeline-stage :execution-plan)))
           (execution (claw-lisp.ir.local-execution:execute-execution-plan-locally runtime plan))
           (node-results (getf execution :node-results))
           (side-effect-result (gethash "node-a" node-results)))
      (%assert (getf side-effect-result :side-effect-required)
               "Expected local execution to mark side-effect execution as required")
      (%assert (eq :notify (getf side-effect-result :effect))
               "Expected local execution to retain the side-effect kind")
      (%assert (equal '(:effect :notify
                        :channel "ops"
                        :message "workflow completed")
                      (getf side-effect-result :payload))
               "Expected local execution to retain the original side-effect payload")))
  (format t "~&+ test-achatina-local-execution-supports-plan-side-effect-nodes passed~%")
  t)

(defun test-achatina-local-execution-rejects-invalid-side-effect-payload ()
  (%with-temp-local-runtime (runtime)
    (dolist (payload '((:channel "ops" :message "workflow completed")
                       (:effect "notify" :channel "ops")
                       (:effect 42 :channel "ops")
                       (:effect nil :channel "ops")))
      (let* ((plan (claw-lisp.ir.schema:make-execution-plan-ir
                    :id "workflow-local-side-effect-invalid"
                    :nodes (list (claw-lisp.ir.schema:make-ir-node
                                  :id "node-a"
                                  :kind :plan-side-effect
                                  :payload payload))
                    :edges nil
                    :metadata '(:artifact-identity "workflow/local-side-effect-invalid"
                                :pipeline-stage :execution-plan)))
             (signaled nil))
        (handler-case
            (claw-lisp.ir.local-execution:execute-execution-plan-locally runtime plan)
          (claw-lisp.ir.conditions:ir-validation-error ()
            (setf signaled t)))
        (%assert signaled
                 "Expected side-effect payload ~S with missing or non-keyword :effect to signal ir-validation-error"
                 payload))))
  (format t "~&+ test-achatina-local-execution-rejects-invalid-side-effect-payload passed~%")
  t)

(defun test-achatina-local-execution-side-effect-preserves-sequencing ()
  (%with-temp-local-runtime (runtime)
    (let* ((plan (claw-lisp.ir.schema:make-execution-plan-ir
                  :id "workflow-local-side-effect-sequencing"
                  :nodes (list (claw-lisp.ir.schema:make-ir-node
                                :id "node-a"
                                :kind :plan-memory-write
                                :payload '(:scope :session-memory
                                           :content "before side effect"))
                               (claw-lisp.ir.schema:make-ir-node
                                :id "node-b"
                                :kind :plan-side-effect
                                :payload '(:effect :notify
                                           :channel "ops"
                                           :message "workflow completed"))
                               (claw-lisp.ir.schema:make-ir-node
                                :id "node-c"
                                :kind :plan-memory-read
                                :payload '(:scope :session-memory
                                           :query "after side effect")))
                  :edges (list (claw-lisp.ir.schema:make-ir-edge
                                :from-id "node-a"
                                :to-id "node-b"
                                :kind :plan-control)
                               (claw-lisp.ir.schema:make-ir-edge
                                :from-id "node-b"
                                :to-id "node-c"
                                :kind :plan-control))
                  :metadata '(:artifact-identity "workflow/local-side-effect-sequencing"
                              :pipeline-stage :execution-plan)))
           (execution (claw-lisp.ir.local-execution:execute-execution-plan-locally runtime plan))
           (node-results (getf execution :node-results))
           (side-effect-result (gethash "node-b" node-results))
           (read-result (gethash "node-c" node-results)))
      (%assert (eq :notify (getf side-effect-result :effect))
               "Expected the side-effect marker to retain its effect kind")
      (%assert (equal '(:scope :session-memory
                        :query "after side effect"
                        :content "before side effect")
                      read-result)
               "Expected :plan-side-effect to preserve execution ordering without altering later local node results")))
  (format t "~&+ test-achatina-local-execution-side-effect-preserves-sequencing passed~%")
  t)

(defun test-achatina-local-execution-errors-on-unregistered-tool ()
  (%with-temp-local-runtime (runtime)
    (let* ((plan (claw-lisp.ir.schema:make-execution-plan-ir
                  :id "workflow-local-missing-tool"
                  :nodes (list (claw-lisp.ir.schema:make-ir-node
                                :id "node-a"
                                :kind :plan-input
                                :payload '(:text "hello"))
                               (claw-lisp.ir.schema:make-ir-node
                                :id "node-b"
                                :kind :plan-tool-call
                                :payload '(:tool "missing-tool")))
                  :edges (list (claw-lisp.ir.schema:make-ir-edge
                                :from-id "node-a"
                                :to-id "node-b"
                                :kind :plan-flow))
                  :metadata '(:artifact-identity "workflow/local-missing-tool"
                              :pipeline-stage :execution-plan)))
           (signaled nil))
      (handler-case
          (claw-lisp.ir.local-execution:execute-execution-plan-locally runtime plan)
        (error ()
          (setf signaled t)))
      (%assert signaled
               "Expected unknown local execution tools to signal an error")))
  (format t "~&+ test-achatina-local-execution-errors-on-unregistered-tool passed~%")
  t)

(defun test-achatina-execution-plan-lowering-prefers-context-parent-over-preparation ()
  (let* ((preparation-parent (claw-lisp.storage.cas:cas-hash "preparation-parent"))
         (context-parent (claw-lisp.storage.cas:cas-hash "context-parent"))
         (graph (claw-lisp.ir.schema:make-optimized-ir
                 :id "workflow-lowering-parent-precedence"
                 :nodes (list (claw-lisp.ir.schema:make-ir-node
                               :id "node-a"
                               :kind :input
                               :payload '(:text "hello"))
                              (claw-lisp.ir.schema:make-ir-node
                               :id "node-b"
                               :kind :tool-call
                               :payload '(:tool "echo")))
                 :edges (list (claw-lisp.ir.schema:make-ir-edge
                               :from-id "node-a"
                               :to-id "node-b"
                               :kind :flow))
                 :metadata (list :artifact-identity "workflow/lowering-parent-precedence"
                                 :pipeline-stage :optimized-ir
                                 :parent-ir-hash preparation-parent
                                 :parent-ir-ref-name "ir/workflows/old-parent/current")))
         (context (claw-lisp.ir.expander:make-ir-pass-context
                   :identity "workflow/lowering-parent-precedence"
                   :parent-ir-hash context-parent
                   :parent-ir-ref-name "ir/workflows/new-parent/current"))
         (plan (claw-lisp.ir.expander:ir-pass-result-graph
                (claw-lisp.ir.execution-plan:lower-to-execution-plan graph context)))
         (metadata (claw-lisp.ir.schema:ir-graph-metadata plan)))
    (%assert (string= context-parent (getf metadata :parent-ir-hash))
             "Expected lowering context parent hash to win over preparation metadata parent hash")
    (%assert (string= "ir/workflows/new-parent/current"
                      (getf metadata :parent-ir-ref-name))
             "Expected lowering context parent ref name to win over preparation metadata parent ref name"))
  (format t "~&+ test-achatina-execution-plan-lowering-prefers-context-parent-over-preparation passed~%")
  t)

(defun test-ir-canonical-serialization-is-stable ()
  (let* ((artifact (claw-lisp.core.domain:make-artifact
                    :id "artifact-1"
                    :kind :tool-result
                    :cas-hash "sha256:1111111111111111111111111111111111111111111111111111111111111111"
                    :cas-type :markdown
                    :cas-ref-name "tool-results/demo/ref"
                    :metadata '(:bytes 12 :tool-name "echo")))
         (left (claw-lisp.ir.schema:make-semantic-ir
                :id "workflow-1"
                :nodes (list (claw-lisp.ir.schema:make-ir-node
                              :id "node-a"
                              :kind :tool-call
                              :payload (list :tool-name "echo"
                                             :artifact artifact
                                             :options (list :b 2 :a 1))
                              :metadata '(:priority 1 :stage :build)))
                :edges (list (claw-lisp.ir.schema:make-ir-edge
                              :from-id "node-a"
                              :to-id "node-b"
                              :kind :data))
                :metadata '(:owner "ops" :labels (:z 3 :a 1))))
         (right (claw-lisp.ir.schema:make-semantic-ir
                 :id "workflow-1"
                 :nodes (list (claw-lisp.ir.schema:make-ir-node
                               :id "node-a"
                               :kind :tool-call
                               :payload (list :options (list :a 1 :b 2)
                                              :artifact artifact
                                              :tool-name "echo")
                               :metadata '(:stage :build :priority 1)))
                 :edges (list (claw-lisp.ir.schema:make-ir-edge
                               :from-id "node-a"
                               :to-id "node-b"
                               :kind :data))
                 :metadata '(:labels (:a 1 :z 3) :owner "ops")))
         (left-string (claw-lisp.ir.schema:ir-object->canonical-string left))
         (right-string (claw-lisp.ir.schema:ir-object->canonical-string right)))
    (%assert (string= left-string right-string)
             "Expected canonical IR strings to match, got ~S vs ~S"
             left-string right-string)
    (%assert (string= (claw-lisp.storage.cas:cas-hash left-string)
                      (claw-lisp.storage.cas:cas-hash right-string))
             "Expected canonical IR hashes to match"))
  (format t "~&+ test-ir-canonical-serialization-is-stable passed~%")
  t)

(defun test-ir-materialize-and-load-roundtrip ()
  (%with-temp-cas-artifact-roots (cas-root ref-root)
    (let* ((config (make-test-runtime-config
                    :cas-objects-root (namestring cas-root)
                    :cas-ref-root (namestring ref-root)))
           (runtime (make-runtime :config config))
           (ir (claw-lisp.ir.schema:make-optimized-ir
                :id "wf-roundtrip"
                :nodes (list (claw-lisp.ir.schema:make-ir-node
                              :id "n1"
                              :kind :model-call
                              :payload '(:provider "mock" :model "mock-model")
                              :metadata '(:attempt 1)))
                :edges (list (claw-lisp.ir.schema:make-ir-edge
                              :from-id "n1"
                              :to-id "n2"
                              :kind :control))
                :metadata '(:compiler-version "0.1"))))
      (multiple-value-bind (artifact info)
          (claw-lisp.ir.cas-bridge:materialize-ir-to-cas
           runtime ir
           :ref-name "ir/workflows/wf-roundtrip/latest"
           :metadata '(:scope :test))
        (%assert (claw-lisp.storage.cas:valid-versioned-hash-p
                  (claw-lisp.core.domain:artifact-cas-hash artifact))
                 "Expected IR artifact to have a valid CAS hash")
        (%assert (equal "wf-roundtrip" (claw-lisp.core.domain:artifact-id artifact))
                 "Expected artifact id to track IR id")
        (%assert (equal :optimized-ir (claw-lisp.core.domain:artifact-kind artifact))
                 "Expected optimized-ir artifact kind, got ~S"
                 (claw-lisp.core.domain:artifact-kind artifact))
        (%assert (equal (claw-lisp.ir.schema:ir-object->canonical-sexp ir)
                        (claw-lisp.ir.schema:ir-object->canonical-sexp
                         (claw-lisp.ir.cas-bridge:load-ir-from-cas
                          runtime
                          (claw-lisp.core.domain:artifact-cas-hash artifact))))
                 "Expected IR to round-trip through direct CAS load")
        (%assert (equal (claw-lisp.ir.schema:ir-object->canonical-sexp ir)
                        (claw-lisp.ir.schema:ir-object->canonical-sexp
                         (claw-lisp.ir.cas-bridge:resolve-ir-from-cas runtime artifact)))
                 "Expected IR to round-trip through artifact resolution")
        (%assert (equal (claw-lisp.storage.cas-ref:resolve-cas-ref
                         ref-root cas-root
                         "ir/workflows/wf-roundtrip/latest"
                         :require-object-p t)
                        (claw-lisp.core.domain:artifact-cas-hash artifact))
                 "Expected IR ref to resolve to the stored hash")
        (%assert (> (getf info :bytes) 0)
                 "Expected materialization summary to record payload bytes"))))
  (format t "~&+ test-ir-materialize-and-load-roundtrip passed~%")
  t)

(defun test-ir-resolve-ref-roundtrip ()
  (%with-temp-cas-artifact-roots (cas-root ref-root)
    (let* ((config (make-test-runtime-config
                    :cas-objects-root (namestring cas-root)
                    :cas-ref-root (namestring ref-root)))
           (runtime (make-runtime :config config))
           (ir (claw-lisp.ir.schema:make-execution-plan-ir
                :id "wf-ref"
                :nodes (list (claw-lisp.ir.schema:make-ir-node
                              :id "node-x"
                              :kind :checkpoint
                              :payload '(:checkpoint "before-hitl")))
                :metadata '(:plan-version 1))))
      (claw-lisp.ir.cas-bridge:materialize-ir-to-cas
       runtime ir
       :ref-name "ir/workflows/wf-ref/current")
      (%assert (equal (claw-lisp.ir.schema:ir-object->canonical-sexp ir)
                      (claw-lisp.ir.schema:ir-object->canonical-sexp
                       (claw-lisp.ir.cas-bridge:resolve-ir-ref
                        runtime
                        "ir/workflows/wf-ref/current")))
               "Expected IR ref resolution to return the stored IR object")))
  (format t "~&+ test-ir-resolve-ref-roundtrip passed~%")
  t)

(defun test-ir-load-rejects-unsupported-version ()
  (%with-temp-cas-artifact-roots (cas-root ref-root)
    (let* ((config (make-test-runtime-config
                    :cas-objects-root (namestring cas-root)
                    :cas-ref-root (namestring ref-root)))
           (runtime (make-runtime :config config))
           (raw "(:IR-GRAPH :EDGES NIL :ID \"wf-bad\" :IR-VERSION \"2099.9\" :METADATA NIL :NODE-TYPE :SEMANTIC-IR :NODES NIL)")
           (cas-hash (claw-lisp.storage.cas:cas-put cas-root raw))
           (signaled nil))
      (handler-case
          (claw-lisp.ir.cas-bridge:load-ir-from-cas runtime cas-hash)
        (claw-lisp.ir.conditions:ir-version-mismatch-error ()
          (setf signaled t)))
      (%assert signaled
               "Expected unsupported IR version to signal ir-version-mismatch-error")))
  (format t "~&+ test-ir-load-rejects-unsupported-version passed~%")
  t)

(defun test-ir-serialization-rejects-non-keyword-symbols ()
  (let ((signaled nil))
    (handler-case
        (claw-lisp.ir.schema:ir-object->canonical-string
         (claw-lisp.ir.schema:make-semantic-ir
          :id "wf-bad-key"
          :metadata (list 'claw-lisp.tests::bad-key "value")))
      (claw-lisp.ir.conditions:ir-serialization-error ()
        (setf signaled t)))
    (%assert signaled
             "Expected non-keyword plist keys to signal ir-serialization-error"))
  (format t "~&+ test-ir-serialization-rejects-non-keyword-symbols passed~%")
  t)

(defun test-ir-serialization-rejects-unsupported-types ()
  (let ((signaled nil))
    (handler-case
        (claw-lisp.ir.schema:ir-object->canonical-string
         (claw-lisp.ir.schema:make-semantic-ir
          :id "wf-bad-type"
          :metadata (list :bad (make-hash-table))))
      (claw-lisp.ir.conditions:ir-serialization-error ()
        (setf signaled t)))
    (%assert signaled
             "Expected unsupported IR payload types to signal ir-serialization-error"))
  (format t "~&+ test-ir-serialization-rejects-unsupported-types passed~%")
  t)

(defun test-ir-materialize-rejects-invalid-ref-name ()
  (%with-temp-cas-artifact-roots (cas-root ref-root)
    (let* ((config (make-test-runtime-config
                    :cas-objects-root (namestring cas-root)
                    :cas-ref-root (namestring ref-root)))
           (runtime (make-runtime :config config))
           (ir (claw-lisp.ir.schema:make-semantic-ir :id "wf-ref-invalid"))
           (signaled nil))
      (handler-case
          (claw-lisp.ir.cas-bridge:materialize-ir-to-cas
           runtime ir :ref-name "/bad/ref")
        (claw-lisp.ir.conditions:ir-storage-error ()
          (setf signaled t)))
      (%assert signaled
               "Expected invalid IR ref names to signal ir-storage-error")))
  (format t "~&+ test-ir-materialize-rejects-invalid-ref-name passed~%")
  t)

(defun test-ir-materialize-requires-cas-root ()
  (let* ((config (make-test-runtime-config
                  :cas-objects-root ""
                  :cas-ref-root ""))
         (runtime (make-runtime :config config))
         (ir (claw-lisp.ir.schema:make-semantic-ir :id "wf-no-cas"))
         (signaled nil))
    (handler-case
        (claw-lisp.ir.cas-bridge:materialize-ir-to-cas runtime ir)
      (claw-lisp.ir.conditions:ir-storage-error ()
        (setf signaled t)))
    (%assert signaled
             "Expected missing CAS root to signal ir-storage-error"))
  (format t "~&+ test-ir-materialize-requires-cas-root passed~%")
  t)

(defun test-ir-load-requires-cas-root ()
  (let* ((config (make-test-runtime-config
                  :cas-objects-root ""
                  :cas-ref-root ""))
         (runtime (make-runtime :config config))
         (signaled nil))
    (handler-case
        (claw-lisp.ir.cas-bridge:load-ir-from-cas
         runtime
         "sha256:1111111111111111111111111111111111111111111111111111111111111111")
      (claw-lisp.ir.conditions:ir-storage-error ()
        (setf signaled t)))
    (%assert signaled
             "Expected missing CAS root on load to signal ir-storage-error"))
  (format t "~&+ test-ir-load-requires-cas-root passed~%")
  t)

(defun test-ir-load-missing-object-signals-storage-error ()
  (%with-temp-cas-artifact-roots (cas-root ref-root)
    (let* ((config (make-test-runtime-config
                    :cas-objects-root (namestring cas-root)
                    :cas-ref-root ""))
           (runtime (make-runtime :config config))
           (signaled nil))
      (handler-case
          (claw-lisp.ir.cas-bridge:load-ir-from-cas
           runtime
           "sha256:1111111111111111111111111111111111111111111111111111111111111111")
        (claw-lisp.ir.conditions:ir-storage-error ()
          (setf signaled t)))
      (%assert signaled
               "Expected missing IR CAS object to signal ir-storage-error")))
  (format t "~&+ test-ir-load-missing-object-signals-storage-error passed~%")
  t)

(defun test-ir-load-rejects-malformed-sexp ()
  (%with-temp-cas-artifact-roots (cas-root ref-root)
    (let* ((config (make-test-runtime-config
                    :cas-objects-root (namestring cas-root)
                    :cas-ref-root ""))
           (runtime (make-runtime :config config))
           (cas-hash (claw-lisp.storage.cas:cas-put cas-root "(:IR-GRAPH :ID \"broken\""))
           (signaled nil))
      (handler-case
          (claw-lisp.ir.cas-bridge:load-ir-from-cas runtime cas-hash)
        (claw-lisp.ir.conditions:ir-deserialization-error ()
          (setf signaled t)))
      (%assert signaled
               "Expected malformed IR payloads to signal ir-deserialization-error")))
  (format t "~&+ test-ir-load-rejects-malformed-sexp passed~%")
  t)

(defun test-ir-load-rejects-invalid-top-level-form ()
  (%with-temp-cas-artifact-roots (cas-root ref-root)
    (let* ((config (make-test-runtime-config
                    :cas-objects-root (namestring cas-root)
                    :cas-ref-root ""))
           (runtime (make-runtime :config config))
           (cas-hash (claw-lisp.storage.cas:cas-put cas-root "42"))
           (signaled nil))
      (handler-case
          (claw-lisp.ir.cas-bridge:load-ir-from-cas runtime cas-hash)
        (claw-lisp.ir.conditions:ir-deserialization-error ()
          (setf signaled t)))
      (%assert signaled
               "Expected invalid top-level IR forms to signal ir-deserialization-error")))
  (format t "~&+ test-ir-load-rejects-invalid-top-level-form passed~%")
  t)

(defun test-ir-load-rejects-non-keyword-plist-keys ()
  (%with-temp-cas-artifact-roots (cas-root ref-root)
    (let* ((config (make-test-runtime-config
                    :cas-objects-root (namestring cas-root)
                    :cas-ref-root ""))
           (runtime (make-runtime :config config))
           (cas-hash (claw-lisp.storage.cas:cas-put
                      cas-root
                      "(:IR-GRAPH :EDGES NIL :ID \"wf-bad-keys\" :IR-VERSION \"2026.1\" :METADATA (CLAW-LISP.TESTS::BAD-KEY \"value\") :NODE-TYPE :SEMANTIC-IR :NODES NIL)"))
           (signaled nil))
      (handler-case
          (claw-lisp.ir.cas-bridge:load-ir-from-cas runtime cas-hash)
        (claw-lisp.ir.conditions:ir-deserialization-error ()
          (setf signaled t)))
      (%assert signaled
               "Expected non-keyword plist keys in loaded IR to signal ir-deserialization-error")))
  (format t "~&+ test-ir-load-rejects-non-keyword-plist-keys passed~%")
  t)

(defun test-ir-load-rejects-unknown-top-level-tag ()
  (%with-temp-cas-artifact-roots (cas-root ref-root)
    (let* ((config (make-test-runtime-config
                    :cas-objects-root (namestring cas-root)
                    :cas-ref-root ""))
           (runtime (make-runtime :config config))
           (cas-hash (claw-lisp.storage.cas:cas-put
                      cas-root
                      "(:FOO :ID \"wf-unknown-tag\")"))
           (signaled nil))
      (handler-case
          (claw-lisp.ir.cas-bridge:load-ir-from-cas runtime cas-hash)
        (claw-lisp.ir.conditions:ir-deserialization-error ()
          (setf signaled t)))
      (%assert signaled
               "Expected unknown top-level IR tags to signal ir-deserialization-error")))
  (format t "~&+ test-ir-load-rejects-unknown-top-level-tag passed~%")
  t)

(defun test-ir-load-rejects-non-keyword-top-level-head ()
  (%with-temp-cas-artifact-roots (cas-root ref-root)
    (let* ((config (make-test-runtime-config
                    :cas-objects-root (namestring cas-root)
                    :cas-ref-root ""))
           (runtime (make-runtime :config config))
           (cas-hash (claw-lisp.storage.cas:cas-put
                      cas-root
                      "(CLAW-LISP.TESTS::NOT-A-KEYWORD :ID \"wf-bad-head\")"))
           (signaled nil))
      (handler-case
          (claw-lisp.ir.cas-bridge:load-ir-from-cas runtime cas-hash)
        (claw-lisp.ir.conditions:ir-deserialization-error ()
          (setf signaled t)))
      (%assert signaled
               "Expected non-keyword top-level IR heads to signal ir-deserialization-error")))
  (format t "~&+ test-ir-load-rejects-non-keyword-top-level-head passed~%")
  t)

(defun test-ir-deserialization-report-summarizes-payload ()
  (let* ((large-payload (format nil "(:IR-GRAPH :ID ~S)"
                                (make-string 400 :initial-element #\x)))
         (condition (make-condition 'claw-lisp.ir.conditions:ir-deserialization-error
                                    :payload large-payload
                                    :reason "bad payload"))
         (rendered (princ-to-string condition)))
    (%assert (< (length rendered) 260)
             "Expected summarized deserialization report, got length ~D"
             (length rendered))
    (%assert (search "bad payload" rendered)
             "Expected deserialization reason to remain visible in report")
    (%assert (search "..." rendered)
             "Expected summarized report to indicate truncation"))
  (format t "~&+ test-ir-deserialization-report-summarizes-payload passed~%")
  t)

(defun test-achatina-ir-validation-succeeds ()
  (let ((graph (%make-test-achatina-semantic-graph)))
    (%assert (eq graph (claw-lisp.ir.validate:validate-achatina-ir-graph
                        graph
                        :expected-stage :semantic-ir))
             "Expected valid Achatina graph to validate successfully"))
  (format t "~&+ test-achatina-ir-validation-succeeds passed~%")
  t)

(defun test-achatina-ir-validation-rejects-missing-identity ()
  (let* ((graph (%make-test-achatina-semantic-graph))
         (broken (claw-lisp.ir.schema:make-semantic-ir
                  :id (claw-lisp.ir.schema:ir-graph-id graph)
                  :nodes (claw-lisp.ir.schema:ir-graph-nodes graph)
                  :edges (claw-lisp.ir.schema:ir-graph-edges graph)
                  :metadata '(:pipeline-stage :semantic-ir)))
         (signaled nil))
    (handler-case
        (claw-lisp.ir.validate:validate-achatina-ir-graph broken)
      (claw-lisp.ir.conditions:ir-validation-error ()
        (setf signaled t)))
    (%assert signaled
             "Expected missing :artifact-identity to signal ir-validation-error"))
  (format t "~&+ test-achatina-ir-validation-rejects-missing-identity passed~%")
  t)

(defun test-achatina-ir-validation-rejects-stage-node-type-mismatch ()
  (let* ((graph (%make-test-achatina-semantic-graph))
         (broken (claw-lisp.ir.schema:make-semantic-ir
                  :id (claw-lisp.ir.schema:ir-graph-id graph)
                  :nodes (claw-lisp.ir.schema:ir-graph-nodes graph)
                  :edges (claw-lisp.ir.schema:ir-graph-edges graph)
                  :metadata '(:artifact-identity "workflow/support-triage"
                              :pipeline-stage :optimized-ir)))
         (signaled nil))
    (handler-case
        (claw-lisp.ir.validate:validate-achatina-ir-graph broken)
      (claw-lisp.ir.conditions:ir-validation-error ()
        (setf signaled t)))
    (%assert signaled
             "Expected stage/node-type mismatch to signal ir-validation-error"))
  (format t "~&+ test-achatina-ir-validation-rejects-stage-node-type-mismatch passed~%")
  t)

(defun test-achatina-ir-validation-rejects-invalid-parent-hash ()
  (let* ((graph (%make-test-achatina-semantic-graph))
         (broken (claw-lisp.ir.schema:make-semantic-ir
                  :id (claw-lisp.ir.schema:ir-graph-id graph)
                  :nodes (claw-lisp.ir.schema:ir-graph-nodes graph)
                  :edges (claw-lisp.ir.schema:ir-graph-edges graph)
                  :metadata '(:artifact-identity "workflow/support-triage"
                              :pipeline-stage :semantic-ir
                              :parent-ir-hash "not-a-hash")))
         (signaled nil))
    (handler-case
        (claw-lisp.ir.validate:validate-achatina-ir-graph broken)
      (claw-lisp.ir.conditions:ir-validation-error ()
        (setf signaled t)))
    (%assert signaled
             "Expected malformed :parent-ir-hash to signal ir-validation-error"))
  (format t "~&+ test-achatina-ir-validation-rejects-invalid-parent-hash passed~%")
  t)

(defun test-achatina-ir-validation-rejects-invalid-governance-metadata ()
  (let* ((graph (%make-test-achatina-semantic-graph))
         (bad-capabilities (claw-lisp.ir.schema:make-semantic-ir
                            :id (claw-lisp.ir.schema:ir-graph-id graph)
                            :nodes (claw-lisp.ir.schema:ir-graph-nodes graph)
                            :edges (claw-lisp.ir.schema:ir-graph-edges graph)
                            :metadata '(:artifact-identity "workflow/support-triage"
                                        :pipeline-stage :semantic-ir
                                        :capabilities ("web-search" :model-call))))
         (bad-policies (claw-lisp.ir.schema:make-semantic-ir
                        :id (claw-lisp.ir.schema:ir-graph-id graph)
                        :nodes (claw-lisp.ir.schema:ir-graph-nodes graph)
                        :edges (claw-lisp.ir.schema:ir-graph-edges graph)
                        :metadata '(:artifact-identity "workflow/support-triage"
                                    :pipeline-stage :semantic-ir
                                    :policies (:no-shell "require-review"))))
         (bad-tools (claw-lisp.ir.schema:make-semantic-ir
                     :id (claw-lisp.ir.schema:ir-graph-id graph)
                     :nodes (claw-lisp.ir.schema:ir-graph-nodes graph)
                     :edges (claw-lisp.ir.schema:ir-graph-edges graph)
                     :metadata '(:artifact-identity "workflow/support-triage"
                                 :pipeline-stage :semantic-ir
                                 :tools ("echo" :file-read))))
         (bad-memory-scopes (claw-lisp.ir.schema:make-semantic-ir
                             :id (claw-lisp.ir.schema:ir-graph-id graph)
                             :nodes (claw-lisp.ir.schema:ir-graph-nodes graph)
                             :edges (claw-lisp.ir.schema:ir-graph-edges graph)
                             :metadata '(:artifact-identity "workflow/support-triage"
                                         :pipeline-stage :semantic-ir
                                         :memory-scopes (:session-memory "durable-memory"))))
         (capabilities-signaled nil)
         (policies-signaled nil)
         (tools-signaled nil)
         (memory-scopes-signaled nil))
    (handler-case
        (claw-lisp.ir.validate:validate-achatina-ir-graph bad-capabilities)
      (claw-lisp.ir.conditions:ir-validation-error ()
        (setf capabilities-signaled t)))
    (handler-case
        (claw-lisp.ir.validate:validate-achatina-ir-graph bad-policies)
      (claw-lisp.ir.conditions:ir-validation-error ()
        (setf policies-signaled t)))
    (handler-case
        (claw-lisp.ir.validate:validate-achatina-ir-graph bad-tools)
      (claw-lisp.ir.conditions:ir-validation-error ()
        (setf tools-signaled t)))
    (handler-case
        (claw-lisp.ir.validate:validate-achatina-ir-graph bad-memory-scopes)
      (claw-lisp.ir.conditions:ir-validation-error ()
        (setf memory-scopes-signaled t)))
    (%assert capabilities-signaled
             "Expected non-string capabilities to signal ir-validation-error")
    (%assert policies-signaled
             "Expected non-keyword policies to signal ir-validation-error")
    (%assert tools-signaled
             "Expected non-string tools to signal ir-validation-error")
    (%assert memory-scopes-signaled
             "Expected non-keyword memory scopes to signal ir-validation-error"))
  (format t "~&+ test-achatina-ir-validation-rejects-invalid-governance-metadata passed~%")
  t)

(defun test-achatina-ir-pass-sequencing-and-normalization ()
  (let* ((graph (%make-test-achatina-semantic-graph))
         (context (claw-lisp.ir.expander:make-ir-pass-context
                   :identity "workflow/support-triage"
                   :capabilities '("model-call" "web-search")
                   :policies '(:no-shell :require-review)
                   :tools '("file-read" "echo")
                   :memory-scopes '(:durable-memory :session-memory)
                   :parent-ir-hash
                   "sha256:1111111111111111111111111111111111111111111111111111111111111111"
                   :metadata '(:owner "ops")))
         (validated (claw-lisp.ir.expander:prepare-validated-ir graph context))
         (optimized (claw-lisp.ir.optimize:optimize-validated-ir
                     (claw-lisp.ir.expander:ir-pass-result-graph validated)
                     context))
         (optimized-graph (claw-lisp.ir.expander:ir-pass-result-graph optimized))
         (metadata (claw-lisp.ir.schema:ir-graph-metadata optimized-graph)))
    (%assert (eq :semantic-ir
                 (claw-lisp.ir.expander:ir-pass-result-input-stage validated))
             "Expected validated pass to start from semantic stage")
    (%assert (eq :validated-ir
                 (claw-lisp.ir.expander:ir-pass-result-output-stage validated))
             "Expected validated pass to emit validated stage")
    (%assert (eq :optimized-ir
                 (claw-lisp.ir.expander:ir-pass-result-output-stage optimized))
             "Expected optimize pass to emit optimized stage")
    (%assert (eq :optimized-ir (getf metadata :pipeline-stage))
             "Expected optimized graph metadata to record optimized stage")
    (%assert (= 1 (length (claw-lisp.ir.schema:ir-graph-edges optimized-graph)))
             "Expected duplicate edges to be removed by normalization")
    (%assert (equal '("model-call" "web-search") (getf metadata :capabilities))
             "Expected capabilities to be normalized into stable order")
    (%assert (equal '(:no-shell :require-review) (getf metadata :policies))
             "Expected policies to be normalized into stable order")
    (%assert (equal '("echo" "file-read") (getf metadata :tools))
             "Expected tools to be normalized into stable order")
    (%assert (equal '(:durable-memory :session-memory) (getf metadata :memory-scopes))
             "Expected memory scopes to be normalized into stable order")
    (%assert (equal "node-a"
                    (claw-lisp.ir.schema:ir-node-id
                     (first (claw-lisp.ir.schema:ir-graph-nodes optimized-graph))))
             "Expected nodes to be normalized into stable order")
    (let* ((*print-case* :downcase)
           (*print-pretty* t)
           (*print-level* 1)
           (*print-length* 1)
           (re-optimized (claw-lisp.ir.optimize:optimize-validated-ir
                          (claw-lisp.ir.expander:ir-pass-result-graph validated)
                          context))
           (re-optimized-graph (claw-lisp.ir.expander:ir-pass-result-graph re-optimized)))
      (%assert (equal (claw-lisp.ir.schema:ir-object->canonical-sexp optimized-graph)
                      (claw-lisp.ir.schema:ir-object->canonical-sexp re-optimized-graph))
               "Expected normalization to be stable across printer variable changes")))
  (format t "~&+ test-achatina-ir-pass-sequencing-and-normalization passed~%")
  t)

(defun test-achatina-ir-pass-persistence-links-parent-and-child ()
  (%with-temp-cas-artifact-roots (cas-root ref-root)
    (let* ((config (make-test-runtime-config
                    :cas-objects-root (namestring cas-root)
                    :cas-ref-root (namestring ref-root)))
           (runtime (make-runtime :config config))
           (graph (%make-test-achatina-semantic-graph))
           (context (claw-lisp.ir.expander:make-ir-pass-context
                     :identity "workflow/support-triage"
                     :capabilities '("web-search" "model-call")
                     :policies '(:require-review :no-shell)))
           (validated (claw-lisp.ir.expander:prepare-validated-ir graph context)))
      (multiple-value-bind (parent-artifact parent-info)
          (claw-lisp.ir.cas-bridge:materialize-ir-to-cas
           runtime graph
           :ref-name "ir/workflows/support-triage/semantic/current"
           :metadata '(:pipeline-stage :semantic-ir))
        (declare (ignore parent-info))
        (let* ((opt-context (claw-lisp.ir.expander:make-ir-pass-context
                             :identity "workflow/support-triage"
                             :capabilities '("web-search" "model-call")
                             :policies '(:require-review :no-shell)
                             :parent-ir-hash (claw-lisp.core.domain:artifact-cas-hash parent-artifact)
                             :parent-ir-ref-name "ir/workflows/support-triage/semantic/current"))
               (optimized (claw-lisp.ir.optimize:optimize-validated-ir
                           (claw-lisp.ir.expander:ir-pass-result-graph validated)
                           opt-context))
               (persisted (claw-lisp.ir.expander:persist-ir-pass-result
                           runtime optimized
                           :parent-artifact parent-artifact
                           :ref-name "ir/workflows/support-triage/optimized/current"
                           :manifest-ref-name "manifests/ir/workflows/support-triage/optimized/current"
                           :metadata '(:workflow-id "workflow/support-triage")))
               (optimized-artifact (getf persisted :graph))
               (manifest-artifact (getf persisted :manifest))
               (manifest (claw-lisp.cas.manifest:load-manifest
                          cas-root
                          (claw-lisp.core.domain:artifact-cas-hash manifest-artifact)))
               (loaded (claw-lisp.ir.cas-bridge:load-ir-from-cas
                        runtime
                        (claw-lisp.core.domain:artifact-cas-hash optimized-artifact))))
          (%assert (eq :optimized-ir (claw-lisp.core.domain:artifact-kind optimized-artifact))
                   "Expected persisted pass output artifact kind to be optimized-ir")
          (%assert (eq :optimized-ir (claw-lisp.ir.schema:ir-graph-node-type loaded))
                   "Expected persisted pass output to reload as optimized-ir graph")
          (%assert (equal '(:optimized-ir :semantic-ir)
                          (sort (copy-list
                                 (mapcar #'claw-lisp.cas.manifest:manifest-entry-role
                                         (claw-lisp.cas.manifest:manifest-entries manifest)))
                                #'string<
                                :key #'symbol-name))
                   "Expected pass manifest to link parent semantic IR and child optimized IR")
          (%assert (string= (claw-lisp.core.domain:artifact-cas-hash parent-artifact)
                            (getf (claw-lisp.ir.schema:ir-graph-metadata loaded) :parent-ir-hash))
                   "Expected persisted optimized graph to retain parent hash provenance")))))
  (format t "~&+ test-achatina-ir-pass-persistence-links-parent-and-child passed~%")
  t)

(defun test-achatina-ir-pass-persistence-allows-nil-parent ()
  (%with-temp-cas-artifact-roots (cas-root ref-root)
    (let* ((config (make-test-runtime-config
                    :cas-objects-root (namestring cas-root)
                    :cas-ref-root (namestring ref-root)))
           (runtime (make-runtime :config config))
           (graph (%make-test-achatina-semantic-graph))
           (context (claw-lisp.ir.expander:make-ir-pass-context
                     :identity "workflow/support-triage"))
           (validated (claw-lisp.ir.expander:prepare-validated-ir graph context))
           (optimized (claw-lisp.ir.optimize:optimize-validated-ir
                       (claw-lisp.ir.expander:ir-pass-result-graph validated)
                       context))
           (persisted (claw-lisp.ir.expander:persist-ir-pass-result
                       runtime optimized
                       :ref-name "ir/workflows/support-triage/optimized/no-parent"))
           (manifest-artifact (getf persisted :manifest))
           (manifest (claw-lisp.cas.manifest:load-manifest
                      cas-root
                      (claw-lisp.core.domain:artifact-cas-hash manifest-artifact))))
      (%assert (= 1 (length (claw-lisp.cas.manifest:manifest-entries manifest)))
               "Expected manifest without parent artifact to contain only child IR entry")))
  (format t "~&+ test-achatina-ir-pass-persistence-allows-nil-parent passed~%")
  t)

(defun test-achatina-ir-optimization-pipeline-chains-provenance ()
  (%with-temp-cas-artifact-roots (cas-root ref-root)
    (let* ((config (make-test-runtime-config
                    :cas-objects-root (namestring cas-root)
                    :cas-ref-root (namestring ref-root)))
           (runtime (make-runtime :config config))
           (graph (%make-test-achatina-semantic-graph))
           (context (claw-lisp.ir.expander:make-ir-pass-context
                     :identity "workflow/support-triage"
                     :capabilities '("web-search" "model-call")
                     :policies '(:require-review :no-shell)))
           (validated (claw-lisp.ir.expander:prepare-validated-ir graph context)))
      (multiple-value-bind (semantic-artifact semantic-info)
          (claw-lisp.ir.cas-bridge:materialize-ir-to-cas
           runtime graph
           :ref-name "ir/workflows/support-triage/semantic/current"
           :metadata '(:pipeline-stage :semantic-ir))
        (declare (ignore semantic-info))
        (let* ((pipeline (claw-lisp.ir.optimize:optimize-validated-ir-pipeline
                          (claw-lisp.ir.expander:ir-pass-result-graph validated)
                          context
                          :runtime runtime
                          :initial-parent-artifact semantic-artifact
                          :ref-prefix "ir/workflows/support-triage/optimized"
                          :manifest-ref-prefix "manifests/ir/workflows/support-triage/optimized"
                          :metadata '(:workflow-id "workflow/support-triage")))
               (results (claw-lisp.ir.expander:ir-pipeline-run-results pipeline))
               (persisted (claw-lisp.ir.expander:ir-pipeline-run-persisted-results pipeline))
               (first-result (first results))
               (second-result (second results))
               (first-persisted (first persisted))
               (second-persisted (second persisted))
               (first-artifact (getf first-persisted :graph))
               (second-artifact (getf second-persisted :graph))
               (first-manifest (claw-lisp.cas.manifest:load-manifest
                                cas-root
                                (claw-lisp.core.domain:artifact-cas-hash
                                 (getf first-persisted :manifest))))
               (second-manifest (claw-lisp.cas.manifest:load-manifest
                                 cas-root
                                 (claw-lisp.core.domain:artifact-cas-hash
                                  (getf second-persisted :manifest))))
               (first-loaded (claw-lisp.ir.cas-bridge:load-ir-from-cas
                              runtime
                              (claw-lisp.core.domain:artifact-cas-hash first-artifact)))
               (second-loaded (claw-lisp.ir.cas-bridge:load-ir-from-cas
                               runtime
                               (claw-lisp.core.domain:artifact-cas-hash second-artifact)))
               (first-entries (claw-lisp.cas.manifest:manifest-entries first-manifest))
               (second-entries (claw-lisp.cas.manifest:manifest-entries second-manifest))
               (second-entry-hashes (mapcar #'claw-lisp.cas.manifest:manifest-entry-cas-hash
                                            second-entries)))
          (%assert (= 2 (length results))
                   "Expected two optimization pipeline pass results")
          (%assert (= 2 (length persisted))
                   "Expected two persisted optimization pipeline results")
          (%assert (eq :normalize-graph-layout
                       (claw-lisp.ir.expander:ir-pass-result-pass-name first-result))
                   "Expected first optimization pass to normalize graph layout")
          (%assert (eq :dedupe-graph-edges
                       (claw-lisp.ir.expander:ir-pass-result-pass-name second-result))
                   "Expected second optimization pass to dedupe edges")
          (%assert (eq :validate-graph
                       (getf (claw-lisp.ir.schema:ir-graph-metadata first-loaded)
                             :source-pass-name))
                   "Expected first optimized artifact to record validated source pass")
          (%assert (eq :normalize-graph-layout
                       (getf (claw-lisp.ir.schema:ir-graph-metadata second-loaded)
                             :source-pass-name))
                   "Expected second optimized artifact to record first optimization pass as source")
          (%assert (eq :validated-ir
                       (getf (claw-lisp.ir.schema:ir-graph-metadata first-loaded)
                             :source-stage))
                   "Expected first optimized artifact to record validated source stage")
          (%assert (eq :optimized-ir
                       (getf (claw-lisp.ir.schema:ir-graph-metadata second-loaded)
                             :source-stage))
                   "Expected second optimized artifact to record optimized source stage")
          (%assert (string= (claw-lisp.core.domain:artifact-cas-hash semantic-artifact)
                            (getf (claw-lisp.ir.schema:ir-graph-metadata first-loaded)
                                  :parent-ir-hash))
                   "Expected first optimization artifact to point at semantic parent hash")
          (%assert (string= (claw-lisp.core.domain:artifact-cas-hash first-artifact)
                            (getf (claw-lisp.ir.schema:ir-graph-metadata second-loaded)
                                  :parent-ir-hash))
                   "Expected second optimization artifact to point at first optimization artifact hash")
          (%assert (= 2 (length first-entries))
                   "Expected first optimization manifest to have parent and child entries")
          (%assert (= 2 (length second-entries))
                   "Expected second optimization manifest to have parent and child entries")
          (%assert (string= (claw-lisp.core.domain:artifact-cas-hash semantic-artifact)
                            (claw-lisp.cas.manifest:manifest-entry-cas-hash (first first-entries)))
                   "Expected first manifest parent entry to point at semantic artifact")
          (%assert (string= (claw-lisp.core.domain:artifact-cas-hash first-artifact)
                            (claw-lisp.cas.manifest:manifest-entry-cas-hash (second first-entries)))
                   "Expected first manifest child entry to point at first optimized artifact")
          ;; Manifest entry ordering is not a public contract for chained optimized
          ;; artifacts, so the second manifest only asserts membership.
          (%assert (member (claw-lisp.core.domain:artifact-cas-hash first-artifact)
                           second-entry-hashes :test #'string=)
                   "Expected second manifest to include the first optimized artifact hash")
          (%assert (member (claw-lisp.core.domain:artifact-cas-hash second-artifact)
                           second-entry-hashes :test #'string=)
                   "Expected second manifest to include the second optimized artifact hash")))))
  (format t "~&+ test-achatina-ir-optimization-pipeline-chains-provenance passed~%")
  t)

(defun test-achatina-ir-pipeline-without-runtime-keeps-in-memory-results ()
  (let* ((graph (%make-test-achatina-semantic-graph))
         (context (claw-lisp.ir.expander:make-ir-pass-context
                   :identity "workflow/support-triage"
                   :capabilities '("web-search" "model-call")
                   :policies '(:require-review :no-shell)))
         (validated (claw-lisp.ir.expander:prepare-validated-ir graph context))
         (pipeline (claw-lisp.ir.optimize:optimize-validated-ir-pipeline
                    (claw-lisp.ir.expander:ir-pass-result-graph validated)
                    context)))
    (%assert (= 2 (length (claw-lisp.ir.expander:ir-pipeline-run-results pipeline)))
             "Expected in-memory pipeline to produce both optimization pass results")
    (%assert (null (claw-lisp.ir.expander:ir-pipeline-run-persisted-results pipeline))
             "Expected in-memory pipeline to avoid persistence records")
    (%assert (null (claw-lisp.ir.expander:ir-pipeline-run-final-graph-artifact pipeline))
             "Expected in-memory pipeline to have no final graph artifact")
    (%assert (null (claw-lisp.ir.expander:ir-pipeline-run-final-manifest-artifact pipeline))
             "Expected in-memory pipeline to have no final manifest artifact"))
  (format t "~&+ test-achatina-ir-pipeline-without-runtime-keeps-in-memory-results passed~%")
  t)

(defun test-achatina-ir-pipeline-rejects-invalid-step ()
  (let* ((graph (%make-test-achatina-semantic-graph))
         (context (claw-lisp.ir.expander:make-ir-pass-context
                   :identity "workflow/support-triage"))
         (bad-step (claw-lisp.ir.expander:make-ir-pipeline-step
                    :pass-name :bad-stage
                    :output-stage :not-a-real-stage
                    :transform-fn (lambda (current-graph current-context)
                                    (declare (ignore current-context))
                                    current-graph)))
         (signaled nil))
    (handler-case
        (claw-lisp.ir.expander:run-achatina-ir-pipeline graph context (list bad-step))
      (error ()
        (setf signaled t)))
    (%assert signaled
             "Expected malformed pipeline step to be rejected before execution"))
  (format t "~&+ test-achatina-ir-pipeline-rejects-invalid-step passed~%")
  t)

(defun test-achatina-ir-pipeline-allows-empty-step-list ()
  (let* ((graph (%make-test-achatina-semantic-graph))
         (context (claw-lisp.ir.expander:make-ir-pass-context
                   :identity "workflow/support-triage"))
         (pipeline (claw-lisp.ir.expander:run-achatina-ir-pipeline graph context nil)))
    (%assert (null (claw-lisp.ir.expander:ir-pipeline-run-results pipeline))
             "Expected empty pipeline to return no pass results")
    (%assert (null (claw-lisp.ir.expander:ir-pipeline-run-persisted-results pipeline))
             "Expected empty pipeline to return no persistence results")
    (%assert (null (claw-lisp.ir.expander:ir-pipeline-run-final-result pipeline))
             "Expected empty pipeline to return no final pass result")
    (%assert (null (claw-lisp.ir.expander:ir-pipeline-run-final-graph-artifact pipeline))
             "Expected empty pipeline to return no final graph artifact")
    (%assert (null (claw-lisp.ir.expander:ir-pipeline-run-final-manifest-artifact pipeline))
             "Expected empty pipeline to return no final manifest artifact"))
  (format t "~&+ test-achatina-ir-pipeline-allows-empty-step-list passed~%")
  t)

(defun test-achatina-ir-pipeline-prefers-context-parent-over-initial-artifact ()
  (%with-temp-cas-artifact-roots (cas-root ref-root)
    (let* ((config (make-test-runtime-config
                    :cas-objects-root (namestring cas-root)
                    :cas-ref-root (namestring ref-root)))
           (runtime (make-runtime :config config))
           (graph (%make-test-achatina-semantic-graph))
           (preset-parent "sha256:aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa")
           (context (claw-lisp.ir.expander:make-ir-pass-context
                     :identity "workflow/support-triage"
                     :parent-ir-hash preset-parent
                     :metadata '(:artifact-identity "stale-value"
                                 :pipeline-stage :semantic-ir))))
      (multiple-value-bind (semantic-artifact semantic-info)
          (claw-lisp.ir.cas-bridge:materialize-ir-to-cas
           runtime graph
           :ref-name "ir/workflows/support-triage/semantic/current"
           :metadata '(:pipeline-stage :semantic-ir))
        (declare (ignore semantic-info))
        (let* ((validated (claw-lisp.ir.expander:prepare-validated-ir graph context))
               (pipeline (claw-lisp.ir.optimize:optimize-validated-ir-pipeline
                          (claw-lisp.ir.expander:ir-pass-result-graph validated)
                          context
                          :initial-parent-artifact semantic-artifact))
               (first-result (first (claw-lisp.ir.expander:ir-pipeline-run-results pipeline)))
               (first-metadata (claw-lisp.ir.schema:ir-graph-metadata
                                (claw-lisp.ir.expander:ir-pass-result-graph first-result))))
          (%assert (string= preset-parent (getf first-metadata :parent-ir-hash))
                   "Expected explicit context parent hash to override initial parent artifact")
          (%assert (string/= (claw-lisp.core.domain:artifact-cas-hash semantic-artifact)
                             (getf first-metadata :parent-ir-hash))
                   "Expected initial parent artifact hash to be ignored when context parent is preset")
          (%assert (string= "workflow/support-triage"
                            (getf first-metadata :artifact-identity))
                   "Expected reserved artifact identity to override transform/context metadata")))))
  (format t "~&+ test-achatina-ir-pipeline-prefers-context-parent-over-initial-artifact passed~%")
  t)

(defun test-achatina-semantic-expansion-materializes-governance ()
  (let* ((graph (%make-test-achatina-semantic-graph))
         (context (claw-lisp.ir.expander:make-ir-pass-context
                   :identity "workflow/support-triage"
                   :capabilities '("web-search" "model-call")
                   :policies '(:require-review :no-shell)))
         (result (claw-lisp.ir.semantic:expand-semantic-ir graph context))
         (expanded (claw-lisp.ir.expander:ir-pass-result-graph result))
         (node-ids (mapcar #'claw-lisp.ir.schema:ir-node-id
                           (claw-lisp.ir.schema:ir-graph-nodes expanded)))
         (edge-kinds (mapcar #'claw-lisp.ir.schema:ir-edge-kind
                             (claw-lisp.ir.schema:ir-graph-edges expanded))))
    (%assert (eq :materialize-governance-declarations
                 (claw-lisp.ir.expander:ir-pass-result-pass-name result))
             "Expected semantic expansion pass to report governance materialization")
    (%assert (eq :semantic-ir
                 (getf (claw-lisp.ir.schema:ir-graph-metadata expanded) :pipeline-stage))
             "Expected semantic expansion output to remain at semantic-ir stage")
    (%assert (member "workflow-achatina-1/semantic-anchor" node-ids :test #'string=)
             "Expected semantic expansion anchor node to be present")
    (%assert (member "workflow-achatina-1/semantic-anchor/capability/model-call" node-ids :test #'string=)
             "Expected capability declaration for model-call")
    (%assert (member "workflow-achatina-1/semantic-anchor/capability/web-search" node-ids :test #'string=)
             "Expected capability declaration for web-search")
    (%assert (member "workflow-achatina-1/semantic-anchor/policy/no-shell" node-ids :test #'string=)
             "Expected policy declaration for no-shell")
    (%assert (member "workflow-achatina-1/semantic-anchor/policy/require-review" node-ids :test #'string=)
             "Expected policy declaration for require-review")
    (%assert (member "workflow-achatina-1/semantic-anchor/tool/echo" node-ids :test #'string=)
             "Expected tool declaration for echo")
    (%assert (member "workflow-achatina-1/semantic-anchor/tool/file-read" node-ids :test #'string=)
             "Expected tool declaration for file-read")
    (%assert (member "workflow-achatina-1/semantic-anchor/memory-scope/durable-memory" node-ids :test #'string=)
             "Expected memory-scope declaration for durable-memory")
    (%assert (member "workflow-achatina-1/semantic-anchor/memory-scope/session-memory" node-ids :test #'string=)
             "Expected memory-scope declaration for session-memory")
    (%assert (= 2 (count :declares-capability edge-kinds))
             "Expected two capability declaration edges")
    (%assert (= 2 (count :declares-policy edge-kinds))
             "Expected two policy declaration edges")
    (%assert (= 2 (count :declares-tool edge-kinds))
             "Expected two tool declaration edges")
    (%assert (= 2 (count :declares-memory-scope edge-kinds))
             "Expected two memory-scope declaration edges"))
  (format t "~&+ test-achatina-semantic-expansion-materializes-governance passed~%")
  t)

(defun test-achatina-semantic-expansion-is-idempotent ()
  (let* ((graph (%make-test-achatina-semantic-graph))
         (context (claw-lisp.ir.expander:make-ir-pass-context
                   :identity "workflow/support-triage"
                   :capabilities '("web-search" "model-call")
                   :policies '(:require-review :no-shell)))
         (first (claw-lisp.ir.expander:ir-pass-result-graph
                 (claw-lisp.ir.semantic:expand-semantic-ir graph context)))
         (second (claw-lisp.ir.expander:ir-pass-result-graph
                  (claw-lisp.ir.semantic:expand-semantic-ir first context)))
         (first-node-ids (mapcar #'claw-lisp.ir.schema:ir-node-id
                                 (claw-lisp.ir.schema:ir-graph-nodes first)))
         (second-node-ids (mapcar #'claw-lisp.ir.schema:ir-node-id
                                  (claw-lisp.ir.schema:ir-graph-nodes second)))
         (first-edge-count (count-if (lambda (edge)
                                       (member (claw-lisp.ir.schema:ir-edge-kind edge)
                                               '(:declares-capability
                                                 :declares-policy
                                                 :declares-tool
                                                 :declares-memory-scope)
                                               :test #'eq))
                                     (claw-lisp.ir.schema:ir-graph-edges first)))
         (second-edge-count (count-if (lambda (edge)
                                        (member (claw-lisp.ir.schema:ir-edge-kind edge)
                                                '(:declares-capability
                                                  :declares-policy
                                                  :declares-tool
                                                  :declares-memory-scope)
                                                :test #'eq))
                                      (claw-lisp.ir.schema:ir-graph-edges second))))
    (%assert (= (length first-node-ids)
                (length (remove-duplicates first-node-ids :test #'string=)))
             "Expected first semantic expansion to have unique node ids")
    (%assert (= (length second-node-ids)
                (length (remove-duplicates second-node-ids :test #'string=)))
             "Expected semantic expansion rerun to keep unique node ids")
    (%assert (= (count-if (lambda (node-id)
                            (search "/semantic-anchor/" node-id))
                          first-node-ids)
                (count-if (lambda (node-id)
                            (search "/semantic-anchor/" node-id))
                          second-node-ids))
             "Expected semantic expansion rerun to avoid adding extra declaration nodes")
    (%assert (= first-edge-count second-edge-count)
             "Expected semantic expansion rerun to avoid duplicating declaration edges"))
  (format t "~&+ test-achatina-semantic-expansion-is-idempotent passed~%")
  t)

(defun test-achatina-semantic-expansion-persistence-links-parent ()
  (%with-temp-cas-artifact-roots (cas-root ref-root)
    (let* ((config (make-test-runtime-config
                    :cas-objects-root (namestring cas-root)
                    :cas-ref-root (namestring ref-root)))
           (runtime (make-runtime :config config))
           (graph (%make-test-achatina-semantic-graph))
           (context (claw-lisp.ir.expander:make-ir-pass-context
                     :identity "workflow/support-triage"
                     :capabilities '("web-search" "model-call")
                     :policies '(:require-review :no-shell))))
      (multiple-value-bind (semantic-artifact semantic-info)
          (claw-lisp.ir.cas-bridge:materialize-ir-to-cas
           runtime graph
           :ref-name "ir/workflows/support-triage/semantic/current"
           :metadata '(:pipeline-stage :semantic-ir))
        (declare (ignore semantic-info))
        (let* ((pipeline (claw-lisp.ir.semantic:expand-semantic-ir-pipeline
                          graph
                          context
                          :runtime runtime
                          :initial-parent-artifact semantic-artifact
                          :ref-prefix "ir/workflows/support-triage/semantic-expanded"
                          :manifest-ref-prefix "manifests/ir/workflows/support-triage/semantic-expanded"
                          :metadata '(:workflow-id "workflow/support-triage")))
               (persisted (first (claw-lisp.ir.expander:ir-pipeline-run-persisted-results pipeline)))
               (expanded-artifact (getf persisted :graph))
               (loaded (claw-lisp.ir.cas-bridge:load-ir-from-cas
                        runtime
                        (claw-lisp.core.domain:artifact-cas-hash expanded-artifact))))
          (%assert (string= (claw-lisp.core.domain:artifact-cas-hash semantic-artifact)
                            (getf (claw-lisp.ir.schema:ir-graph-metadata loaded)
                                  :parent-ir-hash))
                   "Expected semantic expansion artifact to point at semantic parent hash")
          (%assert (eq :semantic-ir
                       (getf (claw-lisp.ir.schema:ir-graph-metadata loaded)
                             :source-stage))
                   "Expected semantic expansion artifact to record semantic source stage")
          (%assert (eq :materialize-governance-declarations
                       (getf (claw-lisp.ir.schema:ir-graph-metadata loaded)
                             :pass-name))
                   "Expected semantic expansion artifact to record governance pass name")))))
  (format t "~&+ test-achatina-semantic-expansion-persistence-links-parent passed~%")
  t)

(defun test-achatina-semantic-expansion-no-governance-is-no-op ()
  (let* ((graph (claw-lisp.ir.schema:make-semantic-ir
                 :id "workflow-achatina-empty"
                 :nodes (copy-list (claw-lisp.ir.schema:ir-graph-nodes
                                    (%make-test-achatina-semantic-graph)))
                 :edges (copy-list (claw-lisp.ir.schema:ir-graph-edges
                                    (%make-test-achatina-semantic-graph)))
                 :metadata '(:artifact-identity "workflow/support-triage"
                             :pipeline-stage :semantic-ir)))
         (context (claw-lisp.ir.expander:make-ir-pass-context
                   :identity "workflow/support-triage"))
         (result (claw-lisp.ir.semantic:expand-semantic-ir graph context))
         (expanded (claw-lisp.ir.expander:ir-pass-result-graph result))
         (original-node-ids (mapcar #'claw-lisp.ir.schema:ir-node-id
                                    (claw-lisp.ir.schema:ir-graph-nodes graph)))
         (expanded-node-ids (mapcar #'claw-lisp.ir.schema:ir-node-id
                                    (claw-lisp.ir.schema:ir-graph-nodes expanded)))
         (declaration-edge-count (count-if (lambda (edge)
                                             (member (claw-lisp.ir.schema:ir-edge-kind edge)
                                                     '(:declares-capability
                                                       :declares-policy
                                                       :declares-tool
                                                       :declares-memory-scope)
                                                     :test #'eq))
                                           (claw-lisp.ir.schema:ir-graph-edges expanded))))
    (%assert (equal original-node-ids expanded-node-ids)
             "Expected semantic expansion without governance metadata to preserve node order")
    (%assert (equal (claw-lisp.ir.schema:ir-graph-edges graph)
                    (claw-lisp.ir.schema:ir-graph-edges expanded))
             "Expected semantic expansion without governance metadata to preserve edges")
    (%assert (= 0 (count-if (lambda (node-id)
                              (search "/semantic-anchor" node-id))
                            expanded-node-ids))
             "Expected semantic expansion without governance metadata to avoid declaration nodes")
    (%assert (= 0 declaration-edge-count)
             "Expected semantic expansion without governance metadata to avoid declaration edges")
    (%assert (eq :materialize-governance-declarations
                 (claw-lisp.ir.expander:ir-pass-result-pass-name result))
             "Expected semantic expansion to still report the governance pass"))
  (format t "~&+ test-achatina-semantic-expansion-no-governance-is-no-op passed~%")
  t)

(defun test-achatina-semantic-expansion-materializes-tools-only ()
  (let* ((graph (claw-lisp.ir.schema:make-semantic-ir
                 :id "workflow-achatina-tools"
                 :nodes nil
                 :edges nil
                 :metadata '(:artifact-identity "workflow/support-triage"
                             :pipeline-stage :semantic-ir
                             :tools ("echo" "file-read"))))
         (context (claw-lisp.ir.expander:make-ir-pass-context
                   :identity "workflow/support-triage"))
         (expanded (claw-lisp.ir.expander:ir-pass-result-graph
                    (claw-lisp.ir.semantic:expand-semantic-ir graph context)))
         (node-ids (mapcar #'claw-lisp.ir.schema:ir-node-id
                           (claw-lisp.ir.schema:ir-graph-nodes expanded)))
         (edge-kinds (mapcar #'claw-lisp.ir.schema:ir-edge-kind
                             (claw-lisp.ir.schema:ir-graph-edges expanded))))
    (%assert (member "workflow-achatina-tools/semantic-anchor" node-ids :test #'string=)
             "Expected tools-only expansion to materialize the semantic anchor")
    (%assert (member "workflow-achatina-tools/semantic-anchor/tool/echo" node-ids :test #'string=)
             "Expected tools-only expansion to materialize echo tool declaration")
    (%assert (member "workflow-achatina-tools/semantic-anchor/tool/file-read" node-ids :test #'string=)
             "Expected tools-only expansion to materialize file-read tool declaration")
    (%assert (= 2 (count :declares-tool edge-kinds))
             "Expected tools-only expansion to emit two :declares-tool edges")
    (%assert (= 0 (count :declares-memory-scope edge-kinds))
             "Expected tools-only expansion to avoid memory-scope edges"))
  (format t "~&+ test-achatina-semantic-expansion-materializes-tools-only passed~%")
  t)

(defun test-achatina-semantic-expansion-materializes-memory-scopes-only ()
  (let* ((graph (claw-lisp.ir.schema:make-semantic-ir
                 :id "workflow-achatina-memory"
                 :nodes nil
                 :edges nil
                 :metadata '(:artifact-identity "workflow/support-triage"
                             :pipeline-stage :semantic-ir
                             :memory-scopes (:session-memory :durable-memory))))
         (context (claw-lisp.ir.expander:make-ir-pass-context
                   :identity "workflow/support-triage"))
         (expanded (claw-lisp.ir.expander:ir-pass-result-graph
                    (claw-lisp.ir.semantic:expand-semantic-ir graph context)))
         (node-ids (mapcar #'claw-lisp.ir.schema:ir-node-id
                           (claw-lisp.ir.schema:ir-graph-nodes expanded)))
         (edge-kinds (mapcar #'claw-lisp.ir.schema:ir-edge-kind
                             (claw-lisp.ir.schema:ir-graph-edges expanded))))
    (%assert (member "workflow-achatina-memory/semantic-anchor" node-ids :test #'string=)
             "Expected memory-scope-only expansion to materialize the semantic anchor")
    (%assert (member "workflow-achatina-memory/semantic-anchor/memory-scope/durable-memory"
                     node-ids :test #'string=)
             "Expected memory-scope-only expansion to materialize durable-memory declaration")
    (%assert (member "workflow-achatina-memory/semantic-anchor/memory-scope/session-memory"
                     node-ids :test #'string=)
             "Expected memory-scope-only expansion to materialize session-memory declaration")
    (%assert (= 2 (count :declares-memory-scope edge-kinds))
             "Expected memory-scope-only expansion to emit two :declares-memory-scope edges")
    (%assert (= 0 (count :declares-tool edge-kinds))
             "Expected memory-scope-only expansion to avoid tool edges"))
  (format t "~&+ test-achatina-semantic-expansion-materializes-memory-scopes-only passed~%")
  t)

(defun test-achatina-semantic-expansion-sanitizes-governance-names ()
  (let* ((graph (claw-lisp.ir.schema:make-semantic-ir
                 :id "workflow-achatina-sanitize"
                 :nodes nil
                 :edges nil
                 :metadata '(:artifact-identity "workflow/support-triage"
                             :pipeline-stage :semantic-ir
                             :capabilities ("web search+beta" "shell/read-only")
                             :policies nil
                             :tools ("git status" "shell/read-only")
                             :memory-scopes (:memory :memory/read-only))))
         (context (claw-lisp.ir.expander:make-ir-pass-context
                   :identity "workflow/support-triage"))
         (expanded (claw-lisp.ir.expander:ir-pass-result-graph
                    (claw-lisp.ir.semantic:expand-semantic-ir graph context)))
         (node-ids (mapcar #'claw-lisp.ir.schema:ir-node-id
                           (claw-lisp.ir.schema:ir-graph-nodes expanded))))
    (%assert (member "workflow-achatina-sanitize/semantic-anchor/capability/web-search-beta"
                     node-ids :test #'string=)
             "Expected spaces and plus signs to normalize into dashes")
    (%assert (member "workflow-achatina-sanitize/semantic-anchor/capability/shell-read-only"
                     node-ids :test #'string=)
             "Expected slash to normalize into a dash")
    (%assert (member "workflow-achatina-sanitize/semantic-anchor/tool/git-status"
                     node-ids :test #'string=)
             "Expected spaces in tool names to normalize into dashes")
    (%assert (member "workflow-achatina-sanitize/semantic-anchor/tool/shell-read-only"
                     node-ids :test #'string=)
             "Expected slash in tool names to normalize into a dash")
    (%assert (member "workflow-achatina-sanitize/semantic-anchor/memory-scope/memory"
                     node-ids :test #'string=)
             "Expected simple memory-scope symbols to normalize through symbol-name")
    (%assert (member "workflow-achatina-sanitize/semantic-anchor/memory-scope/memory-read-only"
                     node-ids :test #'string=)
             "Expected slash in memory-scope names to normalize into a dash"))
  (format t "~&+ test-achatina-semantic-expansion-sanitizes-governance-names passed~%")
  t)

(defun test-achatina-execution-preparation-uses-semantic-declarations ()
  (let* ((graph (%make-test-achatina-semantic-graph))
         (context (claw-lisp.ir.expander:make-ir-pass-context
                   :identity "workflow/support-triage"))
         (expanded (claw-lisp.ir.expander:ir-pass-result-graph
                    (claw-lisp.ir.semantic:expand-semantic-ir graph context)))
         (declaration-only (claw-lisp.ir.schema:make-semantic-ir
                            :id (claw-lisp.ir.schema:ir-graph-id expanded)
                            :nodes (copy-list (claw-lisp.ir.schema:ir-graph-nodes expanded))
                            :edges (copy-list (claw-lisp.ir.schema:ir-graph-edges expanded))
                            :metadata '(:artifact-identity "workflow/support-triage"
                                        :pipeline-stage :semantic-ir
                                        :capabilities ("web-search" "model-call")
                                        :policies (:require-review :no-shell))))
         (prepared (claw-lisp.ir.prepare:prepare-execution-input declaration-only)))
    (%assert (equal '("echo" "file-read")
                    (claw-lisp.ir.prepare:execution-preparation-tools prepared))
             "Expected execution preparation to extract tool declarations from semantic IR")
    (%assert (equal '(:durable-memory :session-memory)
                    (claw-lisp.ir.prepare:execution-preparation-memory-scopes prepared))
             "Expected execution preparation to extract memory-scope declarations from semantic IR")
    (%assert (equal '("model-call" "web-search")
                    (claw-lisp.ir.prepare:execution-preparation-capabilities prepared))
             "Expected execution preparation to extract capability declarations from semantic IR")
    (%assert (equal '(:no-shell :require-review)
                    (claw-lisp.ir.prepare:execution-preparation-policies prepared))
             "Expected execution preparation to extract policy declarations from semantic IR")
    (%assert (string= "workflow/support-triage"
                      (claw-lisp.ir.prepare:execution-preparation-workflow-id prepared))
             "Expected execution preparation to preserve workflow identity"))
  (format t "~&+ test-achatina-execution-preparation-uses-semantic-declarations passed~%")
  t)

(defun test-achatina-execution-preparation-falls-back-to-metadata ()
  (let* ((graph (claw-lisp.ir.schema:make-semantic-ir
                 :id "workflow-prepare-metadata"
                 :nodes nil
                 :edges nil
                 :metadata '(:artifact-identity "workflow/support-triage"
                             :pipeline-stage :semantic-ir
                             :capabilities ("web-search" "model-call" "web-search")
                             :policies (:require-review :no-shell :require-review)
                             :tools ("file-read" "echo" "echo")
                             :memory-scopes (:session-memory :durable-memory :session-memory))))
         (prepared (claw-lisp.ir.prepare:prepare-execution-input graph)))
    (%assert (equal '("model-call" "web-search")
                    (claw-lisp.ir.prepare:execution-preparation-capabilities prepared))
             "Expected metadata-only preparation to normalize capability names")
    (%assert (equal '(:no-shell :require-review)
                    (claw-lisp.ir.prepare:execution-preparation-policies prepared))
             "Expected metadata-only preparation to normalize policies")
    (%assert (equal '("echo" "file-read")
                    (claw-lisp.ir.prepare:execution-preparation-tools prepared))
             "Expected metadata-only preparation to normalize tool names")
    (%assert (equal '(:durable-memory :session-memory)
                    (claw-lisp.ir.prepare:execution-preparation-memory-scopes prepared))
             "Expected metadata-only preparation to normalize memory scopes"))
  (format t "~&+ test-achatina-execution-preparation-falls-back-to-metadata passed~%")
  t)

(defun test-achatina-execution-preparation-allows-mixed-equal-definitions ()
  (let* ((graph (%make-test-achatina-semantic-graph))
         (context (claw-lisp.ir.expander:make-ir-pass-context
                   :identity "workflow/support-triage"))
         (expanded (claw-lisp.ir.expander:ir-pass-result-graph
                    (claw-lisp.ir.semantic:expand-semantic-ir graph context)))
         (prepared (claw-lisp.ir.prepare:prepare-execution-input expanded)))
    (%assert (equal '("echo" "file-read")
                    (claw-lisp.ir.prepare:execution-preparation-tools prepared))
             "Expected mixed-mode preparation to accept equal tool definitions")
    (%assert (equal '(:durable-memory :session-memory)
                    (claw-lisp.ir.prepare:execution-preparation-memory-scopes prepared))
             "Expected mixed-mode preparation to accept equal memory-scope definitions"))
  (format t "~&+ test-achatina-execution-preparation-allows-mixed-equal-definitions passed~%")
  t)

(defun test-achatina-execution-preparation-rejects-conflicting-definitions ()
  (let* ((graph (%make-test-achatina-semantic-graph))
         (context (claw-lisp.ir.expander:make-ir-pass-context
                   :identity "workflow/support-triage"))
         (expanded (claw-lisp.ir.expander:ir-pass-result-graph
                    (claw-lisp.ir.semantic:expand-semantic-ir graph context)))
         (conflicting (claw-lisp.ir.schema:make-semantic-ir
                       :id (claw-lisp.ir.schema:ir-graph-id expanded)
                       :nodes (copy-list (claw-lisp.ir.schema:ir-graph-nodes expanded))
                       :edges (copy-list (claw-lisp.ir.schema:ir-graph-edges expanded))
                       :metadata '(:artifact-identity "workflow/support-triage"
                                   :pipeline-stage :semantic-ir
                                   :capabilities ("web-search" "shell")
                                   :policies (:require-review :no-shell)
                                   :tools ("echo" "shell-command")
                                   :memory-scopes (:session-memory :durable-memory))))
         (signaled nil))
    (handler-case
        (claw-lisp.ir.prepare:prepare-execution-input conflicting)
      (claw-lisp.ir.conditions:ir-validation-error ()
        (setf signaled t)))
    (%assert signaled
             "Expected conflicting metadata and semantic declarations to signal ir-validation-error"))
  (format t "~&+ test-achatina-execution-preparation-rejects-conflicting-definitions passed~%")
  t)

(defun test-achatina-execution-preparation-rejects-multiple-anchors ()
  (let* ((graph (claw-lisp.ir.schema:make-semantic-ir
                 :id "workflow-prepare-bad-anchors"
                 :nodes (list (claw-lisp.ir.schema:make-ir-node
                               :id "anchor-a"
                               :kind :semantic-anchor
                               :payload '(:artifact-identity "workflow/support-triage"
                                          :pipeline-stage :semantic-ir))
                              (claw-lisp.ir.schema:make-ir-node
                               :id "anchor-b"
                               :kind :semantic-anchor
                               :payload '(:artifact-identity "workflow/support-triage"
                                          :pipeline-stage :semantic-ir)))
                 :edges nil
                 :metadata '(:artifact-identity "workflow/support-triage"
                             :pipeline-stage :semantic-ir)))
         (signaled nil))
    (handler-case
        (claw-lisp.ir.prepare:prepare-execution-input graph)
      (claw-lisp.ir.conditions:ir-validation-error ()
        (setf signaled t)))
    (%assert signaled
             "Expected multiple semantic anchors to signal ir-validation-error"))
  (format t "~&+ test-achatina-execution-preparation-rejects-multiple-anchors passed~%")
  t)

(defun test-achatina-execution-preparation-rejects-declaration-edges-without-anchor ()
  (let* ((graph (claw-lisp.ir.schema:make-semantic-ir
                 :id "workflow-prepare-no-anchor"
                 :nodes (list (claw-lisp.ir.schema:make-ir-node
                               :id "tool-node"
                               :kind :tool-declaration
                               :payload '(:name "echo")))
                 :edges (list (claw-lisp.ir.schema:make-ir-edge
                               :from-id "not-anchor"
                               :to-id "tool-node"
                               :kind :declares-tool))
                 :metadata '(:artifact-identity "workflow/support-triage"
                             :pipeline-stage :semantic-ir)))
         (signaled nil))
    (handler-case
        (claw-lisp.ir.prepare:prepare-execution-input graph)
      (claw-lisp.ir.conditions:ir-validation-error ()
        (setf signaled t)))
    (%assert signaled
             "Expected declaration edges without a semantic anchor to signal ir-validation-error"))
  (format t "~&+ test-achatina-execution-preparation-rejects-declaration-edges-without-anchor passed~%")
  t)

(defun test-achatina-execution-preparation-rejects-non-anchor-origin ()
  (let* ((graph (claw-lisp.ir.schema:make-semantic-ir
                 :id "workflow-prepare-wrong-origin"
                 :nodes (list (claw-lisp.ir.schema:make-ir-node
                               :id "anchor"
                               :kind :semantic-anchor
                               :payload '(:artifact-identity "workflow/support-triage"
                                          :pipeline-stage :semantic-ir))
                              (claw-lisp.ir.schema:make-ir-node
                               :id "not-anchor"
                               :kind :input
                               :payload '(:text "hello"))
                              (claw-lisp.ir.schema:make-ir-node
                               :id "tool-node"
                               :kind :tool-declaration
                               :payload '(:name "echo")))
                 :edges (list (claw-lisp.ir.schema:make-ir-edge
                               :from-id "not-anchor"
                               :to-id "tool-node"
                               :kind :declares-tool))
                 :metadata '(:artifact-identity "workflow/support-triage"
                             :pipeline-stage :semantic-ir)))
         (signaled nil))
    (handler-case
        (claw-lisp.ir.prepare:prepare-execution-input graph)
      (claw-lisp.ir.conditions:ir-validation-error ()
        (setf signaled t)))
    (%assert signaled
             "Expected non-anchor declaration edge origins to signal ir-validation-error"))
  (format t "~&+ test-achatina-execution-preparation-rejects-non-anchor-origin passed~%")
  t)

(defun test-achatina-execution-preparation-rejects-missing-declaration-target ()
  (let* ((graph (claw-lisp.ir.schema:make-semantic-ir
                 :id "workflow-prepare-missing-target"
                 :nodes (list (claw-lisp.ir.schema:make-ir-node
                               :id "anchor"
                               :kind :semantic-anchor
                               :payload '(:artifact-identity "workflow/support-triage"
                                          :pipeline-stage :semantic-ir)))
                 :edges (list (claw-lisp.ir.schema:make-ir-edge
                               :from-id "anchor"
                               :to-id "missing-node"
                               :kind :declares-tool))
                 :metadata '(:artifact-identity "workflow/support-triage"
                             :pipeline-stage :semantic-ir)))
         (signaled nil))
    (handler-case
        (claw-lisp.ir.prepare:prepare-execution-input graph)
      (claw-lisp.ir.conditions:ir-validation-error ()
        (setf signaled t)))
    (%assert signaled
             "Expected missing declaration targets to signal ir-validation-error"))
  (format t "~&+ test-achatina-execution-preparation-rejects-missing-declaration-target passed~%")
  t)

(defun test-achatina-execution-preparation-rejects-wrong-declaration-kind ()
  (let* ((graph (claw-lisp.ir.schema:make-semantic-ir
                 :id "workflow-prepare-wrong-kind"
                 :nodes (list (claw-lisp.ir.schema:make-ir-node
                               :id "anchor"
                               :kind :semantic-anchor
                               :payload '(:artifact-identity "workflow/support-triage"
                                          :pipeline-stage :semantic-ir))
                              (claw-lisp.ir.schema:make-ir-node
                               :id "wrong-node"
                               :kind :policy-declaration
                               :payload '(:name :no-shell)))
                 :edges (list (claw-lisp.ir.schema:make-ir-edge
                               :from-id "anchor"
                               :to-id "wrong-node"
                               :kind :declares-tool))
                 :metadata '(:artifact-identity "workflow/support-triage"
                             :pipeline-stage :semantic-ir)))
         (signaled nil))
    (handler-case
        (claw-lisp.ir.prepare:prepare-execution-input graph)
      (claw-lisp.ir.conditions:ir-validation-error ()
        (setf signaled t)))
    (%assert signaled
             "Expected wrong declaration target kinds to signal ir-validation-error"))
  (format t "~&+ test-achatina-execution-preparation-rejects-wrong-declaration-kind passed~%")
  t)

(defun test-achatina-execution-preparation-rejects-orphan-declaration-node ()
  (let* ((graph (claw-lisp.ir.schema:make-semantic-ir
                 :id "workflow-prepare-orphan"
                 :nodes (list (claw-lisp.ir.schema:make-ir-node
                               :id "anchor"
                               :kind :semantic-anchor
                               :payload '(:artifact-identity "workflow/support-triage"
                                          :pipeline-stage :semantic-ir))
                              (claw-lisp.ir.schema:make-ir-node
                               :id "tool-node"
                               :kind :tool-declaration
                               :payload '(:name "echo")))
                 :edges nil
                 :metadata '(:artifact-identity "workflow/support-triage"
                             :pipeline-stage :semantic-ir)))
         (signaled nil))
    (handler-case
        (claw-lisp.ir.prepare:prepare-execution-input graph)
      (claw-lisp.ir.conditions:ir-validation-error ()
        (setf signaled t)))
    (%assert signaled
             "Expected orphan declaration nodes to signal ir-validation-error"))
  (format t "~&+ test-achatina-execution-preparation-rejects-orphan-declaration-node passed~%")
  t)

(defun test-achatina-execution-preparation-rejects-invalid-declaration-payload ()
  (let* ((graph (claw-lisp.ir.schema:make-semantic-ir
                 :id "workflow-prepare-bad-payload"
                 :nodes (list (claw-lisp.ir.schema:make-ir-node
                               :id "anchor"
                               :kind :semantic-anchor
                               :payload '(:artifact-identity "workflow/support-triage"
                                          :pipeline-stage :semantic-ir))
                              (claw-lisp.ir.schema:make-ir-node
                               :id "memory-node"
                               :kind :memory-scope-declaration
                               :payload '(:name "session-memory")))
                 :edges (list (claw-lisp.ir.schema:make-ir-edge
                               :from-id "anchor"
                               :to-id "memory-node"
                               :kind :declares-memory-scope))
                 :metadata '(:artifact-identity "workflow/support-triage"
                             :pipeline-stage :semantic-ir)))
         (signaled nil))
    (handler-case
        (claw-lisp.ir.prepare:prepare-execution-input graph)
      (claw-lisp.ir.conditions:ir-validation-error ()
        (setf signaled t)))
    (%assert signaled
             "Expected invalid declaration payload names to signal ir-validation-error"))
  (format t "~&+ test-achatina-execution-preparation-rejects-invalid-declaration-payload passed~%")
  t)

(defun test-achatina-execution-preparation-rejects-invalid-stage ()
  (let* ((graph (claw-lisp.ir.schema:make-semantic-ir
                 :id "workflow-prepare-invalid-stage"
                 :nodes nil
                 :edges nil
                 :metadata '(:artifact-identity "workflow/support-triage"
                             :pipeline-stage :expanded-form)))
         (signaled nil))
    (handler-case
        (claw-lisp.ir.prepare:prepare-execution-input graph)
      (claw-lisp.ir.conditions:ir-validation-error ()
        (setf signaled t)))
    (%assert signaled
             "Expected unsupported preparation stages to signal ir-validation-error"))
  (format t "~&+ test-achatina-execution-preparation-rejects-invalid-stage passed~%")
  t)

(defun test-achatina-execution-plan-lowering-carries-governance ()
  (let* ((graph (%make-test-achatina-semantic-graph))
         (context (claw-lisp.ir.expander:make-ir-pass-context
                   :identity "workflow/support-triage"))
         (expanded (claw-lisp.ir.expander:ir-pass-result-graph
                    (claw-lisp.ir.semantic:expand-semantic-ir graph context)))
         (result (claw-lisp.ir.execution-plan:lower-to-execution-plan expanded context))
         (plan (claw-lisp.ir.expander:ir-pass-result-graph result))
         (metadata (claw-lisp.ir.schema:ir-graph-metadata plan))
         (node-kinds (mapcar #'claw-lisp.ir.schema:ir-node-kind
                             (claw-lisp.ir.schema:ir-graph-nodes plan)))
         (edge-kinds (mapcar #'claw-lisp.ir.schema:ir-edge-kind
                             (claw-lisp.ir.schema:ir-graph-edges plan))))
    (%assert (eq :execution-plan
                 (claw-lisp.ir.schema:ir-graph-node-type plan))
             "Expected lowered graph node-type to be :execution-plan")
    (%assert (eq :execution-plan
                 (getf metadata :pipeline-stage))
             "Expected lowered graph metadata to report :execution-plan stage")
    (%assert (equal '("model-call" "web-search")
                    (getf metadata :capabilities))
             "Expected normalized capabilities to carry into execution-plan metadata")
    (%assert (equal '(:no-shell :require-review)
                    (getf metadata :policies))
             "Expected normalized policies to carry into execution-plan metadata")
    (%assert (equal '("echo" "file-read")
                    (getf metadata :tools))
             "Expected normalized tools to carry into execution-plan metadata")
    (%assert (equal '(:durable-memory :session-memory)
                    (getf metadata :memory-scopes))
             "Expected normalized memory scopes to carry into execution-plan metadata")
    (%assert (eq :semantic-ir
                 (getf metadata :lowered-from-stage))
             "Expected execution-plan metadata to retain the source stage marker")
    (%assert (eq :prepared-execution-input
                 (getf metadata :governance-source))
             "Expected execution-plan metadata to record the governance source")
    (%assert (equal '(:plan-input :plan-tool-call) node-kinds)
             "Expected governance scaffolding to be filtered from execution-plan nodes")
    (%assert (equal '(:plan-flow) edge-kinds)
             "Expected governance declaration edges to be filtered from execution-plan edges"))
  (format t "~&+ test-achatina-execution-plan-lowering-carries-governance passed~%")
  t)

(defun test-achatina-execution-plan-lowering-deduplicates-flow-edges ()
  (let* ((graph (%make-test-achatina-semantic-graph))
         (context (claw-lisp.ir.expander:make-ir-pass-context
                   :identity "workflow/support-triage"))
         (expanded (claw-lisp.ir.expander:ir-pass-result-graph
                    (claw-lisp.ir.semantic:expand-semantic-ir graph context)))
         (result (claw-lisp.ir.execution-plan:lower-to-execution-plan expanded context))
         (plan (claw-lisp.ir.expander:ir-pass-result-graph result))
         (edges (claw-lisp.ir.schema:ir-graph-edges plan)))
    (%assert (= 1 (length edges))
             "Expected duplicate semantic :flow edges to collapse into one plan-flow edge")
    (%assert (eq :plan-flow
                 (claw-lisp.ir.schema:ir-edge-kind (first edges)))
             "Expected the surviving lowered edge to be :plan-flow"))
  (format t "~&+ test-achatina-execution-plan-lowering-deduplicates-flow-edges passed~%")
  t)

(defun test-achatina-execution-plan-lowering-deduplicates-flow-edges-deterministically ()
  (let* ((left (claw-lisp.ir.schema:make-semantic-ir
                :id "workflow-achatina-dedup-order"
                :nodes (list (claw-lisp.ir.schema:make-ir-node
                              :id "node-a"
                              :kind :input
                              :payload '(:text "hello"))
                             (claw-lisp.ir.schema:make-ir-node
                              :id "node-b"
                              :kind :tool-call
                              :payload '(:tool "echo")))
                :edges (list (claw-lisp.ir.schema:make-ir-edge
                              :from-id "node-a"
                              :to-id "node-b"
                              :kind :flow)
                             (claw-lisp.ir.schema:make-ir-edge
                              :from-id "node-a"
                              :to-id "node-b"
                              :kind :flow)
                             (claw-lisp.ir.schema:make-ir-edge
                              :from-id "node-a"
                              :to-id "node-b"
                              :kind :flow))
                :metadata '(:artifact-identity "workflow/support-triage"
                            :pipeline-stage :semantic-ir)))
         (right (claw-lisp.ir.schema:make-semantic-ir
                 :id "workflow-achatina-dedup-order"
                 :nodes (list (claw-lisp.ir.schema:make-ir-node
                               :id "node-b"
                               :kind :tool-call
                               :payload '(:tool "echo"))
                              (claw-lisp.ir.schema:make-ir-node
                               :id "node-a"
                               :kind :input
                               :payload '(:text "hello")))
                 :edges (list (claw-lisp.ir.schema:make-ir-edge
                               :from-id "node-a"
                               :to-id "node-b"
                               :kind :flow)
                              (claw-lisp.ir.schema:make-ir-edge
                               :from-id "node-a"
                               :to-id "node-b"
                               :kind :flow)
                              (claw-lisp.ir.schema:make-ir-edge
                               :from-id "node-a"
                               :to-id "node-b"
                               :kind :flow))
                 :metadata '(:artifact-identity "workflow/support-triage"
                             :pipeline-stage :semantic-ir)))
         (context (claw-lisp.ir.expander:make-ir-pass-context
                   :identity "workflow/support-triage"))
         (left-plan (claw-lisp.ir.expander:ir-pass-result-graph
                     (claw-lisp.ir.execution-plan:lower-to-execution-plan left context)))
         (right-plan (claw-lisp.ir.expander:ir-pass-result-graph
                      (claw-lisp.ir.execution-plan:lower-to-execution-plan right context))))
    (%assert (string= (claw-lisp.ir.schema:ir-object->canonical-string left-plan)
                      (claw-lisp.ir.schema:ir-object->canonical-string right-plan))
             "Expected duplicate-flow lowering to remain byte-identical across input permutations"))
  (format t "~&+ test-achatina-execution-plan-lowering-deduplicates-flow-edges-deterministically passed~%")
  t)

(defun test-achatina-execution-plan-lowering-allows-empty-operational-graph ()
  (let* ((graph (claw-lisp.ir.schema:make-semantic-ir
                 :id "workflow-empty-plan"
                 :nodes (list (claw-lisp.ir.schema:make-ir-node
                               :id "anchor"
                               :kind :semantic-anchor
                               :payload '(:artifact-identity "workflow/support-triage"
                                          :pipeline-stage :semantic-ir)))
                 :edges nil
                 :metadata '(:artifact-identity "workflow/support-triage"
                             :pipeline-stage :semantic-ir)))
         (context (claw-lisp.ir.expander:make-ir-pass-context
                   :identity "workflow/support-triage"))
         (result (claw-lisp.ir.execution-plan:lower-to-execution-plan graph context))
         (plan (claw-lisp.ir.expander:ir-pass-result-graph result)))
    (%assert (null (claw-lisp.ir.schema:ir-graph-nodes plan))
             "Expected an anchor-only semantic graph to lower to an empty execution plan")
    (%assert (null (claw-lisp.ir.schema:ir-graph-edges plan))
             "Expected an anchor-only semantic graph to have no plan edges"))
  (format t "~&+ test-achatina-execution-plan-lowering-allows-empty-operational-graph passed~%")
  t)

(defun test-achatina-execution-plan-lowering-is-deterministic ()
  (let* ((left (claw-lisp.ir.schema:make-semantic-ir
                :id "workflow-achatina-deterministic"
                :nodes (list (claw-lisp.ir.schema:make-ir-node
                              :id "node-b"
                              :kind :tool-call
                              :payload '(:tool "echo"))
                             (claw-lisp.ir.schema:make-ir-node
                              :id "node-a"
                              :kind :input
                              :payload '(:text "hello")))
                :edges (list (claw-lisp.ir.schema:make-ir-edge
                              :from-id "node-a"
                              :to-id "node-b"
                              :kind :flow))
                :metadata '(:artifact-identity "workflow/support-triage"
                            :pipeline-stage :semantic-ir
                            :capabilities ("web-search" "model-call")
                            :policies (:require-review :no-shell)
                            :tools ("file-read" "echo")
                            :memory-scopes (:session-memory :durable-memory))))
         (right (claw-lisp.ir.schema:make-semantic-ir
                 :id "workflow-achatina-deterministic"
                 :nodes (list (claw-lisp.ir.schema:make-ir-node
                               :id "node-a"
                               :kind :input
                               :payload '(:text "hello"))
                              (claw-lisp.ir.schema:make-ir-node
                               :id "node-b"
                               :kind :tool-call
                               :payload '(:tool "echo")))
                 :edges (list (claw-lisp.ir.schema:make-ir-edge
                               :from-id "node-a"
                               :to-id "node-b"
                               :kind :flow))
                 :metadata '(:artifact-identity "workflow/support-triage"
                             :pipeline-stage :semantic-ir
                             :capabilities ("model-call" "web-search")
                             :policies (:no-shell :require-review)
                             :tools ("echo" "file-read")
                             :memory-scopes (:durable-memory :session-memory))))
         (context (claw-lisp.ir.expander:make-ir-pass-context
                   :identity "workflow/support-triage"))
         (left-plan (claw-lisp.ir.expander:ir-pass-result-graph
                     (claw-lisp.ir.execution-plan:lower-to-execution-plan
                      (claw-lisp.ir.expander:ir-pass-result-graph
                       (claw-lisp.ir.semantic:expand-semantic-ir left context))
                      context)))
         (right-plan (claw-lisp.ir.expander:ir-pass-result-graph
                      (claw-lisp.ir.execution-plan:lower-to-execution-plan
                       (claw-lisp.ir.expander:ir-pass-result-graph
                        (claw-lisp.ir.semantic:expand-semantic-ir right context))
                       context))))
    (%assert (string= (claw-lisp.ir.schema:ir-object->canonical-string left-plan)
                      (claw-lisp.ir.schema:ir-object->canonical-string right-plan))
             "Expected equivalent prepared inputs to lower to byte-identical execution-plan IR"))
  (format t "~&+ test-achatina-execution-plan-lowering-is-deterministic passed~%")
  t)

(defun test-achatina-execution-plan-lowering-drops-semantic-node-metadata ()
  (let* ((graph (claw-lisp.ir.schema:make-semantic-ir
                 :id "workflow-achatina-node-metadata-drop"
                 :nodes (list (claw-lisp.ir.schema:make-ir-node
                               :id "node-a"
                               :kind :input
                               :payload '(:text "hello")
                               :metadata '(:priority 5 :semantic-tag "user-input"))
                              (claw-lisp.ir.schema:make-ir-node
                               :id "node-b"
                               :kind :tool-call
                               :payload '(:tool "echo")
                               :metadata '(:timeout-seconds 30)))
                 :edges (list (claw-lisp.ir.schema:make-ir-edge
                               :from-id "node-a"
                               :to-id "node-b"
                               :kind :flow))
                 :metadata '(:artifact-identity "workflow/support-triage"
                             :pipeline-stage :semantic-ir)))
         (context (claw-lisp.ir.expander:make-ir-pass-context
                   :identity "workflow/support-triage"))
         (plan (claw-lisp.ir.expander:ir-pass-result-graph
                (claw-lisp.ir.execution-plan:lower-to-execution-plan graph context))))
    (dolist (node (claw-lisp.ir.schema:ir-graph-nodes plan))
      (%assert (null (getf (claw-lisp.ir.schema:ir-node-metadata node) :priority))
               "Expected lowered execution-plan nodes to drop semantic :priority metadata")
      (%assert (null (getf (claw-lisp.ir.schema:ir-node-metadata node) :semantic-tag))
               "Expected lowered execution-plan nodes to drop semantic tags")
      (%assert (null (getf (claw-lisp.ir.schema:ir-node-metadata node) :timeout-seconds))
               "Expected lowered execution-plan nodes to drop semantic timeout metadata")
      (%assert (getf (claw-lisp.ir.schema:ir-node-metadata node) :source-node-id)
               "Expected lowered execution-plan nodes to retain provenance metadata")))
  (format t "~&+ test-achatina-execution-plan-lowering-drops-semantic-node-metadata passed~%")
  t)

(defun test-achatina-execution-plan-lowering-supports-control-edges ()
  (let* ((graph (claw-lisp.ir.schema:make-semantic-ir
                 :id "workflow-achatina-control-edge"
                 :nodes (list (claw-lisp.ir.schema:make-ir-node
                               :id "node-a"
                               :kind :input
                               :payload '(:text "hello"))
                              (claw-lisp.ir.schema:make-ir-node
                               :id "node-b"
                               :kind :tool-call
                               :payload '(:tool "echo")))
                 :edges (list (claw-lisp.ir.schema:make-ir-edge
                               :from-id "node-a"
                               :to-id "node-b"
                               :kind :flow)
                              (claw-lisp.ir.schema:make-ir-edge
                               :from-id "node-a"
                               :to-id "node-b"
                               :kind :control))
                 :metadata '(:artifact-identity "workflow/support-triage"
                             :pipeline-stage :semantic-ir)))
         (context (claw-lisp.ir.expander:make-ir-pass-context
                   :identity "workflow/support-triage"))
         (plan (claw-lisp.ir.expander:ir-pass-result-graph
                (claw-lisp.ir.execution-plan:lower-to-execution-plan graph context)))
         (edge-kinds (mapcar #'claw-lisp.ir.schema:ir-edge-kind
                             (claw-lisp.ir.schema:ir-graph-edges plan))))
    (%assert (equal '(:plan-control :plan-flow) edge-kinds)
             "Expected control and flow edges to lower into explicit execution-plan edge kinds"))
  (format t "~&+ test-achatina-execution-plan-lowering-supports-control-edges passed~%")
  t)

(defun test-achatina-execution-plan-lowering-supports-model-call-nodes ()
  (let* ((graph (claw-lisp.ir.schema:make-optimized-ir
                 :id "workflow-achatina-model-call"
                 :nodes (list (claw-lisp.ir.schema:make-ir-node
                               :id "node-a"
                               :kind :input
                               :payload '(:text "hello"))
                              (claw-lisp.ir.schema:make-ir-node
                               :id "node-b"
                               :kind :model-call
                               :payload '(:provider "mock"
                                          :model "mock-model"
                                          :prompt "hello model")))
                 :edges (list (claw-lisp.ir.schema:make-ir-edge
                               :from-id "node-a"
                               :to-id "node-b"
                               :kind :control))
                 :metadata '(:artifact-identity "workflow/support-triage"
                             :pipeline-stage :optimized-ir
                             :capabilities ("model-call"))))
         (context (claw-lisp.ir.expander:make-ir-pass-context
                   :identity "workflow/support-triage"
                   :capabilities '("model-call")))
         (plan (claw-lisp.ir.expander:ir-pass-result-graph
                (claw-lisp.ir.execution-plan:lower-to-execution-plan graph context)))
         (model-node (find "workflow/support-triage/plan/node-b"
                           (claw-lisp.ir.schema:ir-graph-nodes plan)
                           :key #'claw-lisp.ir.schema:ir-node-id
                           :test #'string=)))
    (%assert model-node
             "Expected execution-plan lowering to retain the model-call node")
    (%assert (eq :plan-model-call
                 (claw-lisp.ir.schema:ir-node-kind model-node))
             "Expected semantic :model-call to lower into :plan-model-call")
    (%assert (equal '(:provider "mock" :model "mock-model" :prompt "hello model")
                    (claw-lisp.ir.schema:ir-node-payload model-node))
             "Expected model-call payload to carry through lowering unchanged"))
  (format t "~&+ test-achatina-execution-plan-lowering-supports-model-call-nodes passed~%")
  t)

(defun test-achatina-execution-plan-lowering-supports-branch-nodes ()
  (let* ((graph (claw-lisp.ir.schema:make-optimized-ir
                 :id "workflow-achatina-branch"
                 :nodes (list (claw-lisp.ir.schema:make-ir-node
                               :id "node-a"
                               :kind :input
                               :payload '(:text "hello"))
                              (claw-lisp.ir.schema:make-ir-node
                               :id "node-b"
                               :kind :branch
                               :payload '(:condition "has-user-approval"
                                          :true-target "node-c"
                                          :false-target "node-d")))
                 :edges (list (claw-lisp.ir.schema:make-ir-edge
                               :from-id "node-a"
                               :to-id "node-b"
                               :kind :control))
                 :metadata '(:artifact-identity "workflow/support-triage"
                             :pipeline-stage :optimized-ir)))
         (context (claw-lisp.ir.expander:make-ir-pass-context
                   :identity "workflow/support-triage"))
         (plan (claw-lisp.ir.expander:ir-pass-result-graph
                (claw-lisp.ir.execution-plan:lower-to-execution-plan graph context)))
         (branch-node (find "workflow/support-triage/plan/node-b"
                            (claw-lisp.ir.schema:ir-graph-nodes plan)
                            :key #'claw-lisp.ir.schema:ir-node-id
                            :test #'string=)))
    (%assert branch-node
             "Expected execution-plan lowering to retain the branch node")
    (%assert (eq :plan-branch
                 (claw-lisp.ir.schema:ir-node-kind branch-node))
             "Expected semantic :branch to lower into :plan-branch")
    (%assert (equal '(:condition "has-user-approval"
                      :true-target "node-c"
                      :false-target "node-d")
                    (claw-lisp.ir.schema:ir-node-payload branch-node))
             "Expected branch payload to carry through lowering unchanged"))
  (format t "~&+ test-achatina-execution-plan-lowering-supports-branch-nodes passed~%")
  t)

(defun test-achatina-execution-plan-lowering-supports-human-review-nodes ()
  (let* ((graph (claw-lisp.ir.schema:make-optimized-ir
                 :id "workflow-achatina-human-review"
                 :nodes (list (claw-lisp.ir.schema:make-ir-node
                               :id "node-a"
                               :kind :input
                               :payload '(:text "hello"))
                              (claw-lisp.ir.schema:make-ir-node
                               :id "node-b"
                               :kind :human-review
                               :payload '(:prompt "Approve this response?"
                                          :review-id "review-1")))
                 :edges (list (claw-lisp.ir.schema:make-ir-edge
                               :from-id "node-a"
                               :to-id "node-b"
                               :kind :control))
                 :metadata '(:artifact-identity "workflow/support-triage"
                             :pipeline-stage :optimized-ir
                             :policies (:require-review))))
         (context (claw-lisp.ir.expander:make-ir-pass-context
                   :identity "workflow/support-triage"
                   :policies '(:require-review)))
         (plan (claw-lisp.ir.expander:ir-pass-result-graph
                (claw-lisp.ir.execution-plan:lower-to-execution-plan graph context)))
         (review-node (find "workflow/support-triage/plan/node-b"
                            (claw-lisp.ir.schema:ir-graph-nodes plan)
                            :key #'claw-lisp.ir.schema:ir-node-id
                            :test #'string=)))
    (%assert review-node
             "Expected execution-plan lowering to retain the human-review node")
    (%assert (eq :plan-human-review
                 (claw-lisp.ir.schema:ir-node-kind review-node))
             "Expected semantic :human-review to lower into :plan-human-review")
    (%assert (equal '(:prompt "Approve this response?" :review-id "review-1")
                    (claw-lisp.ir.schema:ir-node-payload review-node))
             "Expected human-review payload to carry through lowering unchanged"))
  (format t "~&+ test-achatina-execution-plan-lowering-supports-human-review-nodes passed~%")
  t)

(defun test-achatina-execution-plan-lowering-supports-child-agent-nodes ()
  (let* ((graph (claw-lisp.ir.schema:make-optimized-ir
                 :id "workflow-achatina-child-agent"
                 :nodes (list (claw-lisp.ir.schema:make-ir-node
                               :id "node-a"
                               :kind :input
                               :payload '(:text "hello"))
                              (claw-lisp.ir.schema:make-ir-node
                               :id "node-b"
                               :kind :agent-spawn
                               :payload '(:agent "worker"
                                          :prompt "Investigate this issue"
                                          :handoff "summary")))
                 :edges (list (claw-lisp.ir.schema:make-ir-edge
                               :from-id "node-a"
                               :to-id "node-b"
                               :kind :control))
                 :metadata '(:artifact-identity "workflow/support-triage"
                             :pipeline-stage :optimized-ir)))
         (context (claw-lisp.ir.expander:make-ir-pass-context
                   :identity "workflow/support-triage"))
         (plan (claw-lisp.ir.expander:ir-pass-result-graph
                (claw-lisp.ir.execution-plan:lower-to-execution-plan graph context)))
         (child-node (find "workflow/support-triage/plan/node-b"
                           (claw-lisp.ir.schema:ir-graph-nodes plan)
                           :key #'claw-lisp.ir.schema:ir-node-id
                           :test #'string=)))
    (%assert child-node
             "Expected execution-plan lowering to retain the child-agent node")
    (%assert (eq :plan-child-agent
                 (claw-lisp.ir.schema:ir-node-kind child-node))
             "Expected semantic :agent-spawn to lower into :plan-child-agent")
    (%assert (equal '(:agent "worker" :prompt "Investigate this issue" :handoff "summary")
                    (claw-lisp.ir.schema:ir-node-payload child-node))
             "Expected child-agent payload to carry through lowering unchanged"))
  (format t "~&+ test-achatina-execution-plan-lowering-supports-child-agent-nodes passed~%")
  t)

(defun test-achatina-execution-plan-lowering-supports-await-nodes ()
  (let* ((graph (claw-lisp.ir.schema:make-optimized-ir
                 :id "workflow-achatina-await"
                 :nodes (list (claw-lisp.ir.schema:make-ir-node
                               :id "node-a"
                               :kind :input
                               :payload '(:text "hello"))
                              (claw-lisp.ir.schema:make-ir-node
                               :id "node-b"
                               :kind :checkpoint
                               :payload '(:checkpoint "before-review")))
                 :edges (list (claw-lisp.ir.schema:make-ir-edge
                               :from-id "node-a"
                               :to-id "node-b"
                               :kind :control))
                 :metadata '(:artifact-identity "workflow/support-triage"
                             :pipeline-stage :optimized-ir)))
         (context (claw-lisp.ir.expander:make-ir-pass-context
                   :identity "workflow/support-triage"))
         (plan (claw-lisp.ir.expander:ir-pass-result-graph
                (claw-lisp.ir.execution-plan:lower-to-execution-plan graph context)))
         (await-node (find "workflow/support-triage/plan/node-b"
                           (claw-lisp.ir.schema:ir-graph-nodes plan)
                           :key #'claw-lisp.ir.schema:ir-node-id
                           :test #'string=)))
    (%assert await-node
             "Expected execution-plan lowering to retain the checkpoint node")
    (%assert (eq :plan-await
                 (claw-lisp.ir.schema:ir-node-kind await-node))
             "Expected semantic :checkpoint to lower into :plan-await")
    (%assert (equal '(:checkpoint "before-review")
                    (claw-lisp.ir.schema:ir-node-payload await-node))
             "Expected checkpoint payload to carry through lowering unchanged"))
  (format t "~&+ test-achatina-execution-plan-lowering-supports-await-nodes passed~%")
  t)

(defun test-achatina-execution-plan-lowering-supports-memory-read-nodes ()
  (let* ((graph (claw-lisp.ir.schema:make-optimized-ir
                 :id "workflow-achatina-memory-read"
                 :nodes (list (claw-lisp.ir.schema:make-ir-node
                               :id "node-a"
                               :kind :input
                               :payload '(:text "hello"))
                              (claw-lisp.ir.schema:make-ir-node
                               :id "node-b"
                               :kind :memory-read
                               :payload '(:scope :session-memory
                                          :query "recent findings")))
                 :edges (list (claw-lisp.ir.schema:make-ir-edge
                               :from-id "node-a"
                               :to-id "node-b"
                               :kind :control))
                 :metadata '(:artifact-identity "workflow/support-triage"
                             :pipeline-stage :optimized-ir
                             :memory-scopes (:session-memory :durable-memory))))
         (context (claw-lisp.ir.expander:make-ir-pass-context
                   :identity "workflow/support-triage"))
         (plan (claw-lisp.ir.expander:ir-pass-result-graph
                (claw-lisp.ir.execution-plan:lower-to-execution-plan graph context)))
         (memory-node (find "workflow/support-triage/plan/node-b"
                            (claw-lisp.ir.schema:ir-graph-nodes plan)
                            :key #'claw-lisp.ir.schema:ir-node-id
                            :test #'string=))
         (plan-metadata (claw-lisp.ir.schema:ir-graph-metadata plan)))
    (%assert memory-node
             "Expected execution-plan lowering to retain the memory-read node")
    (%assert (eq :plan-memory-read
                 (claw-lisp.ir.schema:ir-node-kind memory-node))
             "Expected semantic :memory-read to lower into :plan-memory-read")
    (%assert (equal '(:scope :session-memory :query "recent findings")
                    (claw-lisp.ir.schema:ir-node-payload memory-node))
             "Expected memory-read payload to carry through lowering unchanged")
    (%assert (equal '(:durable-memory :session-memory)
                    (getf plan-metadata :memory-scopes))
             "Expected execution-plan lowering to retain normalized memory-scope metadata"))
  (format t "~&+ test-achatina-execution-plan-lowering-supports-memory-read-nodes passed~%")
  t)

(defun test-achatina-execution-plan-lowering-supports-memory-write-nodes ()
  (let* ((graph (claw-lisp.ir.schema:make-optimized-ir
                 :id "workflow-achatina-memory-write"
                 :nodes (list (claw-lisp.ir.schema:make-ir-node
                               :id "node-a"
                               :kind :input
                               :payload '(:text "hello"))
                              (claw-lisp.ir.schema:make-ir-node
                               :id "node-b"
                               :kind :memory-write
                               :payload '(:scope :session-memory
                                          :content "store this insight")))
                 :edges (list (claw-lisp.ir.schema:make-ir-edge
                               :from-id "node-a"
                               :to-id "node-b"
                               :kind :control))
                 :metadata '(:artifact-identity "workflow/support-triage"
                             :pipeline-stage :optimized-ir
                             :memory-scopes (:session-memory :durable-memory))))
         (context (claw-lisp.ir.expander:make-ir-pass-context
                   :identity "workflow/support-triage"))
         (plan (claw-lisp.ir.expander:ir-pass-result-graph
                (claw-lisp.ir.execution-plan:lower-to-execution-plan graph context)))
         (memory-node (find "workflow/support-triage/plan/node-b"
                            (claw-lisp.ir.schema:ir-graph-nodes plan)
                            :key #'claw-lisp.ir.schema:ir-node-id
                            :test #'string=))
         (plan-metadata (claw-lisp.ir.schema:ir-graph-metadata plan)))
    (%assert memory-node
             "Expected execution-plan lowering to retain the memory-write node")
    (%assert (eq :plan-memory-write
                 (claw-lisp.ir.schema:ir-node-kind memory-node))
             "Expected semantic :memory-write to lower into :plan-memory-write")
    (%assert (equal '(:scope :session-memory :content "store this insight")
                    (claw-lisp.ir.schema:ir-node-payload memory-node))
             "Expected memory-write payload to carry through lowering unchanged")
    (%assert (equal '(:durable-memory :session-memory)
                    (getf plan-metadata :memory-scopes))
             "Expected execution-plan lowering to retain normalized memory-scope metadata"))
  (format t "~&+ test-achatina-execution-plan-lowering-supports-memory-write-nodes passed~%")
  t)

(defun test-achatina-execution-plan-lowering-supports-side-effect-nodes ()
  (let* ((graph (claw-lisp.ir.schema:make-optimized-ir
                 :id "workflow-achatina-side-effect"
                 :nodes (list (claw-lisp.ir.schema:make-ir-node
                               :id "node-a"
                               :kind :input
                               :payload '(:text "hello"))
                              (claw-lisp.ir.schema:make-ir-node
                               :id "node-b"
                               :kind :side-effect
                               :payload '(:effect :notify
                                          :channel "ops"
                                          :message "workflow completed")))
                 :edges (list (claw-lisp.ir.schema:make-ir-edge
                               :from-id "node-a"
                               :to-id "node-b"
                               :kind :control))
                 :metadata '(:artifact-identity "workflow/support-triage"
                             :pipeline-stage :optimized-ir)))
         (context (claw-lisp.ir.expander:make-ir-pass-context
                   :identity "workflow/support-triage"))
         (plan (claw-lisp.ir.expander:ir-pass-result-graph
                (claw-lisp.ir.execution-plan:lower-to-execution-plan graph context)))
         (effect-node (find "workflow/support-triage/plan/node-b"
                            (claw-lisp.ir.schema:ir-graph-nodes plan)
                            :key #'claw-lisp.ir.schema:ir-node-id
                            :test #'string=)))
    (%assert effect-node
             "Expected execution-plan lowering to retain the side-effect node")
    (%assert (eq :plan-side-effect
                 (claw-lisp.ir.schema:ir-node-kind effect-node))
             "Expected semantic :side-effect to lower into :plan-side-effect")
    (%assert (equal '(:effect :notify :channel "ops" :message "workflow completed")
                    (claw-lisp.ir.schema:ir-node-payload effect-node))
             "Expected side-effect payload to carry through lowering unchanged"))
  (format t "~&+ test-achatina-execution-plan-lowering-supports-side-effect-nodes passed~%")
  t)

(defun test-achatina-execution-plan-lowering-deduplicates-control-edges ()
  (let* ((graph (claw-lisp.ir.schema:make-semantic-ir
                 :id "workflow-achatina-control-dedup"
                 :nodes (list (claw-lisp.ir.schema:make-ir-node
                               :id "node-a"
                               :kind :input
                               :payload '(:text "hello"))
                              (claw-lisp.ir.schema:make-ir-node
                               :id "node-b"
                               :kind :tool-call
                               :payload '(:tool "echo")))
                 :edges (list (claw-lisp.ir.schema:make-ir-edge
                               :from-id "node-a"
                               :to-id "node-b"
                               :kind :control)
                              (claw-lisp.ir.schema:make-ir-edge
                               :from-id "node-a"
                               :to-id "node-b"
                               :kind :control))
                 :metadata '(:artifact-identity "workflow/support-triage"
                             :pipeline-stage :semantic-ir)))
         (context (claw-lisp.ir.expander:make-ir-pass-context
                   :identity "workflow/support-triage"))
         (plan (claw-lisp.ir.expander:ir-pass-result-graph
                (claw-lisp.ir.execution-plan:lower-to-execution-plan graph context)))
         (edges (claw-lisp.ir.schema:ir-graph-edges plan)))
    (%assert (= 1 (length edges))
             "Expected duplicate semantic :control edges to collapse into one plan-control edge")
    (%assert (eq :plan-control
                 (claw-lisp.ir.schema:ir-edge-kind (first edges)))
             "Expected the surviving lowered control edge to be :plan-control"))
  (format t "~&+ test-achatina-execution-plan-lowering-deduplicates-control-edges passed~%")
  t)

(defun test-achatina-execution-plan-persistence-links-parent ()
  (%with-temp-cas-artifact-roots (cas-root ref-root)
    (let* ((config (make-test-runtime-config
                    :cas-objects-root (namestring cas-root)
                    :cas-ref-root (namestring ref-root)))
           (runtime (make-runtime :config config))
           (graph (%make-test-achatina-semantic-graph))
           (context (claw-lisp.ir.expander:make-ir-pass-context
                     :identity "workflow/support-triage")))
      (multiple-value-bind (semantic-artifact semantic-info)
          (claw-lisp.ir.cas-bridge:materialize-ir-to-cas
           runtime graph
           :ref-name "ir/workflows/support-triage/semantic/current"
           :metadata '(:pipeline-stage :semantic-ir))
        (declare (ignore semantic-info))
        (multiple-value-bind (result persisted)
            (claw-lisp.ir.execution-plan:persist-execution-plan
             runtime
             (claw-lisp.ir.expander:ir-pass-result-graph
              (claw-lisp.ir.semantic:expand-semantic-ir graph context))
             context
             :parent-artifact semantic-artifact
             :ref-name "ir/workflows/support-triage/execution/current"
             :manifest-ref-name "manifests/workflows/support-triage/execution/current")
          (let* ((plan (claw-lisp.ir.expander:ir-pass-result-graph result))
                 (plan-metadata (claw-lisp.ir.schema:ir-graph-metadata plan))
                 (graph-artifact (getf persisted :graph))
                 (manifest-artifact (getf persisted :manifest))
                 (manifest (claw-lisp.cas.manifest:load-manifest
                            cas-root
                            (claw-lisp.core.domain:artifact-cas-hash manifest-artifact)))
                 (roles (mapcar #'claw-lisp.cas.manifest:manifest-entry-role
                                (claw-lisp.cas.manifest:manifest-entries manifest))))
            (%assert (string= (claw-lisp.core.domain:artifact-cas-hash semantic-artifact)
                              (getf plan-metadata :parent-ir-hash))
                     "Expected execution plan metadata to point at semantic parent hash")
            (%assert (eq :execution-plan
                         (claw-lisp.core.domain:artifact-kind graph-artifact))
                     "Expected persisted graph artifact kind to be :execution-plan")
            (%assert (member :semantic-ir roles :test #'eq)
                     "Expected manifest to retain the semantic parent role")
            (%assert (member :execution-plan roles :test #'eq)
                     "Expected manifest to include the execution-plan artifact role")
            (let ((loaded (claw-lisp.ir.cas-bridge:load-ir-from-cas
                           runtime
                           (claw-lisp.core.domain:artifact-cas-hash graph-artifact))))
              (%assert (string=
                        (claw-lisp.ir.schema:ir-object->canonical-string plan)
                        (claw-lisp.ir.schema:ir-object->canonical-string loaded))
                       "Expected persisted execution-plan IR to round-trip through CAS unchanged")))))))
  (format t "~&+ test-achatina-execution-plan-persistence-links-parent passed~%")
  t)

(defun test-achatina-execution-plan-persistence-prefers-context-parent ()
  (%with-temp-cas-artifact-roots (cas-root ref-root)
    (let* ((config (make-test-runtime-config
                    :cas-objects-root (namestring cas-root)
                    :cas-ref-root (namestring ref-root)))
           (runtime (make-runtime :config config))
           (graph (%make-test-achatina-semantic-graph))
           (artifact-a (nth-value 0
                                  (claw-lisp.ir.cas-bridge:materialize-ir-to-cas
                                   runtime graph
                                   :ref-name "ir/workflows/support-triage/semantic/a")))
           (artifact-b (nth-value 0
                                  (claw-lisp.ir.cas-bridge:materialize-ir-to-cas
                                   runtime graph
                                   :ref-name "ir/workflows/support-triage/semantic/b")))
           (context (claw-lisp.ir.expander:make-ir-pass-context
                     :identity "workflow/support-triage"
                     :parent-ir-hash (claw-lisp.core.domain:artifact-cas-hash artifact-a)
                     :parent-ir-ref-name (claw-lisp.core.domain:artifact-cas-ref-name artifact-a)))
           (expanded (claw-lisp.ir.expander:ir-pass-result-graph
                      (claw-lisp.ir.semantic:expand-semantic-ir graph
                                                                (claw-lisp.ir.expander:make-ir-pass-context
                                                                 :identity "workflow/support-triage")))))
      (multiple-value-bind (result persisted)
          (claw-lisp.ir.execution-plan:persist-execution-plan
           runtime
           expanded
           context
           :parent-artifact artifact-b)
        (declare (ignore persisted))
        (%assert (string= (claw-lisp.core.domain:artifact-cas-hash artifact-a)
                          (getf (claw-lisp.ir.schema:ir-graph-metadata
                                 (claw-lisp.ir.expander:ir-pass-result-graph result))
                                :parent-ir-hash))
                 "Expected explicit context parent hash to win over parent-artifact seeding"))))
  (format t "~&+ test-achatina-execution-plan-persistence-prefers-context-parent passed~%")
  t)

(defun test-achatina-execution-plan-lowering-rejects-unsupported-kinds ()
  (let* ((graph (claw-lisp.ir.schema:make-semantic-ir
                 :id "workflow-unsupported-lowering"
                 :nodes (list (claw-lisp.ir.schema:make-ir-node
                              :id "node-a"
                              :kind :input
                              :payload '(:text "hello"))
                             (claw-lisp.ir.schema:make-ir-node
                               :id "node-b"
                               ;; :tool-result is intentionally data-shaped and
                               ;; not an operational execution-plan node kind.
                               :kind :tool-result
                               :payload '(:status :ok :value "ignored")))
                 :edges (list (claw-lisp.ir.schema:make-ir-edge
                               :from-id "node-a"
                               :to-id "node-b"
                               :kind :control))
                 :metadata '(:artifact-identity "workflow/support-triage"
                             :pipeline-stage :semantic-ir)))
         (context (claw-lisp.ir.expander:make-ir-pass-context
                   :identity "workflow/support-triage"))
         (signaled nil))
    (handler-case
        (claw-lisp.ir.execution-plan:lower-to-execution-plan graph context)
      (claw-lisp.ir.conditions:ir-validation-error ()
        (setf signaled t)))
    (%assert signaled
             "Expected unsupported execution-plan lowering constructs to signal ir-validation-error"))
  (format t "~&+ test-achatina-execution-plan-lowering-rejects-unsupported-kinds passed~%")
  t)

(defun test-achatina-execution-plan-lowering-rejects-unsupported-edge-kind ()
  (let* ((graph (claw-lisp.ir.schema:make-semantic-ir
                 :id "workflow-unsupported-edge-lowering"
                 :nodes (list (claw-lisp.ir.schema:make-ir-node
                               :id "node-a"
                               :kind :input
                               :payload '(:text "hello"))
                              (claw-lisp.ir.schema:make-ir-node
                               :id "node-b"
                               :kind :tool-call
                               :payload '(:tool "echo")))
                 :edges (list (claw-lisp.ir.schema:make-ir-edge
                               :from-id "node-a"
                               :to-id "node-b"
                               :kind :branch))
                 :metadata '(:artifact-identity "workflow/support-triage"
                             :pipeline-stage :semantic-ir)))
         (context (claw-lisp.ir.expander:make-ir-pass-context
                   :identity "workflow/support-triage"))
         (signaled nil)
         (reason nil))
    (handler-case
        (claw-lisp.ir.execution-plan:lower-to-execution-plan graph context)
      (claw-lisp.ir.conditions:ir-validation-error (condition)
        (setf signaled t
              reason (claw-lisp.ir.conditions:ir-validation-error-reason
                      condition))))
    (%assert signaled
             "Expected unsupported edge kinds to signal ir-validation-error even when node kinds are valid")
    (%assert (and reason
                  (search ":BRANCH" reason)
                  (search ":CONTROL" reason))
             "Expected unsupported :branch rejection to explain the current :control-based lowering contract"))
  (format t "~&+ test-achatina-execution-plan-lowering-rejects-unsupported-edge-kind passed~%")
  t)

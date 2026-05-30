(in-package #:claw-lisp.ir.local-execution)

(defun %validation-error (subject format-string &rest args)
  (error 'ir-validation-error
         :subject subject
         :reason (apply #'format nil format-string args)))

(defun %safe-local-session-id (plan)
  "Return a runtime-safe local session id for PLAN.
Local execution should preserve workflow identity in IR/provenance, but runtime
session ids also flow into transcript and memory pathnames, so path separators
must not be carried through verbatim."
  (let ((graph-id (or (ir-graph-id plan) "workflow")))
    (labels ((safe-char-p (ch)
               (or (alphanumericp ch)
                   (find ch "-._" :test #'char=))))
      (let* ((normalized
               (with-output-to-string (out)
                 (loop for ch across graph-id
                       do (write-char (if (safe-char-p ch) ch #\-) out))))
             (trimmed (string-right-trim "-" normalized))
             (base (if (> (length trimmed) 0) trimmed "workflow"))
             (capped (if (> (length base) 96)
                         (subseq base 0 96)
                         base)))
        (format nil "~A-local" capped)))))

(defun %ensure-runtime-session-paths (runtime session)
  (ensure-directories-exist
   (claw-lisp.core.runtime:session-transcript-path runtime session))
  (ensure-directories-exist
   (claw-lisp.core.runtime:session-memory-path-for-session runtime session)))

(defun %execution-node-table (plan)
  (let ((table (make-hash-table :test #'equal)))
    (dolist (node (ir-graph-nodes plan) table)
      (setf (gethash (ir-node-id node) table) node))))

(defun %incoming-edges (plan)
  (let ((table (make-hash-table :test #'equal)))
    (dolist (node (ir-graph-nodes plan))
      (setf (gethash (ir-node-id node) table) '()))
    (dolist (edge (ir-graph-edges plan) table)
      (push edge (gethash (ir-edge-to-id edge) table)))))

(defun %topological-order (plan)
  (let ((node-table (%execution-node-table plan))
        (adjacency (make-hash-table :test #'equal))
        (indegree (make-hash-table :test #'equal))
        (ready-node-ids '())
        (enqueued (make-hash-table :test #'equal))
        (ordered '()))
    (dolist (node (ir-graph-nodes plan))
      (setf (gethash (ir-node-id node) indegree) 0))
    (dolist (edge (ir-graph-edges plan))
      (push edge (gethash (ir-edge-from-id edge) adjacency))
      (incf (gethash (ir-edge-to-id edge) indegree 0)))
    (dolist (node (sort (copy-list (ir-graph-nodes plan)) #'string< :key #'ir-node-id))
      (when (zerop (gethash (ir-node-id node) indegree 0))
        (push (ir-node-id node) ready-node-ids)
        (setf (gethash (ir-node-id node) enqueued) t)))
    (setf ready-node-ids (sort ready-node-ids #'string<))
    (loop while ready-node-ids
          for node-id = (pop ready-node-ids)
          for node = (gethash node-id node-table)
          do (unless node
               (%validation-error (ir-graph-id plan)
                                  "Execution plan ~S references missing node ~S during local execution ordering."
                                  (ir-graph-id plan)
                                  node-id))
             (push node ordered)
             (dolist (edge (gethash node-id adjacency))
               (let ((remaining (decf (gethash (ir-edge-to-id edge) indegree))))
                 (when (minusp remaining)
                   (%validation-error (ir-graph-id plan)
                                      "Execution plan ~S has invalid indegree bookkeeping for node ~S."
                                      (ir-graph-id plan)
                                      (ir-edge-to-id edge)))
                 (when (and (zerop remaining)
                            (not (gethash (ir-edge-to-id edge) enqueued)))
                   (push (ir-edge-to-id edge) ready-node-ids)
                   (setf (gethash (ir-edge-to-id edge) enqueued) t))))
             (setf ready-node-ids (sort ready-node-ids #'string<)))
    (unless (= (length ordered) (length (ir-graph-nodes plan)))
      (%validation-error (ir-graph-id plan)
                         "Execution plan ~S contains a cycle or unreachable dependency chain."
                         (ir-graph-id plan)))
    (nreverse ordered)))

(defun %plan-tool-input (incoming-edges node node-results)
  (let ((payload (ir-node-payload node)))
    (or (getf payload :input)
        (let* ((edges (gethash (ir-node-id node) incoming-edges '()))
               (predecessors
                 (remove nil
                         (mapcar (lambda (edge)
                                   (and (eq (ir-edge-kind edge) :plan-flow)
                                        (list :node-id (ir-edge-from-id edge)
                                              :value (gethash (ir-edge-from-id edge) node-results))))
                                 edges))))
          (when (> (length predecessors) 1)
            (%validation-error (ir-node-id node)
                               "Tool-call node ~S has ambiguous predecessor-derived input from ~S."
                               (ir-node-id node)
                               (mapcar (lambda (entry) (getf entry :node-id)) predecessors)))
          (when (null predecessors)
            (%validation-error (ir-node-id node)
                               "Tool-call node ~S has no explicit :input and no predecessor-derived :plan-flow input."
                               (ir-node-id node)))
          (copy-tree (getf (first predecessors) :value))))))

(defun %memory-plan-node-kind-p (kind)
  (member kind '(:plan-memory-read :plan-memory-write) :test #'eq))

(defun %pathname-parent-directory (path)
  (make-pathname :name nil :type nil :version nil :defaults path))

(defun %execute-plan-memory-read (runtime session node)
  (let* ((payload (ir-node-payload node))
         (scope (getf payload :scope))
         (query (getf payload :query)))
    (case scope
      (:session-memory
       (list :scope :session-memory
             :query query
             :content (claw-lisp.core.runtime:read-session-memory-text runtime session)))
      (t
       (%validation-error (ir-node-id node)
                          "Execution-plan memory-read scope ~S is not yet supported for local execution."
                          scope)))))

(defun %execute-plan-memory-write (runtime session node)
  "Execute a local session-memory write for NODE.
For the current narrow runtime contract, `:scope :session-memory` means the
provided `:content` replaces the entire session-memory note for SESSION. This is
an explicit overwrite contract, not append/merge semantics."
  (let* ((payload (ir-node-payload node))
         (scope (getf payload :scope))
         (content (getf payload :content)))
    (case scope
      (:session-memory
       (unless (stringp content)
         (%validation-error (ir-node-id node)
                            "Execution-plan memory-write node ~S requires string :content for local :session-memory writes."
                            (ir-node-id node)))
       (let ((path (claw-lisp.core.runtime:session-memory-path-for-session runtime session)))
         ;; Intentional overwrite contract: session-memory writes replace the
         ;; entire note, rather than appending or merging into prior content.
         (ensure-directories-exist (%pathname-parent-directory path))
         (with-open-file (stream path
                                 :direction :output
                                 :if-exists :supersede
                                 :if-does-not-exist :create)
           (write-string content stream))
         ;; :path is returned for write-side diagnostics/observability only; the
         ;; read path remains content-oriented and does not promise symmetry.
         (list :scope :session-memory
               :content content
               :path (namestring path))))
      (t
       (%validation-error (ir-node-id node)
                          "Execution-plan memory-write scope ~S is not yet supported for local execution."
                          scope)))))

(defun %execute-plan-await (node)
  "Execute a narrow local checkpoint marker for NODE.
This does not introduce resume/orchestration semantics. It only records that the
await/checkpoint boundary was reached during local execution."
  (let ((payload (ir-node-payload node)))
    (list :awaited t
          :checkpoint (getf payload :checkpoint)
          :payload (copy-tree payload))))

(defun %execute-plan-human-review (node)
  "Execute a narrow local human-review marker for NODE.
This records that a review boundary was reached, but does not implement
approval, pause/resume, or external reviewer orchestration semantics."
  (let ((payload (ir-node-payload node)))
    (unless (stringp (getf payload :review-id))
      (%validation-error (ir-node-id node)
                         "Execution-plan human-review node ~S requires string :review-id for local execution."
                         (ir-node-id node)))
    (unless (stringp (getf payload :prompt))
      (%validation-error (ir-node-id node)
                         "Execution-plan human-review node ~S requires string :prompt for local execution."
                         (ir-node-id node)))
    (list :human-review-required t
          :review-id (getf payload :review-id)
          :prompt (getf payload :prompt)
          :payload (copy-tree payload))))

(defun %execute-plan-child-agent (node)
  "Execute a narrow local child-agent marker for NODE.
This records that a child-agent boundary was reached, but does not implement
spawn/orchestration, nested runtime execution, or handoff semantics. It is a
pure non-blocking marker and does not alter later execution, memory, or tool
results."
  (let ((payload (ir-node-payload node)))
    (unless (stringp (getf payload :agent))
      (%validation-error (ir-node-id node)
                         "Execution-plan child-agent node ~S requires string :agent for local execution."
                         (ir-node-id node)))
    (unless (stringp (getf payload :prompt))
      (%validation-error (ir-node-id node)
                         "Execution-plan child-agent node ~S requires string :prompt for local execution."
                         (ir-node-id node)))
    (let ((handoff (getf payload :handoff)))
      (unless (or (null handoff) (stringp handoff))
        (%validation-error (ir-node-id node)
                           "Execution-plan child-agent node ~S requires string :handoff when present."
                           (ir-node-id node))))
    (list :child-agent-required t
          :agent (getf payload :agent)
          :prompt (getf payload :prompt)
          :handoff (getf payload :handoff)
          :payload (copy-tree payload))))

(defun %execute-plan-side-effect (node)
  "Execute a narrow local side-effect marker for NODE.
This records that a side-effect boundary was reached, but does not perform the
effect. It is a pure non-blocking marker and does not alter later execution,
memory, or tool results."
  (let ((payload (ir-node-payload node)))
    (unless (keywordp (getf payload :effect))
      (%validation-error (ir-node-id node)
                         "Execution-plan side-effect node ~S requires keyword :effect for local execution."
                         (ir-node-id node)))
    (list :side-effect-required t
          :effect (getf payload :effect)
          :payload (copy-tree payload))))

(defun execute-execution-plan-locally (runtime plan &key session)
  "Execute the currently supported local execution-plan subset through RUNTIME.
This local proof path currently executes `:plan-input` and `:plan-tool-call`
nodes, plus `:plan-memory-read` and `:plan-memory-write` for
`:scope :session-memory`, plus `:plan-await` as an explicit checkpoint marker
and `:plan-human-review` as an explicit review marker. These narrow local
contracts do not implement orchestration, approval, or resume semantics.
`:plan-child-agent` also supports an explicit spawn marker with no nested
execution semantics. `:plan-side-effect` supports an explicit effect marker
with no performed side effect. Other execution-plan node kinds, including
`:plan-model-call` and `:plan-branch`, are kept explicit and rejected until
dedicated runtime contracts are added."
  (unless (eq (ir-graph-node-type plan) :execution-plan)
    (%validation-error (ir-graph-id plan)
                       "Local execution expects an :execution-plan graph, got ~S."
                       (ir-graph-node-type plan)))
  (let ((effective-session (or session
                               (claw-lisp.core.runtime:start-session
                                runtime
                                :session-id (%safe-local-session-id plan))))
        (incoming-edges (%incoming-edges plan))
        (node-results (make-hash-table :test #'equal))
        (tool-results '()))
    (%ensure-runtime-session-paths runtime effective-session)
    (dolist (node (%topological-order plan))
      (case (ir-node-kind node)
        (:plan-input
         (setf (gethash (ir-node-id node) node-results)
               (copy-tree (ir-node-payload node))))
        (:plan-tool-call
         (let* ((payload (ir-node-payload node))
                (tool-name (or (getf payload :tool)
                               (getf payload :tool-name)))
                (input (%plan-tool-input incoming-edges node node-results))
                (result (claw-lisp.core.runtime:execute-registered-tool
                         runtime
                         effective-session
                         tool-name
                         input)))
           (setf (gethash (ir-node-id node) node-results) result)
           (push result tool-results)))
        (:plan-memory-read
         (setf (gethash (ir-node-id node) node-results)
               (%execute-plan-memory-read runtime effective-session node)))
        (:plan-memory-write
         (setf (gethash (ir-node-id node) node-results)
               (%execute-plan-memory-write runtime effective-session node)))
        (:plan-await
         (setf (gethash (ir-node-id node) node-results)
               (%execute-plan-await node)))
        (:plan-human-review
         (setf (gethash (ir-node-id node) node-results)
               (%execute-plan-human-review node)))
        (:plan-child-agent
         (setf (gethash (ir-node-id node) node-results)
               (%execute-plan-child-agent node)))
        (:plan-side-effect
         (setf (gethash (ir-node-id node) node-results)
               (%execute-plan-side-effect node)))
        (t
         (if (%memory-plan-node-kind-p (ir-node-kind node))
             (%validation-error (ir-node-id node)
                                "Execution-plan node kind ~S is not yet supported for local execution; memory runtime contract not implemented."
                                (ir-node-kind node))
             (%validation-error (ir-node-id node)
                                "Unsupported execution-plan node kind ~S for local execution."
                                (ir-node-kind node))))))
    (list :session effective-session
          :node-results node-results
          :tool-results (nreverse tool-results))))

(defun run-workflow-form-locally (runtime form &key semantic-ref-name execution-plan-ref-name
                                                  execution-plan-manifest-ref-name
                                                  register-default-tools-p)
  "Run FORM through surface form -> IR -> CAS -> execution plan -> local runtime."
  (when register-default-tools-p
    (claw-lisp.core.runtime:register-default-tools runtime))
  (let* ((context (make-workflow-form-context form))
         (semantic-graph (compile-minimal-workflow-form form))
         (semantic-artifact
           (first (multiple-value-list
                   (materialize-ir-to-cas runtime
                                          semantic-graph
                                          :ref-name semantic-ref-name
                                          :metadata '(:pipeline-stage :semantic-ir)))))
         (expanded (ir-pass-result-graph
                    (expand-semantic-ir semantic-graph context)))
         (validated (prepare-validated-ir expanded context))
         (validated-artifact
           (getf (persist-ir-pass-result runtime
                                         validated
                                         :parent-artifact semantic-artifact
                                         :ref-name nil
                                         :manifest-ref-name nil)
                 :graph))
         (optimized-run
           (optimize-validated-ir-pipeline
            (ir-pass-result-graph validated)
            context
            :runtime runtime
            :initial-parent-artifact validated-artifact))
         (optimized-result (ir-pipeline-run-final-result optimized-run))
         (optimized-artifact (ir-pipeline-run-final-graph-artifact optimized-run))
         (plan-result-and-bundle
           (multiple-value-list
            (persist-execution-plan runtime
                                    (ir-pass-result-graph optimized-result)
                                    context
                                    :parent-artifact optimized-artifact
                                    :ref-name execution-plan-ref-name
                                    :manifest-ref-name execution-plan-manifest-ref-name)))
         (plan-result (first plan-result-and-bundle))
         (plan-bundle (second plan-result-and-bundle))
         (plan-artifact (getf plan-bundle :graph))
         (loaded-plan (load-ir-from-cas runtime (artifact-cas-hash plan-artifact)))
         (execution (execute-execution-plan-locally runtime loaded-plan)))
    (list :semantic-graph semantic-graph
          :semantic-artifact semantic-artifact
          :validated-result validated
          :validated-artifact validated-artifact
          :optimized-result optimized-result
          :optimized-artifact optimized-artifact
          :execution-plan-result plan-result
          :execution-plan-bundle plan-bundle
          :loaded-plan loaded-plan
          :execution execution)))

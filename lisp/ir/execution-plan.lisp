(in-package #:claw-lisp.ir.execution-plan)

(defparameter +governance-node-kinds+
  '(:semantic-anchor
    :capability-declaration
    :policy-declaration
    :tool-declaration
    :memory-scope-declaration))

(defparameter +governance-edge-kinds+
  '(:declares-capability
    :declares-policy
    :declares-tool
    :declares-memory-scope))

(defun %validation-error (subject format-string &rest args)
  (error 'ir-validation-error
         :subject subject
         :reason (apply #'format nil format-string args)))

(defun %unsupported-edge-kind-message (kind)
  (case kind
    (:branch
     "Unsupported edge kind :BRANCH for execution-plan lowering. Use explicit :CONTROL edges for minimal control dependencies; richer branching remains a later 10d task.")
    (t
     (format nil
             "Unsupported edge kind ~S for execution-plan lowering."
             kind))))

(defun %supported-node-kind-p (kind)
  (member kind '(:input :tool-call :model-call :branch :human-review :agent-spawn :checkpoint
                  :memory-read :memory-write :side-effect)
          :test #'eq))

(defun %supported-edge-kind-p (kind)
  (member kind '(:flow :control) :test #'eq))

(defun %governance-node-kind-p (kind)
  (member kind +governance-node-kinds+ :test #'eq))

(defun %governance-edge-kind-p (kind)
  (member kind +governance-edge-kinds+ :test #'eq))

(defun %execution-node-kind (kind)
  (case kind
    (:input :plan-input)
    (:tool-call :plan-tool-call)
    (:model-call :plan-model-call)
    (:branch :plan-branch)
    (:human-review :plan-human-review)
    (:agent-spawn :plan-child-agent)
    (:checkpoint :plan-await)
    (:memory-read :plan-memory-read)
    (:memory-write :plan-memory-write)
    (:side-effect :plan-side-effect)
    (t kind)))

(defun %execution-edge-kind (kind)
  (case kind
    (:flow :plan-flow)
    (:control :plan-control)
    (t kind)))

(defun %sorted-plan-nodes (nodes)
  (sort nodes #'string< :key #'ir-node-id))

(defun %sorted-plan-edges (edges)
  "Return deterministic execution-plan edges for the first 10d slice.
Parallel edges with the same `(from-id kind to-id)` triple are intentionally
collapsed here because the current lowering contract models only explicit
`plan-flow` / `plan-control` connectivity, not multi-edge execution semantics.
Different edge kinds between the same nodes are preserved; only duplicate
triples are collapsed."
  (let ((table (make-hash-table :test #'equal))
        (deduped '()))
    (dolist (edge edges)
      (let ((key (format nil "~A|~A|~A"
                         (ir-edge-from-id edge)
                         (ir-edge-kind edge)
                         (ir-edge-to-id edge))))
        (unless (gethash key table)
          (setf (gethash key table) t)
          (push edge deduped))))
    (sort deduped
          #'string<
          :key (lambda (edge)
                 (format nil "~A|~A|~A"
                         (ir-edge-from-id edge)
                         (ir-edge-kind edge)
                         (ir-edge-to-id edge))))))

(defun %execution-node-id (workflow-id source-node-id)
  (format nil "~A/plan/~A" workflow-id source-node-id))

(defun %lower-node (preparation node)
  "Lower a semantic operational NODE into execution-plan form.
This intentionally preserves payload only and replaces node metadata with
execution-plan provenance. Semantic node metadata is not yet part of the 10d
contract and is therefore dropped instead of being carried through silently."
  (make-ir-node
   :id (%execution-node-id (execution-preparation-workflow-id preparation)
                           (ir-node-id node))
   :kind (%execution-node-kind (ir-node-kind node))
   :payload (copy-tree (or (ir-node-payload node) '()))
   :metadata (list :source-node-id (ir-node-id node)
                   :source-kind (ir-node-kind node)
                   :source-stage (execution-preparation-pipeline-stage preparation))))

(defun %lower-edge (preparation edge)
  (make-ir-edge
   :from-id (%execution-node-id (execution-preparation-workflow-id preparation)
                                (ir-edge-from-id edge))
   :to-id (%execution-node-id (execution-preparation-workflow-id preparation)
                              (ir-edge-to-id edge))
   :kind (%execution-edge-kind (ir-edge-kind edge))
   :metadata (list :source-edge-kind (ir-edge-kind edge)
                   :source-stage (execution-preparation-pipeline-stage preparation))))

(defun %lower-execution-plan-graph (preparation)
  (let ((nodes '())
        (edges '())
        (graph (execution-preparation-graph preparation)))
    (dolist (node (ir-graph-nodes graph))
      (cond
        ((%governance-node-kind-p (ir-node-kind node))
         nil)
        ((%supported-node-kind-p (ir-node-kind node))
         (push (%lower-node preparation node) nodes))
        (t
         (%validation-error (ir-graph-id graph)
                            "Unsupported node kind ~S for execution-plan lowering."
                            (ir-node-kind node)))))
    (dolist (edge (ir-graph-edges graph))
      (cond
        ((%governance-edge-kind-p (ir-edge-kind edge))
         nil)
        ((%supported-edge-kind-p (ir-edge-kind edge))
         (push (%lower-edge preparation edge) edges))
        (t
         (%validation-error (ir-graph-id graph)
                            "~A"
                            (%unsupported-edge-kind-message
                             (ir-edge-kind edge))))))
    (make-execution-plan-ir
     :id (ir-graph-id graph)
     :ir-version (ir-graph-ir-version graph)
     :nodes (%sorted-plan-nodes nodes)
     :edges (%sorted-plan-edges edges)
     :metadata (append
                (list :pipeline-stage :execution-plan
                      :lowered-from-stage (execution-preparation-pipeline-stage preparation)
                      :governance-source :prepared-execution-input)
                (when (execution-preparation-parent-ir-hash preparation)
                  (list :parent-ir-hash
                        (execution-preparation-parent-ir-hash preparation)))
                (when (execution-preparation-parent-ir-ref-name preparation)
                  (list :parent-ir-ref-name
                        (execution-preparation-parent-ir-ref-name preparation)))
                (when (execution-preparation-capabilities preparation)
                  (list :capabilities
                        (copy-list (execution-preparation-capabilities preparation))))
                (when (execution-preparation-policies preparation)
                  (list :policies
                        (copy-list (execution-preparation-policies preparation))))
                (when (execution-preparation-tools preparation)
                  (list :tools
                        (copy-list (execution-preparation-tools preparation))))
                (when (execution-preparation-memory-scopes preparation)
                  (list :memory-scopes
                        (copy-list (execution-preparation-memory-scopes preparation))))))))

(defun %context-from-preparation (context preparation)
  (make-ir-pass-context
   :identity (execution-preparation-workflow-id preparation)
   :capabilities (copy-list (execution-preparation-capabilities preparation))
   :policies (copy-list (execution-preparation-policies preparation))
   :tools (copy-list (execution-preparation-tools preparation))
   :memory-scopes (copy-list (execution-preparation-memory-scopes preparation))
   :parent-ir-hash (or (ir-pass-context-parent-ir-hash context)
                       (execution-preparation-parent-ir-hash preparation))
   :parent-ir-ref-name (or (ir-pass-context-parent-ir-ref-name context)
                           (execution-preparation-parent-ir-ref-name preparation))
   :metadata (copy-list (ir-pass-context-metadata context))))

(defun %context-with-parent-artifact (context parent-artifact)
  (make-ir-pass-context
   :identity (ir-pass-context-identity context)
   :capabilities (copy-list (ir-pass-context-capabilities context))
   :policies (copy-list (ir-pass-context-policies context))
   :tools (copy-list (ir-pass-context-tools context))
   :memory-scopes (copy-list (ir-pass-context-memory-scopes context))
   :parent-ir-hash (or (and parent-artifact
                            (claw-lisp.core.domain:artifact-cas-hash parent-artifact))
                       (ir-pass-context-parent-ir-hash context))
   :parent-ir-ref-name (or (and parent-artifact
                                (claw-lisp.core.domain:artifact-cas-ref-name parent-artifact))
                           (ir-pass-context-parent-ir-ref-name context))
   :metadata (copy-list (ir-pass-context-metadata context))))

(defun lower-to-execution-plan (graph context)
  "Lower GRAPH into a deterministic execution-plan IR artifact.
This pass consumes normalized governance from `prepare-execution-input`,
filters governance scaffolding from the semantic graph, and lowers supported
operational node/edge kinds into `:execution-plan` IR.

The lowering graph explicitly stamps execution-plan stage, governance, and
parent provenance metadata so the contract remains visible here even though
`run-achatina-ir-pass` also finalizes reserved metadata from CONTEXT.

In the current slice, `:control` is the only supported explicit control edge
and lowers to `:plan-control`. Supported operational node lowering now covers
`:input`, `:tool-call`, `:model-call`, `:branch`, `:human-review`,
`:agent-spawn`, `:checkpoint` (lowered as `:plan-await`), and
`:memory-read` (lowered as `:plan-memory-read`), and `:memory-write`
(lowered as `:plan-memory-write`), and `:side-effect`
(lowered as `:plan-side-effect`). Richer branch/fanout edge semantics remain
unsupported until later 10d work."
  (let* ((preparation (prepare-execution-input graph))
         (lowering-context (%context-from-preparation context preparation)))
    (run-achatina-ir-pass graph lowering-context :lower-to-execution-plan :execution-plan
                          (lambda (current-graph current-context)
                            (declare (ignore current-graph current-context))
                            (%lower-execution-plan-graph preparation)))))

(defun persist-execution-plan (runtime graph context &key parent-artifact ref-name manifest-ref-name metadata)
  "Lower GRAPH to execution-plan IR and persist the resulting artifact bundle.
If CONTEXT already carries `:parent-ir-hash`, that explicit context provenance
takes precedence over PARENT-ARTIFACT. PARENT-ARTIFACT is used only to seed
missing parent linkage before persistence."
  (let* ((effective-context (if (and parent-artifact
                                     (null (ir-pass-context-parent-ir-hash context)))
                                (%context-with-parent-artifact context parent-artifact)
                                context))
         (result (lower-to-execution-plan graph effective-context)))
    (values result
            (persist-ir-pass-result runtime
                                    result
                                    :parent-artifact parent-artifact
                                    :ref-name ref-name
                                    :manifest-ref-name manifest-ref-name
                                    :metadata metadata))))

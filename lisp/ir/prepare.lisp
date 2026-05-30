(in-package #:claw-lisp.ir.prepare)

(defstruct (execution-preparation
            (:constructor make-execution-preparation
                (&key workflow-id pipeline-stage tools memory-scopes capabilities
                      policies parent-ir-hash parent-ir-ref-name graph)))
  workflow-id
  pipeline-stage
  tools
  memory-scopes
  capabilities
  policies
  parent-ir-hash
  parent-ir-ref-name
  graph)

(defparameter +governance-declaration-kinds+
  '((:declares-capability . :capability-declaration)
    (:declares-policy . :policy-declaration)
    (:declares-tool . :tool-declaration)
    (:declares-memory-scope . :memory-scope-declaration))
  "Mapping from semantic governance edge kinds to their declaration node kinds.")

(defun %validation-error (subject format-string &rest args)
  (error 'ir-validation-error
         :subject subject
         :reason (apply #'format nil format-string args)))

(defun %normalize-string-list (values)
  (sort (remove-duplicates (copy-list values) :test #'string=) #'string<))

(defun %normalize-keyword-list (values)
  (sort (remove-duplicates (copy-list values) :test #'eq)
        #'string<
        :key #'symbol-name))

(defun %semantic-anchor-nodes (graph)
  (remove-if-not (lambda (node)
                   (eq (ir-node-kind node) :semantic-anchor))
                 (ir-graph-nodes graph)))

(defun %find-semantic-anchor-id (graph)
  (let ((anchors (%semantic-anchor-nodes graph)))
    (when (> (length anchors) 1)
      (%validation-error (ir-graph-id graph)
                         "Expected at most one semantic anchor, got ~D."
                         (length anchors)))
    (and anchors (ir-node-id (first anchors)))))

(defun %declaration-node-kind (edge-kind)
  (cdr (assoc edge-kind +governance-declaration-kinds+ :test #'eq)))

(defun %node-table (graph)
  (let ((table (make-hash-table :test #'equal)))
    (dolist (node (ir-graph-nodes graph) table)
      (setf (gethash (ir-node-id node) table) node))))

(defun %payload-name (node graph kind-predicate)
  (let ((payload (ir-node-payload node)))
    (unless (and (listp payload)
                 (funcall kind-predicate (getf payload :name)))
      (%validation-error (ir-graph-id graph)
                         "Declaration node ~S must provide a valid payload :name, got ~S."
                         (ir-node-id node)
                         payload))
    (getf payload :name)))

(defun %collect-declaration-values (graph edge-kind expected-node-kind normalize-fn kind-predicate)
  (let* ((anchor-id (%find-semantic-anchor-id graph))
         (node-table (%node-table graph))
         (matched-node-ids '())
         (values '()))
    (dolist (edge (ir-graph-edges graph))
      (when (eq (ir-edge-kind edge) edge-kind)
        (unless anchor-id
          (%validation-error (ir-graph-id graph)
                             "Graph contains ~S edges but no semantic anchor."
                             edge-kind))
        (unless (string= anchor-id (ir-edge-from-id edge))
          (%validation-error (ir-graph-id graph)
                             "Declaration edge ~S must originate from semantic anchor ~S, got ~S."
                             edge-kind
                             anchor-id
                             (ir-edge-from-id edge)))
        (let ((target (gethash (ir-edge-to-id edge) node-table)))
          (unless target
            (%validation-error (ir-graph-id graph)
                               "Declaration edge ~S references missing node ~S."
                               edge-kind
                               (ir-edge-to-id edge)))
          (unless (eq (ir-node-kind target) expected-node-kind)
            (%validation-error (ir-graph-id graph)
                               "Declaration edge ~S must target a ~S node, got ~S."
                               edge-kind
                               expected-node-kind
                               (ir-node-kind target)))
          (push (ir-node-id target) matched-node-ids)
          (push (%payload-name target graph kind-predicate) values))))
    (dolist (node (ir-graph-nodes graph))
      (when (eq (ir-node-kind node) expected-node-kind)
        (unless anchor-id
          (%validation-error (ir-graph-id graph)
                             "Graph contains declaration nodes of kind ~S but no semantic anchor."
                             expected-node-kind))
        (unless (member (ir-node-id node) matched-node-ids :test #'string=)
          (%validation-error (ir-graph-id graph)
                             "Declaration node ~S of kind ~S is not linked from the semantic anchor."
                             (ir-node-id node)
                             expected-node-kind))))
    (and matched-node-ids
         (funcall normalize-fn values))))

(defun %compare-declarations-to-metadata (graph label declarations metadata normalize-fn test-fn)
  (let ((normalized-metadata (and metadata (funcall normalize-fn metadata))))
    (cond
      ((and declarations normalized-metadata)
       (unless (and (= (length declarations) (length normalized-metadata))
                    (every test-fn declarations normalized-metadata))
         (%validation-error (ir-graph-id graph)
                            "Semantic declarations for ~A conflict with graph metadata: ~S vs ~S."
                            label
                            declarations
                            normalized-metadata))
       declarations)
      (declarations declarations)
      (normalized-metadata normalized-metadata)
      (t nil))))

(defun %prepared-governance-values (graph label edge-kind metadata-key normalize-fn kind-predicate test-fn)
  (let ((expected-node-kind (%declaration-node-kind edge-kind)))
    (unless expected-node-kind
      (%validation-error (ir-graph-id graph)
                         "Unsupported governance declaration edge kind ~S."
                         edge-kind))
    (%compare-declarations-to-metadata
     graph
     label
     (%collect-declaration-values graph edge-kind expected-node-kind
                                  normalize-fn kind-predicate)
     (getf (ir-graph-metadata graph) metadata-key)
     normalize-fn
     test-fn)))

(defun %prepared-capabilities (graph)
  (%prepared-governance-values graph :capabilities :declares-capability :capabilities
                               #'%normalize-string-list #'stringp #'string=))

(defun %prepared-policies (graph)
  (%prepared-governance-values graph :policies :declares-policy :policies
                               #'%normalize-keyword-list #'keywordp #'eq))

(defun %prepared-tools (graph)
  (%prepared-governance-values graph :tools :declares-tool :tools
                               #'%normalize-string-list #'stringp #'string=))

(defun %prepared-memory-scopes (graph)
  (%prepared-governance-values graph :memory-scopes :declares-memory-scope :memory-scopes
                               #'%normalize-keyword-list #'keywordp #'eq))

(defun prepare-execution-input (graph)
  "Build a deterministic execution-preparation view from semantic Achatina IR.
Semantic governance declarations are the primary source of truth for
capabilities, policies, tools, and memory scopes. Graph metadata is accepted
only as a transition fallback when declarations are absent; mixed conflicting
definitions signal `ir-validation-error`.
The semantic anchor is required when governance declaration nodes or edges are
present. Execution preparation currently accepts stages `:semantic-ir`,
`:validated-ir`, and `:optimized-ir`; update this guard when extending the
pre-lowering pipeline contract."
  (validate-achatina-ir-graph graph)
  (let* ((metadata (ir-graph-metadata graph))
         (stage (getf metadata :pipeline-stage)))
    (unless (member stage '(:semantic-ir :validated-ir :optimized-ir) :test #'eq)
      (%validation-error (ir-graph-id graph)
                         "Execution preparation expects semantic/validated/optimized IR, got ~S."
                         stage))
    (make-execution-preparation
     :workflow-id (getf metadata :artifact-identity)
     :pipeline-stage stage
     :tools (%prepared-tools graph)
     :memory-scopes (%prepared-memory-scopes graph)
     :capabilities (%prepared-capabilities graph)
     :policies (%prepared-policies graph)
     :parent-ir-hash (getf metadata :parent-ir-hash)
     :parent-ir-ref-name (getf metadata :parent-ir-ref-name)
     :graph graph)))

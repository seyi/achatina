(in-package #:claw-lisp.ir.validate)

(defparameter +allowed-achatina-pipeline-stages+
  '(:surface-form :expanded-form :semantic-ir :validated-ir :optimized-ir :execution-plan)
  "Allowed staged representation labels for Achatina pipeline artifacts.")

(defun %validation-error (subject format-string &rest args)
  (error 'ir-validation-error
         :subject subject
         :reason (apply #'format nil format-string args)))

(defun %non-empty-string-p (value)
  (and (stringp value)
       (> (length value) 0)))

(defun %string-list-p (value)
  (and (listp value)
       (every #'stringp value)))

(defun %keyword-list-p (value)
  (and (listp value)
       (every #'keywordp value)))

(defun %expected-node-type-for-stage (stage)
  (case stage
    (:semantic-ir :semantic-ir)
    (:validated-ir :validated-ir)
    (:optimized-ir :optimized-ir)
    (:execution-plan :execution-plan)
    (t nil)))

(defun %validate-node (node graph-id)
  (unless (ir-node-p node)
    (%validation-error graph-id "Node ~S is not an ir-node." node))
  (unless (%non-empty-string-p (ir-node-id node))
    (%validation-error graph-id "Node id must be a non-empty string, got ~S." (ir-node-id node)))
  (unless (keywordp (ir-node-kind node))
    (%validation-error graph-id "Node ~A kind must be a keyword, got ~S."
                       (ir-node-id node)
                       (ir-node-kind node))))

(defun %validate-edge (edge graph-id node-id-table)
  (unless (ir-edge-p edge)
    (%validation-error graph-id "Edge ~S is not an ir-edge." edge))
  (unless (%non-empty-string-p (ir-edge-from-id edge))
    (%validation-error graph-id "Edge from-id must be a non-empty string, got ~S."
                       (ir-edge-from-id edge)))
  (unless (%non-empty-string-p (ir-edge-to-id edge))
    (%validation-error graph-id "Edge to-id must be a non-empty string, got ~S."
                       (ir-edge-to-id edge)))
  (unless (keywordp (ir-edge-kind edge))
    (%validation-error graph-id "Edge kind must be a keyword, got ~S."
                       (ir-edge-kind edge)))
  (unless (gethash (ir-edge-from-id edge) node-id-table)
    (%validation-error graph-id "Edge references unknown from-id ~S." (ir-edge-from-id edge)))
  (unless (gethash (ir-edge-to-id edge) node-id-table)
    (%validation-error graph-id "Edge references unknown to-id ~S." (ir-edge-to-id edge))))

(defun validate-achatina-ir-graph (graph &key expected-stage)
  "Validate GRAPH for the staged Achatina pipeline.
Signals `ir-validation-error` on structural or metadata failures and returns
GRAPH when validation succeeds."
  (unless (ir-graph-p graph)
    (%validation-error graph "Expected an ir-graph."))
  (unless (%non-empty-string-p (ir-graph-id graph))
    (%validation-error graph "Graph id must be a non-empty string, got ~S." (ir-graph-id graph)))
  (unless (keywordp (ir-graph-node-type graph))
    (%validation-error (ir-graph-id graph)
                       "Graph node-type must be a keyword, got ~S."
                       (ir-graph-node-type graph)))
  (let* ((metadata (ir-graph-metadata graph))
         (identity (getf metadata :artifact-identity))
         (stage (getf metadata :pipeline-stage))
         (capabilities (getf metadata :capabilities))
         (policies (getf metadata :policies))
         (tools (getf metadata :tools))
         (memory-scopes (getf metadata :memory-scopes))
         (parent-hash (getf metadata :parent-ir-hash))
         (node-id-table (make-hash-table :test #'equal)))
    (unless (%non-empty-string-p identity)
      (%validation-error (ir-graph-id graph)
                         "Graph metadata must include non-empty :artifact-identity."))
    (unless (member stage +allowed-achatina-pipeline-stages+ :test #'eq)
      (%validation-error (ir-graph-id graph)
                         "Graph metadata :pipeline-stage ~S is not allowed."
                         stage))
    (when expected-stage
      (unless (eq stage expected-stage)
        (%validation-error (ir-graph-id graph)
                           "Expected pipeline stage ~S, got ~S."
                           expected-stage
                           stage)))
    (let ((expected-node-type (%expected-node-type-for-stage stage)))
      (when (and expected-node-type
                 (not (eq (ir-graph-node-type graph) expected-node-type)))
        (%validation-error (ir-graph-id graph)
                           "Graph node-type ~S does not match pipeline stage ~S."
                           (ir-graph-node-type graph)
                           stage)))
    (when capabilities
      (unless (%string-list-p capabilities)
        (%validation-error (ir-graph-id graph)
                           "Graph metadata :capabilities must be a list of strings, got ~S."
                           capabilities)))
    (when policies
      (unless (%keyword-list-p policies)
        (%validation-error (ir-graph-id graph)
                           "Graph metadata :policies must be a list of keywords, got ~S."
                           policies)))
    (when tools
      (unless (%string-list-p tools)
        (%validation-error (ir-graph-id graph)
                           "Graph metadata :tools must be a list of strings, got ~S."
                           tools)))
    (when memory-scopes
      (unless (%keyword-list-p memory-scopes)
        (%validation-error (ir-graph-id graph)
                           "Graph metadata :memory-scopes must be a list of keywords, got ~S."
                           memory-scopes)))
    (when parent-hash
      (unless (valid-versioned-hash-p parent-hash)
        (%validation-error (ir-graph-id graph)
                           "Graph metadata :parent-ir-hash must be a valid versioned hash, got ~S."
                           parent-hash)))
    (dolist (node (ir-graph-nodes graph))
      (%validate-node node (ir-graph-id graph))
      (when (gethash (ir-node-id node) node-id-table)
        (%validation-error (ir-graph-id graph)
                           "Duplicate node id ~S." (ir-node-id node)))
      (setf (gethash (ir-node-id node) node-id-table) t))
    (dolist (edge (ir-graph-edges graph))
      (%validate-edge edge (ir-graph-id graph) node-id-table))
    graph))

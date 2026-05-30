(in-package #:claw-lisp.ir.semantic)

(defun %string-token (value)
  "Normalize VALUE into a stable node-id token.
This downcases text and replaces characters outside `[A-Za-z0-9_-]` with `-`."
  (string-downcase
   (with-output-to-string (stream)
     (loop for character across (princ-to-string value)
           do (write-char (if (or (alphanumericp character)
                                  (char= character #\-)
                                  (char= character #\_))
                              character
                              #\-)
                          stream)))))

(defun %semantic-anchor-node-id (graph)
  (format nil "~A/semantic-anchor" (ir-graph-id graph)))

(defun %capability-node-id (graph capability)
  (format nil "~A/capability/~A"
          (%semantic-anchor-node-id graph)
          (%string-token capability)))

(defun %policy-node-id (graph policy)
  (format nil "~A/policy/~A"
          (%semantic-anchor-node-id graph)
          (%string-token (symbol-name policy))))

(defun %tool-node-id (graph tool-name)
  (format nil "~A/tool/~A"
          (%semantic-anchor-node-id graph)
          (%string-token tool-name)))

(defun %memory-scope-node-id (graph memory-scope)
  (format nil "~A/memory-scope/~A"
          (%semantic-anchor-node-id graph)
          (%string-token (symbol-name memory-scope))))

(defun %existing-node-table (nodes)
  (let ((table (make-hash-table :test #'equal)))
    (dolist (node nodes table)
      (setf (gethash (ir-node-id node) table) node))))

(defun %edge-key (from-id to-id kind)
  (list from-id to-id kind))

(defun %existing-edge-table (edges)
  (let ((table (make-hash-table :test #'equal)))
    (dolist (edge edges table)
      (setf (gethash (%edge-key (claw-lisp.ir.schema:ir-edge-from-id edge)
                                (claw-lisp.ir.schema:ir-edge-to-id edge)
                                (claw-lisp.ir.schema:ir-edge-kind edge))
                     table)
            t))))

(defun %maybe-add-node (node nodes node-table)
  (unless (gethash (ir-node-id node) node-table)
    (setf (gethash (ir-node-id node) node-table) node)
    (push node nodes))
  nodes)

(defun %maybe-add-edge (edge edges edge-table)
  (let ((key (%edge-key (claw-lisp.ir.schema:ir-edge-from-id edge)
                        (claw-lisp.ir.schema:ir-edge-to-id edge)
                        (claw-lisp.ir.schema:ir-edge-kind edge))))
    (unless (gethash key edge-table)
      (setf (gethash key edge-table) t)
      (push edge edges)))
  edges)

(defun %sorted-capabilities (graph)
  (sort (copy-list (or (getf (ir-graph-metadata graph) :capabilities) '()))
        #'string<))

(defun %sorted-policies (graph)
  (sort (copy-list (or (getf (ir-graph-metadata graph) :policies) '()))
        #'string<
        :key #'symbol-name))

(defun %sorted-tools (graph)
  (sort (copy-list (or (getf (ir-graph-metadata graph) :tools) '()))
        #'string<))

(defun %sorted-memory-scopes (graph)
  (sort (copy-list (or (getf (ir-graph-metadata graph) :memory-scopes) '()))
        #'string<
        :key #'symbol-name))

(defun %semantic-anchor-node (graph)
  (make-ir-node
   :id (%semantic-anchor-node-id graph)
   :kind :semantic-anchor
   :payload (list :artifact-identity (getf (ir-graph-metadata graph) :artifact-identity)
                  :pipeline-stage (getf (ir-graph-metadata graph) :pipeline-stage))
   :metadata '(:semantic-expansion t)))

(defun %capability-node (graph capability)
  (make-ir-node
   :id (%capability-node-id graph capability)
   :kind :capability-declaration
   :payload (list :name capability)
   :metadata '(:semantic-expansion t)))

(defun %policy-node (graph policy)
  (make-ir-node
   :id (%policy-node-id graph policy)
   :kind :policy-declaration
   :payload (list :name policy)
   :metadata '(:semantic-expansion t)))

(defun %tool-node (graph tool-name)
  (make-ir-node
   :id (%tool-node-id graph tool-name)
   :kind :tool-declaration
   :payload (list :name tool-name)
   :metadata '(:semantic-expansion t)))

(defun %memory-scope-node (graph memory-scope)
  (make-ir-node
   :id (%memory-scope-node-id graph memory-scope)
   :kind :memory-scope-declaration
   :payload (list :name memory-scope)
   :metadata '(:semantic-expansion t)))

(defun %anchor-edge (graph node-id kind)
  (make-ir-edge
   :from-id (%semantic-anchor-node-id graph)
   :to-id node-id
   :kind kind
   :metadata '(:semantic-expansion t)))

(defun %expand-governance-declarations (graph)
  "Materialize governance declarations from graph metadata into explicit IR.
This pass treats `ir-graph-metadata` as the source of truth for
`:capabilities`, `:policies`, `:tools`, and `:memory-scopes`; pass context is
intentionally ignored here."
  (let* ((capabilities (%sorted-capabilities graph))
         (policies (%sorted-policies graph))
         (tools (%sorted-tools graph))
         (memory-scopes (%sorted-memory-scopes graph))
         (nodes (copy-list (ir-graph-nodes graph)))
         (edges (copy-list (ir-graph-edges graph)))
         (node-table (%existing-node-table nodes))
         (edge-table (%existing-edge-table edges))
         (anchor-present-p (gethash (%semantic-anchor-node-id graph) node-table)))
    (unless (or capabilities policies tools memory-scopes anchor-present-p)
      (return-from %expand-governance-declarations
        (make-semantic-ir
         :id (ir-graph-id graph)
         :ir-version (ir-graph-ir-version graph)
         :nodes nodes
         :edges edges
         :metadata (copy-list (ir-graph-metadata graph)))))
    (when (or capabilities policies tools memory-scopes
              anchor-present-p)
      (setf nodes (%maybe-add-node (%semantic-anchor-node graph) nodes node-table))
      (dolist (capability capabilities)
        (let* ((node (%capability-node graph capability))
               (node-id (ir-node-id node)))
          (setf nodes (%maybe-add-node node nodes node-table))
          (setf edges (%maybe-add-edge (%anchor-edge graph node-id :declares-capability)
                                       edges edge-table))))
      (dolist (policy policies)
        (let* ((node (%policy-node graph policy))
               (node-id (ir-node-id node)))
          (setf nodes (%maybe-add-node node nodes node-table))
          (setf edges (%maybe-add-edge (%anchor-edge graph node-id :declares-policy)
                                       edges edge-table))))
      (dolist (tool-name tools)
        (let* ((node (%tool-node graph tool-name))
               (node-id (ir-node-id node)))
          (setf nodes (%maybe-add-node node nodes node-table))
          (setf edges (%maybe-add-edge (%anchor-edge graph node-id :declares-tool)
                                       edges edge-table))))
      (dolist (memory-scope memory-scopes)
        (let* ((node (%memory-scope-node graph memory-scope))
               (node-id (ir-node-id node)))
          (setf nodes (%maybe-add-node node nodes node-table))
          (setf edges (%maybe-add-edge (%anchor-edge graph node-id :declares-memory-scope)
                                       edges edge-table)))))
    (make-semantic-ir
     :id (ir-graph-id graph)
     :ir-version (ir-graph-ir-version graph)
     :nodes (nreverse nodes)
     :edges (nreverse edges)
     :metadata (copy-list (ir-graph-metadata graph)))))

(defun make-default-semantic-expansion-steps (&key ref-prefix manifest-ref-prefix metadata)
  "Return the first semantic expansion pipeline for Achatina IR."
  (labels ((step-ref (suffix)
             (and ref-prefix
                  (format nil "~A/~A/current" ref-prefix suffix)))
           (manifest-ref (suffix)
             (and manifest-ref-prefix
                  (format nil "~A/~A/current" manifest-ref-prefix suffix))))
    (list
     (make-ir-pipeline-step
      :pass-name :materialize-governance-declarations
      :output-stage :semantic-ir
      :ref-name (step-ref "governance")
      :manifest-ref-name (manifest-ref "governance")
      :metadata metadata
      :transform-fn
      (lambda (current-graph current-context)
        (declare (ignore current-context))
        (%expand-governance-declarations current-graph))))))

(defun expand-semantic-ir-pipeline (graph context &key runtime initial-parent-artifact
                                                  ref-prefix manifest-ref-prefix metadata)
  "Run the default semantic expansion pipeline for semantic Achatina IR."
  (run-achatina-ir-pipeline graph
                            context
                            (make-default-semantic-expansion-steps
                             :ref-prefix ref-prefix
                             :manifest-ref-prefix manifest-ref-prefix
                             :metadata metadata)
                            :runtime runtime
                            :initial-parent-artifact initial-parent-artifact))

(defun expand-semantic-ir (graph context)
  "Run the default semantic expansion pipeline and return the final pass result."
  (ir-pipeline-run-final-result
   (expand-semantic-ir-pipeline graph context)))

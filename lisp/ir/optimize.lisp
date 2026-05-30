(in-package #:claw-lisp.ir.optimize)

(defun %stringify-keyword (value)
  (if (keywordp value)
      (symbol-name value)
      (princ-to-string value)))

(defun %canonical-fragment (value)
  (with-standard-io-syntax
    (let ((*print-circle* nil)
          (*print-readably* t)
          (*print-pretty* nil)
          (*print-case* :upcase))
      (write-to-string (claw-lisp.ir.schema:ir-object->canonical-sexp value)))))

(defun %normalize-edge-key (edge)
  (format nil "~A|~A|~A|~A"
          (ir-edge-from-id edge)
          (ir-edge-to-id edge)
          (%stringify-keyword (ir-edge-kind edge))
          (%canonical-fragment (ir-edge-metadata edge))))

(defun %normalize-node-key (node)
  (format nil "~A|~A|~A|~A"
          (ir-node-id node)
          (%stringify-keyword (ir-node-kind node))
          (%canonical-fragment (ir-node-metadata node))
          (%canonical-fragment (ir-node-payload node))))

(defun %dedupe-edges (edges)
  (let ((seen (make-hash-table :test #'equal))
        (result '()))
    (dolist (edge edges)
      (let ((key (%normalize-edge-key edge)))
        (unless (gethash key seen)
          (setf (gethash key seen) t)
          (push edge result))))
    (nreverse result)))

(defun %sorted-nodes (nodes)
  (sort (copy-list nodes)
        #'string<
        :key (lambda (node)
               (%normalize-node-key node))))

(defun %sorted-edges (edges)
  (sort (copy-list edges)
        #'string<
        :key (lambda (edge)
               (%normalize-edge-key edge))))

(defun make-default-optimization-pipeline-steps (&key ref-prefix manifest-ref-prefix metadata)
  "Return the first reusable optimization pipeline for validated Achatina IR."
  (labels ((step-ref (suffix)
             (and ref-prefix
                  (format nil "~A/~A/current" ref-prefix suffix)))
           (manifest-ref (suffix)
             (and manifest-ref-prefix
                  (format nil "~A/~A/current" manifest-ref-prefix suffix))))
    (list
     (make-ir-pipeline-step
      :pass-name :normalize-graph-layout
      :output-stage :optimized-ir
      :ref-name (step-ref "layout")
      :manifest-ref-name (manifest-ref "layout")
      :metadata metadata
      :transform-fn
      (lambda (current-graph current-context)
        (declare (ignore current-context))
        (make-optimized-ir
         :id (ir-graph-id current-graph)
         :ir-version (ir-graph-ir-version current-graph)
         :nodes (%sorted-nodes (ir-graph-nodes current-graph))
         :edges (%sorted-edges (ir-graph-edges current-graph))
         :metadata (ir-graph-metadata current-graph))))
     (make-ir-pipeline-step
      :pass-name :dedupe-graph-edges
      :output-stage :optimized-ir
      :ref-name (step-ref "deduped")
      :manifest-ref-name (manifest-ref "deduped")
      :metadata metadata
      :transform-fn
      (lambda (current-graph current-context)
        (declare (ignore current-context))
        (make-optimized-ir
         :id (ir-graph-id current-graph)
         :ir-version (ir-graph-ir-version current-graph)
         :nodes (copy-list (ir-graph-nodes current-graph))
         :edges (%sorted-edges (%dedupe-edges (ir-graph-edges current-graph)))
         :metadata (ir-graph-metadata current-graph)))))))

(defun optimize-validated-ir-pipeline (graph context &key runtime initial-parent-artifact
                                                     ref-prefix manifest-ref-prefix metadata)
  "Run the default optimization pipeline for validated Achatina IR."
  (run-achatina-ir-pipeline graph
                            context
                            (make-default-optimization-pipeline-steps
                             :ref-prefix ref-prefix
                             :manifest-ref-prefix manifest-ref-prefix
                             :metadata metadata)
                            :runtime runtime
                            :initial-parent-artifact initial-parent-artifact))

(defun optimize-validated-ir (graph context)
  "Run the default deterministic Achatina optimization pipeline and return the final pass result."
  (ir-pipeline-run-final-result
   (optimize-validated-ir-pipeline graph context)))

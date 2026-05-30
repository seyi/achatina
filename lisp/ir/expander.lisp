(in-package #:claw-lisp.ir.expander)

(defstruct (ir-pass-context
            (:constructor make-ir-pass-context
                (&key
                 (identity "")
                 (capabilities nil)
                 (policies nil)
                 (tools nil)
                 (memory-scopes nil)
                 (parent-ir-hash nil)
                 (parent-ir-ref-name nil)
                 (metadata nil))))
  (identity "" :type string)
  (capabilities nil :type list)
  (policies nil :type list)
  (tools nil :type list)
  (memory-scopes nil :type list)
  parent-ir-hash
  parent-ir-ref-name
  metadata)

(defstruct (ir-pass-result
            (:constructor make-ir-pass-result
                (&key pass-name input-stage output-stage graph metadata)))
  pass-name
  input-stage
  output-stage
  graph
  metadata)

(defstruct (ir-pipeline-step
            (:constructor make-ir-pipeline-step
                (&key pass-name output-stage transform-fn ref-name manifest-ref-name metadata)))
  pass-name
  output-stage
  transform-fn
  ref-name
  manifest-ref-name
  metadata)

(defstruct (ir-pipeline-run
            (:constructor make-ir-pipeline-run
                (&key results persisted-results final-result final-graph-artifact final-manifest-artifact)))
  results
  persisted-results
  final-result
  final-graph-artifact
  final-manifest-artifact)

(defun %sorted-copy (items predicate key)
  (sort (copy-list items) predicate :key key))

(defun %normalize-capability-strings (capabilities)
  (when capabilities
    (%sorted-copy capabilities #'string< #'identity)))

(defun %normalize-policy-keywords (policies)
  (when policies
    (%sorted-copy policies #'string< (lambda (value) (symbol-name value)))))

(defun %normalize-tool-strings (tools)
  (when tools
    (%sorted-copy tools #'string< #'identity)))

(defun %normalize-memory-scope-keywords (memory-scopes)
  (when memory-scopes
    (%sorted-copy memory-scopes #'string< (lambda (value) (symbol-name value)))))

(defun %merge-plists (&rest plists)
  "Merge PLISTS with explicit last-write-wins precedence.
Later plists override earlier ones for duplicate keys. This is used in
Achatina provenance assembly where reserved keys from pipeline context and
pass bookkeeping intentionally override transform-supplied metadata."
  (let ((result '()))
    (dolist (plist plists)
      (loop for (key value) on plist by #'cddr
            do (setf (getf result key) value)))
    result))

(defun %pass-name-token (pass-name)
  (if (keywordp pass-name)
      (string-downcase (symbol-name pass-name))
      (princ-to-string pass-name)))

(defun %normalize-pass-lineage (metadata pass-name)
  (let* ((existing-lineage (copy-list (or (getf metadata :pass-lineage) '())))
         (current-pass (getf metadata :pass-name)))
    (when (and current-pass (null existing-lineage))
      (setf existing-lineage (list (%pass-name-token current-pass))))
    (append existing-lineage (list (%pass-name-token pass-name)))))

(defun %pass-metadata (input-graph graph context pass-name output-stage)
  "Assemble canonical pass metadata with explicit reserved-key precedence.
Transform metadata is merged first, then context metadata, then reserved keys
owned by the pipeline runner. Reserved keys include:
`:artifact-identity`, `:source-stage`, `:pipeline-stage`, `:pass-name`,
`:pass-lineage`, `:source-pass-name`, `:parent-ir-hash`,
`:parent-ir-ref-name`, `:capabilities`, `:policies`, `:tools`, and
`:memory-scopes`."
  (let* ((input-metadata (ir-graph-metadata input-graph))
         (source-stage (getf input-metadata :pipeline-stage))
         (source-pass-name (getf input-metadata :pass-name))
         (pass-lineage (%normalize-pass-lineage input-metadata pass-name)))
    (canonicalize-plist
     (%merge-plists
      (ir-graph-metadata graph)
      (ir-pass-context-metadata context)
      (list :artifact-identity (ir-pass-context-identity context)
            :source-stage source-stage
            :pipeline-stage output-stage
            :pass-name pass-name
            :pass-lineage pass-lineage)
      (when source-pass-name
        (list :source-pass-name source-pass-name))
      (when (ir-pass-context-parent-ir-hash context)
        (list :parent-ir-hash (ir-pass-context-parent-ir-hash context)))
      (when (ir-pass-context-parent-ir-ref-name context)
        (list :parent-ir-ref-name (ir-pass-context-parent-ir-ref-name context)))
      (when (ir-pass-context-capabilities context)
        (list :capabilities (%normalize-capability-strings
                             (ir-pass-context-capabilities context))))
      (when (ir-pass-context-policies context)
        (list :policies (%normalize-policy-keywords
                         (ir-pass-context-policies context))))
      (when (ir-pass-context-tools context)
        (list :tools (%normalize-tool-strings
                      (ir-pass-context-tools context))))
      (when (ir-pass-context-memory-scopes context)
        (list :memory-scopes (%normalize-memory-scope-keywords
                              (ir-pass-context-memory-scopes context))))))))

(defun %copy-graph-for-stage (graph node-type metadata)
  (make-ir-graph :id (ir-graph-id graph)
                 :ir-version (ir-graph-ir-version graph)
                 :node-type node-type
                 :nodes (copy-list (ir-graph-nodes graph))
                 :edges (copy-list (ir-graph-edges graph))
                 :metadata metadata))

(defun run-achatina-ir-pass (graph context pass-name output-stage transform-fn)
  "Run a deterministic IR-to-IR pass and return an `ir-pass-result`.
TRANSFORM-FN may reshape the graph, but stage/identity provenance is finalized
from CONTEXT and OUTPUT-STAGE after the transform returns."
  (validate-achatina-ir-graph graph)
  (let* ((input-stage (getf (ir-graph-metadata graph) :pipeline-stage))
         (transformed (funcall transform-fn graph context))
         (node-type (case output-stage
                      (:validated-ir :validated-ir)
                      (:optimized-ir :optimized-ir)
                      (t (ir-graph-node-type transformed))))
         (output-graph (%copy-graph-for-stage transformed
                                              node-type
                                              (%pass-metadata graph transformed context pass-name output-stage))))
    (validate-achatina-ir-graph output-graph :expected-stage output-stage)
    (make-ir-pass-result
     :pass-name pass-name
     :input-stage input-stage
     :output-stage output-stage
     :graph output-graph
     :metadata (list :artifact-identity (ir-pass-context-identity context)
                     :parent-ir-hash (ir-pass-context-parent-ir-hash context)
                     :parent-ir-ref-name (ir-pass-context-parent-ir-ref-name context)
                     :source-stage input-stage
                     :capabilities (%normalize-capability-strings
                                    (ir-pass-context-capabilities context))
                     :policies (%normalize-policy-keywords
                                (ir-pass-context-policies context))
                     :tools (%normalize-tool-strings
                             (ir-pass-context-tools context))
                     :memory-scopes (%normalize-memory-scope-keywords
                                     (ir-pass-context-memory-scopes context))))))

(defun prepare-validated-ir (graph context)
  "Prepare GRAPH as a validated Achatina IR stage using CONTEXT metadata."
  (run-achatina-ir-pass graph context :validate-graph :validated-ir
                        (lambda (current-graph current-context)
                          (declare (ignore current-context))
                          (make-validated-ir
                           :id (ir-graph-id current-graph)
                           :ir-version (ir-graph-ir-version current-graph)
                           :nodes (copy-list (ir-graph-nodes current-graph))
                           :edges (copy-list (ir-graph-edges current-graph))
                           :metadata (ir-graph-metadata current-graph)))))

(defun persist-ir-pass-result (runtime pass-result &key parent-artifact ref-name manifest-ref-name metadata)
  "Persist PASS-RESULT through the IR CAS bridge and link it with an optional parent artifact."
  (multiple-value-bind (graph-artifact graph-descriptor)
      (materialize-ir-to-cas runtime
                             (ir-pass-result-graph pass-result)
                             :ref-name ref-name
                             :metadata (canonicalize-plist
                                        (%merge-plists
                                         metadata
                                         (list :pass-name (ir-pass-result-pass-name pass-result)
                                               :pipeline-stage (ir-pass-result-output-stage pass-result)))))
    (let* ((cas-root (runtime-effective-cas-root runtime))
           (ref-root (runtime-effective-cas-ref-root runtime))
           (manifest
            (make-manifest
             :entries
             (remove nil
                     (list
                       (and parent-artifact
                            (artifact-cas-hash parent-artifact)
                            (make-manifest-entry
                             :role (artifact-kind parent-artifact)
                             :cas-hash (artifact-cas-hash parent-artifact)
                             :type (artifact-cas-type parent-artifact)
                             :metadata nil))
                       (make-manifest-entry
                        :role (artifact-kind graph-artifact)
                        :cas-hash (artifact-cas-hash graph-artifact)
                        :type :sexp
                        :metadata nil)))
              :metadata nil))
           (manifest-hash (store-manifest cas-root manifest))
           (manifest-text (serialize-manifest manifest))
           (manifest-metadata
             (canonicalize-plist
              (%merge-plists
               metadata
               (list :kind :ir-pass-manifest
                     :pass-name (ir-pass-result-pass-name pass-result)
                     :pipeline-stage (ir-pass-result-output-stage pass-result)))))
           (manifest-artifact
             (make-artifact
              :id (ir-graph-id (ir-pass-result-graph pass-result))
              :kind :ir-pass-manifest
              :cas-hash manifest-hash
              :cas-type :json
              :cas-ref-name manifest-ref-name
              :metadata manifest-metadata))
           (manifest-descriptor
             (list :kind :ir-pass-manifest
                   :cas-hash manifest-hash
                   :cas-type :json
                   :cas-ref-name manifest-ref-name
                   :bytes (length manifest-text)
                   :metadata manifest-metadata)))
      (when (and ref-root manifest-ref-name)
        (write-cas-ref ref-root manifest-ref-name manifest-hash
                       :record-history-p t
                       :metadata manifest-metadata))
      (list :graph graph-artifact
            :graph-descriptor graph-descriptor
            :manifest manifest-artifact
            :manifest-descriptor manifest-descriptor))))

(defun %copy-context-with-parent (context parent-artifact parent-ref-name)
  (make-ir-pass-context
   :identity (ir-pass-context-identity context)
   :capabilities (copy-list (ir-pass-context-capabilities context))
   :policies (copy-list (ir-pass-context-policies context))
   :tools (copy-list (ir-pass-context-tools context))
   :memory-scopes (copy-list (ir-pass-context-memory-scopes context))
   :parent-ir-hash (and parent-artifact (artifact-cas-hash parent-artifact))
   :parent-ir-ref-name parent-ref-name
   :metadata (copy-list (ir-pass-context-metadata context))))

(defun %valid-pipeline-step-output-stage-p (stage)
  (member stage claw-lisp.ir.validate:+allowed-achatina-pipeline-stages+ :test #'eq))

(defun %valid-pipeline-step-p (step)
  (and (ir-pipeline-step-p step)
       (symbolp (ir-pipeline-step-pass-name step))
       (%valid-pipeline-step-output-stage-p (ir-pipeline-step-output-stage step))
       (functionp (ir-pipeline-step-transform-fn step))))

(defun run-achatina-ir-pipeline (graph context steps &key runtime initial-parent-artifact)
  "Run STEPS against GRAPH using CONTEXT and optionally persist each step.
When RUNTIME is provided, each pass result is persisted and the next step is
chained to the previous output artifact for explicit CAS-backed provenance.
If CONTEXT already carries `parent-ir-hash`, that wins over
INITIAL-PARENT-ARTIFACT for the first pass; otherwise the initial parent
artifact seeds the first pass context."
  (let ((current-graph graph)
        (current-context
          (if (and initial-parent-artifact
                   (null (ir-pass-context-parent-ir-hash context)))
              (%copy-context-with-parent context
                                         initial-parent-artifact
                                         (or (artifact-cas-ref-name initial-parent-artifact)
                                             (ir-pass-context-parent-ir-ref-name context)))
              context))
        (parent-artifact initial-parent-artifact)
        (results '())
        (persisted-results '())
        (final-artifact nil)
        (final-manifest nil))
    (dolist (step steps)
      (unless (%valid-pipeline-step-p step)
        (error "Invalid pipeline step: ~S" step))
      (let* ((result (run-achatina-ir-pass current-graph
                                           current-context
                                           (ir-pipeline-step-pass-name step)
                                           (ir-pipeline-step-output-stage step)
                                           (ir-pipeline-step-transform-fn step)))
             (persisted (and runtime
                             (persist-ir-pass-result runtime
                                                     result
                                                     :parent-artifact parent-artifact
                                                     :ref-name (ir-pipeline-step-ref-name step)
                                                     :manifest-ref-name (ir-pipeline-step-manifest-ref-name step)
                                                     :metadata (ir-pipeline-step-metadata step)))))
        (push result results)
        (when persisted
          (push persisted persisted-results)
          (setf final-artifact (getf persisted :graph)
                final-manifest (getf persisted :manifest)
                parent-artifact final-artifact
                current-context (%copy-context-with-parent current-context
                                                          final-artifact
                                                          (ir-pipeline-step-ref-name step))))
        (setf current-graph (ir-pass-result-graph result))))
    (let ((ordered-results (nreverse results))
          (ordered-persisted-results (nreverse persisted-results)))
      (make-ir-pipeline-run
       :results ordered-results
       :persisted-results ordered-persisted-results
       :final-result (car (last ordered-results))
       :final-graph-artifact final-artifact
       :final-manifest-artifact final-manifest))))

(in-package #:claw-lisp.ir.schema)

(defparameter +default-ir-version+ "2026.1"
  "Current supported version for canonical IR payloads.")

(defstruct (ir-node
            (:constructor make-ir-node
                (&key
                 (id "")
                 (kind :unknown)
                 (payload nil)
                 (metadata nil))))
  (id "" :type string)
  (kind :unknown :type keyword)
  payload
  metadata)

(defstruct (ir-edge
            (:constructor make-ir-edge
                (&key
                 (from-id "")
                 (to-id "")
                 (kind :flow)
                 (metadata nil))))
  (from-id "" :type string)
  (to-id "" :type string)
  (kind :flow :type keyword)
  metadata)

(defstruct (ir-graph
            (:constructor make-ir-graph
                (&key
                 (id "")
                 (ir-version +default-ir-version+)
                 (node-type :generic-ir)
                 (nodes nil)
                 (edges nil)
                 (metadata nil))))
  (id "" :type string)
  (ir-version +default-ir-version+ :type string)
  (node-type :generic-ir :type keyword)
  (nodes nil :type list)
  (edges nil :type list)
  metadata)

(defun make-semantic-ir (&key (id "") (ir-version +default-ir-version+) (nodes nil) (edges nil) (metadata nil))
  (make-ir-graph :id id
                 :ir-version ir-version
                 :node-type :semantic-ir
                 :nodes nodes
                 :edges edges
                 :metadata metadata))

(defun make-optimized-ir (&key (id "") (ir-version +default-ir-version+) (nodes nil) (edges nil) (metadata nil))
  (make-ir-graph :id id
                 :ir-version ir-version
                 :node-type :optimized-ir
                 :nodes nodes
                 :edges edges
                 :metadata metadata))

(defun make-validated-ir (&key (id "") (ir-version +default-ir-version+) (nodes nil) (edges nil) (metadata nil))
  (make-ir-graph :id id
                 :ir-version ir-version
                 :node-type :validated-ir
                 :nodes nodes
                 :edges edges
                 :metadata metadata))

(defun make-execution-plan-ir (&key (id "") (ir-version +default-ir-version+) (nodes nil) (edges nil) (metadata nil))
  (make-ir-graph :id id
                 :ir-version ir-version
                 :node-type :execution-plan
                 :nodes nodes
                 :edges edges
                 :metadata metadata))

(defun %plist-like-p (value)
  (and (listp value)
       (evenp (length value))
       (loop for key in value by #'cddr
             always (keywordp key))))

(defun %unsupported-serialization (object reason)
  (error 'ir-serialization-error
         :object object
         :reason reason))

(defun %unsupported-deserialization (payload reason)
  (error 'ir-deserialization-error
         :payload payload
         :reason reason))

(defun %ensure-supported-version (actual object-type)
  (unless (string= actual +default-ir-version+)
    (error 'ir-version-mismatch-error
           :expected +default-ir-version+
           :actual actual
           :object-type object-type)))

(defun %canonical-plist (plist)
  (let ((pairs '()))
    (loop for (key value) on plist by #'cddr
          do (unless (keywordp key)
               (%unsupported-serialization plist
                                           (format nil "plist key ~S is not a keyword" key)))
             (push (cons key (%canonical-value value)) pairs))
    (loop for (key . value)
            in (sort pairs
                     #'string<
                     :key (lambda (pair)
                            (format nil "~A::~A"
                                    (or (and (symbol-package (car pair))
                                             (package-name (symbol-package (car pair))))
                                        "")
                                    (symbol-name (car pair)))))
          append (list key value))))

(defun %artifact->canonical-sexp (artifact)
  (list :artifact
        :cas-hash (artifact-cas-hash artifact)
        :cas-ref-name (artifact-cas-ref-name artifact)
        :cas-type (artifact-cas-type artifact)
        :id (artifact-id artifact)
        :kind (artifact-kind artifact)
        :metadata (%canonical-value (artifact-metadata artifact))))

(defun %ir-node->canonical-sexp (node)
  (list :ir-node
        :id (ir-node-id node)
        :kind (ir-node-kind node)
        :metadata (%canonical-value (ir-node-metadata node))
        :payload (%canonical-value (ir-node-payload node))))

(defun %ir-edge->canonical-sexp (edge)
  (list :ir-edge
        :from-id (ir-edge-from-id edge)
        :kind (ir-edge-kind edge)
        :metadata (%canonical-value (ir-edge-metadata edge))
        :to-id (ir-edge-to-id edge)))

(defun %ir-graph->canonical-sexp (graph)
  (list :ir-graph
        :edges (mapcar #'%canonical-value (ir-graph-edges graph))
        :id (ir-graph-id graph)
        :ir-version (ir-graph-ir-version graph)
        :metadata (%canonical-value (ir-graph-metadata graph))
        :node-type (ir-graph-node-type graph)
        :nodes (mapcar #'%canonical-value (ir-graph-nodes graph))))

(defun %canonical-value (value)
  (cond
    ((null value) nil)
    ((or (stringp value)
         (numberp value)
         (characterp value)
         (eq value t)
         (keywordp value))
     value)
    ((typep value 'artifact)
     (%artifact->canonical-sexp value))
    ((ir-node-p value)
     (%ir-node->canonical-sexp value))
    ((ir-edge-p value)
     (%ir-edge->canonical-sexp value))
    ((ir-graph-p value)
     (%ir-graph->canonical-sexp value))
    ((vectorp value)
     ;; Canonical IR normalizes vectors to lists. Round-trips do not preserve
     ;; vector identity unless a future tagged representation is introduced.
     (map 'list #'%canonical-value value))
    ((%plist-like-p value)
     (%canonical-plist value))
    ((listp value)
     (mapcar #'%canonical-value value))
    ((and (symbolp value)
          (not (keywordp value)))
     (%unsupported-serialization value
                                 "non-keyword symbols are not permitted in canonical IR payloads"))
    (t
     (%unsupported-serialization value
                                 (format nil "unsupported value type ~S" (type-of value))))))

(defun ir-object->canonical-sexp (object)
  "Return OBJECT as a deterministic tagged s-expression."
  (%canonical-value object))

(defun ir-object->canonical-string (object)
  "Return a deterministic printed representation of OBJECT."
  (let ((*print-circle* nil)
        (*print-readably* t)
        (*print-pretty* nil)
        (*print-case* :upcase))
    (with-output-to-string (stream)
      (write (ir-object->canonical-sexp object) :stream stream))))

(defun %plist-value (plist key)
  (getf plist key))

(defun %recognized-tag-p (tag)
  (member tag '(:artifact :ir-node :ir-edge :ir-graph) :test #'eq))

(defun %sexp->artifact (sexp)
  (let ((plist (rest sexp)))
    (make-artifact
     :id (%plist-value plist :id)
     :kind (%plist-value plist :kind)
     :cas-hash (%plist-value plist :cas-hash)
     :cas-type (%plist-value plist :cas-type)
     :cas-ref-name (%plist-value plist :cas-ref-name)
     :metadata (%sexp->value (%plist-value plist :metadata)))))

(defun %sexp->ir-node (sexp)
  (let ((plist (rest sexp)))
    (make-ir-node
     :id (%plist-value plist :id)
     :kind (%plist-value plist :kind)
     :payload (%sexp->value (%plist-value plist :payload))
     :metadata (%sexp->value (%plist-value plist :metadata)))))

(defun %sexp->ir-edge (sexp)
  (let ((plist (rest sexp)))
    (make-ir-edge
     :from-id (%plist-value plist :from-id)
     :to-id (%plist-value plist :to-id)
     :kind (%plist-value plist :kind)
     :metadata (%sexp->value (%plist-value plist :metadata)))))

(defun %sexp->ir-graph (sexp)
  (let* ((plist (rest sexp))
         (ir-version (%plist-value plist :ir-version))
         (node-type (%plist-value plist :node-type)))
    (%ensure-supported-version ir-version node-type)
    (make-ir-graph
     :id (%plist-value plist :id)
     :ir-version ir-version
     :node-type node-type
     :nodes (mapcar #'%sexp->value (%plist-value plist :nodes))
     :edges (mapcar #'%sexp->value (%plist-value plist :edges))
     :metadata (%sexp->value (%plist-value plist :metadata)))))

(defun %sexp->plist (sexp)
  (let ((result '()))
    (loop for (key value) on sexp by #'cddr
          do (unless (keywordp key)
               (%unsupported-deserialization sexp
                                             (format nil "plist key ~S is not a keyword" key)))
             (push key result)
             (push (%sexp->value value) result))
    (nreverse result)))

(defun %sexp->value (value)
  (cond
    ((null value) nil)
    ((or (stringp value)
         (numberp value)
         (characterp value)
         (eq value t)
         (keywordp value))
     value)
    ((and (listp value)
          (consp value)
          (keywordp (first value))
          (%recognized-tag-p (first value)))
     (case (first value)
       (:artifact (%sexp->artifact value))
       (:ir-node (%sexp->ir-node value))
       (:ir-edge (%sexp->ir-edge value))
       (:ir-graph (%sexp->ir-graph value))
       (t (%unsupported-deserialization value
                                        (format nil "unknown IR tag ~S" (first value))))))
    ((%plist-like-p value)
     (%sexp->plist value))
    ((listp value)
     (mapcar #'%sexp->value value))
    (t
     (%unsupported-deserialization value
                                   (format nil "unsupported IR payload value of type ~S"
                                           (type-of value))))))

(defun canonical-sexp->ir-object (sexp)
  "Decode a canonical IR s-expression back into structs and plists."
  (%sexp->value sexp))

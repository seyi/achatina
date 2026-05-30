(in-package #:claw-lisp.ir.surface-form)

(defun %validation-error (subject format-string &rest args)
  (error 'ir-validation-error
         :subject subject
         :reason (apply #'format nil format-string args)))

(defun %required-string (form key)
  (let ((value (getf form key)))
    (unless (stringp value)
      (%validation-error form "~S must be a string, got ~S." key value))
    value))

(defun %optional-string-list (form key)
  (let ((value (getf form key)))
    (cond
      ((null value) nil)
      ((and (listp value) (every #'stringp value))
       (copy-list value))
      (t
       (%validation-error form "~S must be a list of strings, got ~S." key value)))))

(defun %optional-keyword-list (form key)
  (let ((value (getf form key)))
    (cond
      ((null value) nil)
      ((and (listp value) (every #'keywordp value))
       (copy-list value))
      (t
       (%validation-error form "~S must be a list of keywords, got ~S." key value)))))

(defun make-workflow-form-context (form)
  "Build the default pass context for a minimal authored workflow FORM."
  (make-ir-pass-context
   :identity (%required-string form :workflow-id)
   :capabilities (%optional-string-list form :capabilities)
   :policies (%optional-keyword-list form :policies)
   :tools (let ((tool (%required-string form :tool-name)))
            (list tool))
   :memory-scopes (%optional-keyword-list form :memory-scopes)))

(defun compile-minimal-workflow-form (form)
  "Compile a minimal authored workflow FORM into semantic Achatina IR.

This is intentionally a proof-slice compiler, not a general workflow frontend:
it supports exactly one input node, one tool-call node, and fixed node ids.

Supported form:

  (:workflow-id \"workflow/demo\"
   :input-text \"hello\"
   :tool-name \"echo\"
   [:capabilities (...)]
   [:policies (...)]
   [:memory-scopes (...)])"
  (let* ((workflow-id (%required-string form :workflow-id))
         (input-text (%required-string form :input-text))
         (tool-name (%required-string form :tool-name))
         (capabilities (%optional-string-list form :capabilities))
         (policies (%optional-keyword-list form :policies))
         (memory-scopes (%optional-keyword-list form :memory-scopes)))
    (make-semantic-ir
     :id workflow-id
     :nodes (list (make-ir-node
                   :id "node-a"
                   :kind :input
                   :payload (list :text input-text))
                  (make-ir-node
                   :id "node-b"
                   :kind :tool-call
                   :payload (list :tool tool-name)))
     :edges (list (make-ir-edge
                   :from-id "node-a"
                   :to-id "node-b"
                   :kind :flow)
                  (make-ir-edge
                   :from-id "node-a"
                   :to-id "node-b"
                   :kind :control))
     :metadata (append
                (list :artifact-identity workflow-id
                      :pipeline-stage :semantic-ir
                      :tools (list tool-name))
                (when capabilities
                  (list :capabilities capabilities))
                (when policies
                  (list :policies policies))
                (when memory-scopes
                  (list :memory-scopes memory-scopes))))))

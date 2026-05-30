(in-package #:claw-lisp.tools.echo)

(defclass echo-tool (tool) ())

(defun make-echo-tool ()
  "Create the baseline local echo tool used to verify runtime tool execution."
  (make-instance 'echo-tool
                 :name "echo"
                 :description "Return the provided text unchanged."))

(defmethod tool-input-schema ((tool echo-tool))
  (declare (ignore tool))
  (list :type "object"
        :properties (list :text (list :type "string"
                                      :description "The text to echo back."))
        :required #("text")))

(defmethod validate-tool-input ((tool echo-tool) input)
  (declare (ignore tool))
  (unless (and (listp input)
               (getf input :text)
               (stringp (getf input :text)))
    (error "Echo tool requires a plist input with a string :text field."))
  input)

(defmethod execute-tool ((tool echo-tool) input runtime)
  (declare (ignore tool runtime))
  (getf input :text))

(defmethod normalize-tool-result ((tool echo-tool) result)
  (declare (ignore tool))
  (make-tool-result
   :tool-name "echo"
   :content result
   :bytes (length result)))

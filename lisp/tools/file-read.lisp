(in-package #:claw-lisp.tools.file-read)

(defclass file-read-tool (tool) ())

(defun make-file-read-tool ()
  "Create the baseline local file-read tool for plain text file access."
  (make-instance 'file-read-tool
                 :name "file-read"
                 :description "Read a text file from a provided filesystem path."))

(defmethod tool-input-schema ((tool file-read-tool))
  (declare (ignore tool))
  (list :type "object"
        :properties (list :path (list :type "string"
                                      :description "The filesystem path to read."))
        :required #("path")))

(defmethod validate-tool-input ((tool file-read-tool) input)
  (declare (ignore tool))
  (unless (and (listp input)
               (getf input :path)
               (stringp (getf input :path)))
    (error "File-read tool requires a plist input with a string :path field."))
  (let ((pathname (pathname (getf input :path))))
    (unless (probe-file pathname)
      (error "File-read tool path does not exist: ~A" (getf input :path)))
    (when (uiop:directory-pathname-p pathname)
      (error "File-read tool path must be a file, not a directory: ~A" (getf input :path))))
  input)

(defmethod execute-tool ((tool file-read-tool) input runtime)
  (declare (ignore tool runtime))
  (uiop:read-file-string (getf input :path)))

(defmethod normalize-tool-result ((tool file-read-tool) result)
  (declare (ignore tool))
  (make-tool-result
   :tool-name "file-read"
   :content result
   :bytes (length result)))

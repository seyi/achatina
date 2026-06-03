(in-package #:claw-lisp.tools.file-write)

(defclass file-write-tool (tool) ())

(defun make-file-write-tool ()
  "Create the baseline local file-write tool for plain text file writes."
  (make-instance 'file-write-tool
                 :name "file-write"
                 :description "Write provided text content to a filesystem path."))

(defparameter +file-write-capability+
  '(:class :write :valid-phases (:edit) :mutates-fs t)
  "Loop-control capability for file-write. File-mutation action; counts as progress.")

(defmethod tool-capability ((tool file-write-tool))
  (declare (ignore tool))
  +file-write-capability+)

(register-tool-capability "file-write" +file-write-capability+)

(defmethod tool-input-schema ((tool file-write-tool))
  (declare (ignore tool))
  (list :type "object"
        :properties (list :path (list :type "string"
                                      :description "The filesystem path to write to.")
                          :text (list :type "string"
                                      :description "The text content to write."))
        :required #("path" "text")))

(defmethod validate-tool-input ((tool file-write-tool) input)
  (declare (ignore tool))
  (unless (and (listp input)
               (getf input :path)
               (stringp (getf input :path))
               (getf input :text)
               (stringp (getf input :text)))
    (error "File-write tool requires a plist input with string :path and :text fields."))
  (let ((pathname (pathname (getf input :path))))
    (when (uiop:directory-pathname-p pathname)
      (error "File-write tool path must be a file, not a directory: ~A" (getf input :path))))
  input)

(defmethod execute-tool ((tool file-write-tool) input runtime)
  (declare (ignore tool runtime))
  (let ((pathname (pathname (getf input :path)))
        (text (getf input :text)))
    (ensure-directories-exist pathname)
    (with-open-file (stream pathname
                            :direction :output
                            :if-exists :supersede
                            :if-does-not-exist :create)
      (write-string text stream))
    (list :path (namestring pathname)
          :bytes (length text))))

(defmethod normalize-tool-result ((tool file-write-tool) result)
  (declare (ignore tool))
  (make-tool-result
   :tool-name "file-write"
   :content (format nil "Wrote ~D bytes to ~A"
                    (getf result :bytes)
                    (getf result :path))
   :bytes (length (format nil "Wrote ~D bytes to ~A"
                          (getf result :bytes)
                          (getf result :path)))))

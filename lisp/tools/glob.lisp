(in-package #:claw-lisp.tools.glob)

(defclass glob-tool (tool) ())

(defun make-glob-tool ()
  "Create the baseline local glob tool for filesystem pattern matching."
  (make-instance 'glob-tool
                 :name "glob"
                 :description "Find files matching a glob pattern."))

(defparameter +glob-capability+
  '(:class :read :valid-phases (:inspect) :mutates-fs nil)
  "Loop-control capability for glob. Read-only discovery action.")

(defmethod tool-capability ((tool glob-tool))
  (declare (ignore tool))
  +glob-capability+)

(register-tool-capability "glob" +glob-capability+)

(defmethod tool-input-schema ((tool glob-tool))
  (declare (ignore tool))
  (list :type "object"
        :properties (list :path (list :type "string"
                                      :description "The directory to search in.")
                          :pattern (list :type "string"
                                         :description "The glob pattern to match."))
        :required #("path" "pattern")))

(defmethod validate-tool-input ((tool glob-tool) input)
  (declare (ignore tool))
  (unless (and (listp input)
               (getf input :path)
               (stringp (getf input :path))
               (getf input :pattern)
               (stringp (getf input :pattern)))
    (error "Glob tool requires :path and :pattern string fields."))
  (let ((pathname (pathname (getf input :path))))
    (unless (probe-file pathname)
      (error "Glob tool path does not exist: ~A" (getf input :path)))
    (unless (uiop:directory-pathname-p pathname)
      (error "Glob tool path must be a directory: ~A" (getf input :path))))
  input)

(defmethod execute-tool ((tool glob-tool) input runtime)
  (declare (ignore tool runtime))
  (let* ((directory (uiop:ensure-directory-pathname (getf input :path)))
         (pattern (getf input :pattern))
         (files (uiop:directory-files
                 directory
                 (uiop:merge-pathnames* (pathname pattern) directory))))
    (if files
        (format nil "~{~A~^~%~}" (mapcar #'namestring (sort files #'string<)))
        (format nil "[no files matching ~A in ~A]" pattern (getf input :path)))))

(defmethod normalize-tool-result ((tool glob-tool) result)
  (declare (ignore tool))
  (make-tool-result
   :tool-name "glob"
   :content result
   :bytes (length result)))

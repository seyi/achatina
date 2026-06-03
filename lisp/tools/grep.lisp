(in-package #:claw-lisp.tools.grep)

(defclass grep-tool (tool) ())

(defun make-grep-tool ()
  "Create the baseline local grep tool for substring search in files."
  (make-instance 'grep-tool
                 :name "grep"
                 :description "Search for a pattern in files within a directory."))

(defparameter +grep-capability+
  '(:class :read :valid-phases (:inspect) :mutates-fs nil)
  "Loop-control capability for grep. Read-only discovery action.")

(defmethod tool-capability ((tool grep-tool))
  (declare (ignore tool))
  +grep-capability+)

(register-tool-capability "grep" +grep-capability+)

(defmethod tool-input-schema ((tool grep-tool))
  (declare (ignore tool))
  (list :type "object"
        :properties (list :path (list :type "string"
                                      :description "The directory to search in.")
                          :pattern (list :type "string"
                                         :description "The pattern to search for."))
        :required #("path" "pattern")))

(defmethod validate-tool-input ((tool grep-tool) input)
  (declare (ignore tool))
  (unless (and (listp input)
               (getf input :path)
               (stringp (getf input :path))
               (getf input :pattern)
               (stringp (getf input :pattern)))
    (error "Grep tool requires :path and :pattern string fields."))
  (let ((pathname (pathname (getf input :path))))
    (unless (probe-file pathname)
      (error "Grep tool path does not exist: ~A" (getf input :path)))
    (unless (uiop:directory-pathname-p pathname)
      (error "Grep tool path must be a directory: ~A" (getf input :path))))
  input)

(defmethod execute-tool ((tool grep-tool) input runtime)
  (declare (ignore tool runtime))
  (let* ((directory (uiop:ensure-directory-pathname (getf input :path)))
         (pattern (getf input :pattern))
         (files (uiop:directory-files directory))
         (all-matches nil))
    (dolist (file files)
      (handler-case
          (let ((content (uiop:read-file-string file)))
            (let ((lines (uiop:split-string content :separator '(#\Newline)))
                  (line-num 0))
              (dolist (line lines)
                (incf line-num)
                (when (search pattern line)
                  (push (format nil "~A:~D:~A" (file-namestring file) line-num line)
                        all-matches)))))
        (error () nil)))
    (if all-matches
        (format nil "~{~A~^~%~}" (reverse all-matches))
        (format nil "[no matches for ~S in ~A]" pattern (getf input :path)))))

(defmethod normalize-tool-result ((tool grep-tool) result)
  (declare (ignore tool))
  (make-tool-result
   :tool-name "grep"
   :content result
   :bytes (length result)))

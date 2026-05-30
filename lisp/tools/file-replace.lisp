(in-package #:claw-lisp.tools.file-replace)

(defclass file-replace-tool (tool) ())

(defun make-file-replace-tool ()
  "Create the baseline local file-replace tool for exact text replacement."
  (make-instance 'file-replace-tool
                 :name "file-replace"
                 :description "Replace exact text occurrences in a file."))

(defmethod tool-input-schema ((tool file-replace-tool))
  (declare (ignore tool))
  (list :type "object"
        :properties (list :path (list :type "string"
                                      :description "The filesystem path to modify.")
                          :old-text (list :type "string"
                                          :description "The exact text to find and replace.")
                          :new-text (list :type "string"
                                          :description "The text to replace it with.")
                          :replace-all (list :type "boolean"
                                             :description "Replace all occurrences (default: false)."
                                             :default nil))
        :required #("path" "old-text" "new-text")))

(defmethod validate-tool-input ((tool file-replace-tool) input)
  (declare (ignore tool))
  (unless (and (listp input)
               (getf input :path)
               (stringp (getf input :path))
               (getf input :old-text)
               (stringp (getf input :old-text))
               (getf input :new-text)
               (stringp (getf input :new-text)))
    (error "File-replace tool requires :path, :old-text, and :new-text fields."))
  (let ((pathname (pathname (getf input :path))))
    (unless (probe-file pathname)
      (error "File-replace tool path does not exist: ~A" (getf input :path))))
  input)

(defmethod execute-tool ((tool file-replace-tool) input runtime)
  (declare (ignore tool runtime))
  (let* ((pathname (pathname (getf input :path)))
         (old-text (getf input :old-text))
         (new-text (getf input :new-text))
         (replace-all (getf input :replace-all))
         (content (uiop:read-file-string pathname))
         (pos (search old-text content))
         (count 0))
    (unless pos
      (error "Old text not found in file: ~A" (getf input :path)))
    (let ((new-content
            (if replace-all
                (replace-all-occurrences content old-text new-text)
                (let ((before (subseq content 0 pos))
                      (after (subseq content (+ pos (length old-text)))))
                  (concatenate 'string before new-text after)))))
      (with-open-file (stream pathname
                              :direction :output
                              :if-exists :supersede)
        (write-string new-content stream))
      (setf count (if replace-all
                      (count-substrings old-text content)
                      1))
      (list :path (namestring pathname)
            :replacements count
            :bytes-changed (- (length new-content) (length content))))))

(defun replace-all-occurrences (text old new)
  "Replace all occurrences of OLD with NEW in TEXT."
  (with-output-to-string (out)
    (let ((start 0))
      (loop
        (let ((pos (search old text :start2 start)))
          (if pos
              (progn
                (write-string (subseq text start pos) out)
                (write-string new out)
                (setf start (+ pos (length old))))
              (progn
                (write-string (subseq text start) out)
                (return))))))))

(defun count-substrings (sub seq)
  "Count occurrences of SUB in SEQ."
  (let ((count 0) (pos 0))
    (loop
      (setf pos (search sub seq :start2 pos))
      (unless pos (return count))
      (incf count)
      (incf pos (length sub)))))

(defmethod normalize-tool-result ((tool file-replace-tool) result)
  (declare (ignore tool))
  (make-tool-result
   :tool-name "file-replace"
   :content (format nil "Replaced ~D occurrence~:P in ~A (~@D bytes)"
                    (getf result :replacements)
                    (getf result :path)
                    (getf result :bytes-changed))
   :bytes (length (format nil "Replaced ~D occurrence~:P in ~A (~@D bytes)"
                          (getf result :replacements)
                          (getf result :path)
                          (getf result :bytes-changed)))))

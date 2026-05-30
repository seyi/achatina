(in-package #:claw-lisp.storage.tool-results)

(defun ensure-string-content (value)
  "Return VALUE as a string for persistence and preview generation."
  (if (stringp value)
      value
      (princ-to-string value)))

(defun tool-result-artifact-path (config session-id call-id)
  "Return the artifact pathname for SESSION-ID and CALL-ID."
  (merge-pathnames
   (make-pathname :directory `(:relative "tool-results" ,session-id)
                  :name call-id
                  :type "txt")
   (uiop:ensure-directory-pathname
    (runtime-config-artifacts-root config))))

(defun write-artifact (pathname content)
  "Write CONTENT to PATHNAME, creating parent directories if needed."
  (ensure-directories-exist pathname)
  (with-open-file (stream pathname
                          :direction :output
                          :if-exists :supersede
                          :if-does-not-exist :create)
    (write-string content stream)))

(defun session-tool-results-directory (config session-id)
  "Return the directory pathname containing persisted tool results for SESSION-ID."
  (merge-pathnames
   (make-pathname :directory `(:relative "tool-results" ,session-id))
   (uiop:ensure-directory-pathname
    (runtime-config-artifacts-root config))))

(defun preview-content (content limit)
  "Return the working-context preview for CONTENT under LIMIT."
  (subseq content 0 (min (length content) limit)))

(defun store-tool-result (config session result)
  "Persist oversized RESULT content and return the working-context form.

If the content exceeds the configured preview budget, the full payload is
written under the artifacts root and the returned result contains only a
preview plus persisted-path metadata."
  (let* ((full-content (ensure-string-content (tool-result-content result)))
         (bytes (max (tool-result-bytes result) (length full-content)))
         (limit (runtime-config-tool-preview-bytes config)))
    (if (> bytes limit)
        (let* ((path (tool-result-artifact-path config
                                                (agent-session-id session)
                                                (tool-result-call-id result)))
               (preview (preview-content full-content limit)))
          (write-artifact path full-content)
          (claw-lisp.core.domain::%copy-tool-result-with
           result
           :content preview
           :persisted-path (namestring path)
           :truncated-p t
           :bytes bytes))
        (claw-lisp.core.domain::%copy-tool-result-with
         result
         :content full-content
         :persisted-path (tool-result-persisted-path result)
         :truncated-p (tool-result-truncated-p result)
         :bytes bytes))))

(defun read-persisted-tool-result (pathname)
  "Read the full persisted tool result content from PATHNAME."
  (uiop:read-file-string pathname))

(defun delete-session-tool-results (config session-id)
  "Delete persisted tool-result artifacts for SESSION-ID when present."
  (let ((directory (session-tool-results-directory config session-id)))
    (when (probe-file directory)
      (uiop:delete-directory-tree directory :validate t))
    nil))

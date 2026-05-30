(in-package #:claw-lisp.storage.consolidation-lock)

(defun consolidation-lock-path (config)
  "Return the durable-memory consolidation lock path for CONFIG."
  (merge-pathnames
   ".consolidate-lock"
   (merge-pathnames
    (make-pathname :directory '(:relative "durable"))
    (uiop:ensure-directory-pathname
     (runtime-config-memory-root config)))))

(defmacro with-consolidation-lock ((config) &body body)
  "Run BODY while holding the baseline durable-memory consolidation lock."
  `(let ((path (consolidation-lock-path ,config)))
     (ensure-directories-exist path)
     (when (probe-file path)
       (error "Durable-memory consolidation lock already held: ~A" path))
     (unwind-protect
          (progn
            (with-open-file (stream path
                                    :direction :output
                                    :if-exists :supersede
                                    :if-does-not-exist :create)
              (format stream "locked ~A~%" (get-universal-time)))
            ,@body)
       (when (probe-file path)
         (delete-file path)))))

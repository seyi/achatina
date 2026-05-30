#!/usr/bin/env -S sbcl --script

(require :asdf)

(defun maybe-load-quicklisp ()
  (let ((candidates (list (uiop:getenv "QUICKLISP_SETUP")
                          "/root/quicklisp/setup.lisp"
                          (namestring (merge-pathnames "quicklisp/setup.lisp"
                                                       (user-homedir-pathname))))))
    (dolist (candidate candidates)
      (when (and candidate (probe-file candidate))
        (load candidate)
        (return t)))))

(maybe-load-quicklisp)

(defun resolve-script-path ()
  (or (and *load-pathname*
           (probe-file *load-pathname*))
      (and *compile-file-pathname*
           (probe-file *compile-file-pathname*))
      (let ((argv0 (uiop:argv0)))
        (and argv0
             (probe-file argv0)))))

(defun resolve-repo-root ()
  (let ((script-path (resolve-script-path)))
    (unless script-path
      (error "Unable to resolve the migrate-to-cas script path for ASDF bootstrap."))
    (let* ((script-directory (uiop:pathname-directory-pathname script-path))
           (repo-root (and script-directory
                           (uiop:pathname-parent-directory-pathname script-directory)))
           (asd-path (and repo-root
                          (merge-pathnames "claw-lisp.asd" repo-root))))
      (unless repo-root
        (error "Unable to derive repository root from script path: ~A" script-path))
      (unless (and asd-path (probe-file asd-path))
        (error "Resolved repository root does not contain claw-lisp.asd: ~A" repo-root))
      repo-root)))

(let ((repo-root (resolve-repo-root)))
  (pushnew repo-root asdf:*central-registry* :test #'equal)
  (asdf:load-system :claw-lisp))

(defun usage ()
  (format t
          "Usage: sbcl --script tools/migrate-to-cas.lisp -- [options]~%~%")
  (format t "Options:~%")
  (format t "  --data-root PATH         Set data root and derive artifacts/CAS roots.~%")
  (format t "  --artifacts-root PATH    Override legacy artifacts root.~%")
  (format t "  --cas-objects-root PATH  Override CAS objects root.~%")
  (format t "  --cas-ref-root PATH      Override CAS refs root.~%")
  (format t "  --session SESSION-ID     Restrict migration to one session.~%")
  (format t "  --report-file PATH       Write JSON summary to PATH.~%")
  (format t "  --strict                 Stop on the first migration error.~%")
  (format t "  --help                   Show this help text.~%"))

(defun script-command-line-arguments ()
  (or (let ((args (uiop:command-line-arguments)))
        (and args
             (not (null args))
             args))
      (let ((argv (and (boundp 'sb-ext:*posix-argv*)
                       sb-ext:*posix-argv*)))
        (when argv
          (let ((sentinel (position "--" argv :test #'string=)))
            (when sentinel
              (subseq argv (1+ sentinel))))))))

(defun parse-args (argv)
  (labels ((need-value (flag rest)
             (unless rest
               (error "Missing value for ~A" flag))
             (values (first rest) (rest rest))))
    (loop with args = (if (and argv (string= (first argv) "--"))
                          (rest argv)
                          argv)
          with parsed = nil
          while args
          do (let ((arg (first args)))
               (setf args (rest args))
               (cond
                 ((string= arg "--")
                  (setf args nil))
                 ((string= arg "--help")
                  (setf parsed (list* :help t parsed)))
                 ((string= arg "--strict")
                  (setf parsed (list* :strict t parsed)))
                 ((member arg '("--data-root" "--artifacts-root" "--cas-objects-root"
                                "--cas-ref-root" "--session" "--report-file")
                          :test #'string=)
                  (multiple-value-bind (value rest-args)
                      (need-value arg args)
                    (setf args rest-args)
                    (setf parsed
                          (append parsed
                                  (list (intern (string-upcase (subseq arg 2)) "KEYWORD")
                                        value)))))
                 (t
                  (error "Unknown argument: ~A" arg))))
          finally (return parsed))))

(defun ensure-directory-string (path)
  (when path
    (namestring (uiop:ensure-directory-pathname path))))

(defun maybe-write-report-file (report-path report-directory json)
  (when report-path
    (when report-directory
      (ensure-directories-exist report-directory))
    (with-open-file (stream report-path
                            :direction :output
                            :if-exists :supersede
                            :if-does-not-exist :create)
      (write-string json stream)
      (terpri stream))))

(defun report-parent-directory (report-path)
  (when report-path
    (make-pathname :name nil :type nil :defaults report-path)))

(defun apply-root-overrides (config args)
  (let ((data-root (getf args :DATA-ROOT)))
    (when data-root
      (let ((root (uiop:ensure-directory-pathname data-root)))
        (setf (claw-lisp.config:runtime-config-data-root config) (namestring root))
        (unless (getf args :ARTIFACTS-ROOT)
          (setf (claw-lisp.config:runtime-config-artifacts-root config)
                (namestring (merge-pathnames "artifacts/" root))))
        (unless (getf args :CAS-OBJECTS-ROOT)
          (setf (claw-lisp.config:runtime-config-cas-objects-root config)
                (namestring (merge-pathnames "cas/objects/" root))))
        (unless (getf args :CAS-REF-ROOT)
          (setf (claw-lisp.config:runtime-config-cas-ref-root config)
                (namestring (merge-pathnames "cas/refs/" root)))))))
  (when (getf args :ARTIFACTS-ROOT)
    (setf (claw-lisp.config:runtime-config-artifacts-root config)
          (ensure-directory-string (getf args :ARTIFACTS-ROOT))))
  (when (getf args :CAS-OBJECTS-ROOT)
    (setf (claw-lisp.config:runtime-config-cas-objects-root config)
          (ensure-directory-string (getf args :CAS-OBJECTS-ROOT))))
  (when (getf args :CAS-REF-ROOT)
    (setf (claw-lisp.config:runtime-config-cas-ref-root config)
          (ensure-directory-string (getf args :CAS-REF-ROOT))))
  config)

(defun main ()
  (let* ((raw-args (script-command-line-arguments))
         (args nil)
         (report-file nil)
         (report-path nil)
         (report-directory nil))
    (handler-case
        (progn
          (setf args (parse-args raw-args)
                report-file (getf args :REPORT-FILE)
                report-path (and report-file (pathname report-file))
                report-directory (report-parent-directory report-path))
          (when (getf args :help)
            (usage)
            (uiop:quit 0))
          (let* ((config (apply-root-overrides
                          (claw-lisp.config:make-default-runtime-config)
                          args))
                 (runtime (claw-lisp.core.runtime:make-runtime :config config))
                 (summary (claw-lisp.core.artifacts:migrate-legacy-tool-results-to-cas
                           runtime
                           :session-id (getf args :SESSION)
                           :continue-on-error-p (not (getf args :strict))))
                 (json (claw-lisp.providers.http-utils:json-encode-string summary)))
            (maybe-write-report-file report-path report-directory json)
            (format t "~A~%" json)
            (uiop:quit (if (> (getf summary :failure-count 0) 0) 2 0))))
      (claw-lisp.core.artifacts::legacy-tool-results-migration-aborted (condition)
        (let* ((summary (append (claw-lisp.core.artifacts::legacy-tool-results-migration-aborted-summary
                                 condition)
                                (list :aborted-p t
                                      :abort-reason
                                      (princ-to-string
                                       (claw-lisp.core.artifacts::legacy-tool-results-migration-aborted-cause
                                        condition)))))
               (json (claw-lisp.providers.http-utils:json-encode-string summary)))
          (maybe-write-report-file report-path report-directory json)
          (format t "~A~%" json)
          (uiop:quit 1)))
      (error (condition)
        (format *error-output* "Migration failed: ~A~%" condition)
        (usage)
        (uiop:quit 1)))))

(main)

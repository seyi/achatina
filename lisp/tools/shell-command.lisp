(in-package #:claw-lisp.tools.shell-command)

(defclass shell-command-tool (tool) ())

(defun make-shell-command-tool ()
  "Create the baseline local shell-command tool for explicit command execution."
  (make-instance 'shell-command-tool
                 :name "shell-command"
                 :description "Execute an explicit local shell command and return its output."))

(defmethod tool-input-schema ((tool shell-command-tool))
  (declare (ignore tool))
  (list :type "object"
        :properties (list :text (list :type "string"
                                      :description "The shell command to execute."))
        :required #("text")))

(defmethod validate-tool-input ((tool shell-command-tool) input)
  (declare (ignore tool))
  (unless (and (listp input)
               (getf input :text)
               (stringp (getf input :text)))
    (error "Shell-command tool requires a plist input with a string :text field."))
  input)

(defmethod execute-tool ((tool shell-command-tool) input runtime)
  (declare (ignore tool))
  (let ((command (getf input :text))
        (timeout-seconds (claw-lisp.config:runtime-config-shell-command-timeout-seconds
                          (claw-lisp.core.runtime:runtime-settings runtime))))
    (multiple-value-bind (stdout stderr status)
        (uiop:run-program (list "timeout"
                                (format nil "~Ds" timeout-seconds)
                                "sh"
                                "-lc"
                                command)
                          :output '(:string :stripped t)
                          :error-output '(:string :stripped t)
                          :ignore-error-status t)
      (let ((combined (string-trim '(#\Newline)
                                   (format nil "~@[~A~]~@[~%[stderr]~%~A~]"
                                           (and stdout (> (length stdout) 0) stdout)
                                           (and stderr (> (length stderr) 0) stderr)))))
        (list :command command
              :status status
              :output (cond
                        ((= status 124)
                         (format nil "[command timed out after ~D seconds; exit 124]"
                                 timeout-seconds))
                        ((> (length combined) 0)
                         combined)
                        (t
                         (format nil "[command exited ~D with no output]" status))))))))

(defmethod normalize-tool-result ((tool shell-command-tool) result)
  (declare (ignore tool))
  (let ((content (getf result :output)))
    (make-tool-result
     :tool-name "shell-command"
     :content content
     :bytes (length content))))

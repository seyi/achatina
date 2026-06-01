(in-package #:claw-lisp.core.claude-md)

;; --- CLAUDE.md Discovery and Loading ---
;;
;; Finds and reads CLAUDE.md files from:
;;   1. User-level: ~/.achatina/CLAUDE.md
;;   2. Project-level: $PROJECT_ROOT/CLAUDE.md
;;
;; Files are concatenated in precedence order (user first, then project).

(defvar *claude-md-filename* "CLAUDE.md"
  "The filename used for project-specific instructions.")

(defvar *user-claude-md-subdir* ".achatina"
  "The preferred subdirectory in the user's home directory for global CLAUDE.md.")

(defvar *legacy-user-claude-md-subdir* ".claw-lisp"
  "Legacy compatibility subdirectory for global CLAUDE.md discovery.")

(defun user-home-dir ()
  "Return the user's home directory as a pathname."
  (uiop:ensure-directory-pathname
   (or (uiop:getenv "HOME")
       (uiop:getenv "USERPROFILE")
       (user-homedir-pathname))))

(defun user-claude-md-path ()
  "Return the preferred user-level CLAUDE.md path, with legacy fallback."
  (labels ((candidate-path (subdir)
             (merge-pathnames
              *claude-md-filename*
              (merge-pathnames
               (make-pathname :directory (list :relative subdir))
               (user-home-dir)))))
    (or (probe-file (candidate-path *user-claude-md-subdir*))
        (probe-file (candidate-path *legacy-user-claude-md-subdir*)))))

(defun project-claude-md-paths (project-root)
  "Return a list of CLAUDE.md paths under PROJECT-ROOT.
   
   Checks:
   - $PROJECT_ROOT/CLAUDE.md
   - $PROJECT_ROOT/.claude/CLAUDE.md
   
   Returns paths in order: project root first, then .claude/ subdir."
  (let ((paths nil)
        (root (uiop:ensure-directory-pathname project-root)))
    ;; Direct CLAUDE.md in project root
    (let ((path (merge-pathnames *claude-md-filename* root)))
      (when (probe-file path)
        (push path paths)))
    ;; .claude/CLAUDE.md in project root
    (let ((subdir-path (merge-pathnames (make-pathname :directory '(:relative ".claude")) root)))
      (let ((path (merge-pathnames *claude-md-filename* subdir-path)))
        (when (probe-file path)
          (push path paths))))
    ;; Reverse so root is first, .claude/ is second
    (nreverse paths)))

(defun read-claude-md-file (path &key (max-size 50000))
  "Read the content of a CLAUDE.md file at PATH.
   Truncates to MAX-SIZE characters if file content exceeds that length.
   MAX-SIZE is measured in characters, not bytes, to avoid splitting
   multi-byte UTF-8 sequences."
  (handler-case
      (let ((truename (probe-file path)))
        (when truename
          (let ((content (uiop:read-file-string truename)))
            (if (> (length content) max-size)
                (progn
                  (warn "CLAUDE.md at ~A exceeds ~A characters, truncating" path max-size)
                  (concatenate 'string
                               (subseq content 0 max-size)
                               " ... [truncated]"))
                content))))
    (error (c)
      (warn "Failed to read CLAUDE.md at ~A: ~A" path c)
      nil)))

(defun load-claude-md-files (&key (project-root nil))
  "Load all CLAUDE.md files in precedence order.
   
   Returns a string with all files concatenated, each with a header.
   Returns NIL if no files exist."
  (let ((contents nil))
    ;; User-level CLAUDE.md (highest precedence)
    (let ((user-path (user-claude-md-path)))
      (when user-path
        (let ((content (read-claude-md-file user-path)))
          (when content
            (push (format nil "## User Instructions~%~%~A~%" content) contents)))))
    ;; Project-level CLAUDE.md files
    (when project-root
      (dolist (path (project-claude-md-paths project-root))
        (let ((content (read-claude-md-file path)))
          (when content
            (let ((rel-path (enough-namestring path (uiop:ensure-directory-pathname project-root))))
              (push (format nil "## Project Instructions (~A)~%~%~A~%"
                            (namestring rel-path)
                            content)
                    contents))))))
    (when contents
      (format nil "~{~A~^~%---~%~}" (nreverse contents)))))

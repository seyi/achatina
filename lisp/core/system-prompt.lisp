(in-package #:claw-lisp.core.system-prompt)

;; --- System Prompt Construction ---
;;
;; Builds the system prompt sent with every Anthropic API request.
;; Components:
;;   1. Base identity/behavior prompt
;;   2. CLAUDE.md content (user + project level)
;;   3. Tool descriptions from registered tools
;;   4. Dynamic context (date/time, git status, working directory)

(defparameter +base-system-prompt+
  "You are Claw, a coding assistant operating in a terminal environment.

## Core Principles
- You have access to tools that let you read files, write files, search code, and run shell commands.
- Use tools proactively when they will help complete the user's request.
- When editing files, prefer precise replacements over full rewrites.
- Always verify your changes work by running relevant tests or commands.
- If you're unsure about something, ask the user rather than guessing.

## Tool Usage
- Use the available tools to accomplish the user's request.
- When a tool call fails, read the error message carefully and fix the issue.
- Don't repeat the same failed tool call without changing the inputs.

## Output Format
- Be concise. Explain your reasoning when it helps the user understand.
- When you've completed a task, summarize what you did and any relevant details.
- If you encounter an error you can't resolve, explain what you tried and ask for guidance."
  "The base system prompt that establishes Claw's identity and behavior.")

(defparameter +base-system-prompt-directive+
  "You are Claw, a coding assistant.

TURN BUDGET: Complete the task in at most 3 tool turns.
- Turn 1: Read the file(s) named in the task.
- Turn 2: Write the fix. Use file-write with the full corrected file content when uncertain about the exact substring to replace.
- Turn 3: Run the verification command the user specified.

STRICT RULES:
1. Do not read a file more than once.
2. After a failed file-replace (substring not found), use file-write with the full corrected content — do not re-read.
3. Use the exact verification command the user specified, not an alternative.
4. After writing and running verification, stop.
5. Do not narrate your plan. Call tools."
  "Directive system prompt for models that benefit from explicit step-by-step turn budgeting.")

(defun model-family (model-string)
  "Return a keyword identifying the provider family for MODEL-STRING.
   MODEL-STRING is typically 'provider/model-name' from the session."
  (cond
    ((null model-string) :default)
    ((or (search "anthropic" model-string :test #'char-equal)
         (search "claude" model-string :test #'char-equal))
     :anthropic)
    ((or (search "openai" model-string :test #'char-equal)
         (search "gpt" model-string :test #'char-equal)
         (search "azure" model-string :test #'char-equal))
     :openai)
    ((or (search "moonshotai" model-string :test #'char-equal)
         (search "kimi" model-string :test #'char-equal))
     :moonshot)
    ((search "qwen" model-string :test #'char-equal)
     :qwen)
    (t :default)))

(defun safe-truncate-string (string max-chars &optional (suffix ""))
  "Truncate STRING to at most MAX-CHARS characters, appending SUFFIX if truncated.
   MAX-CHARS refers to character count (not byte count), so this is always safe
   for multi-byte UTF-8 when operating on CL character strings."
  (check-type string string)
  (check-type max-chars (integer 0))
  (if (<= (length string) max-chars)
      string
      (let ((end max-chars))
        ;; Back up if we land between CR and LF
        (when (and (> end 0)
                   (< end (length string))
                   (char= (char string (1- end)) #\Return)
                   (char= (char string end) #\Linefeed))
          (decf end))
        (concatenate 'string (subseq string 0 end) suffix))))

(defun format-git-context (project-root)
  "Return a string with git status and recent commits, or NIL if not a git repo.
   Truncates output to prevent token overflow."
  (let ((root (uiop:ensure-directory-pathname project-root)))
    (unless (probe-file (merge-pathnames ".git/" root))
      (return-from format-git-context nil))
    (handler-case
        (let* ((status (uiop:run-program
                        (list "git" "status" "--short")
                        :directory root
                        :output '(:string :stripped t)
                        :error-output nil
                        :ignore-error-status t))
               (log (uiop:run-program
                     (list "git" "log" "--oneline" "-5")
                     :directory root
                     :output '(:string :stripped t)
                     :error-output nil
                     :ignore-error-status t)))
          (format nil "## Git Context~%~%Working directory: ~A~%~%Status:~%~A~%~%Recent commits:~%~A~%"
                  (namestring root)
                  (if (and status (> (length status) 0))
                      (safe-truncate-string status 2000 " ... [truncated]")
                      "(clean)")
                  (if (and log (> (length log) 0))
                      (safe-truncate-string log 500 " ... [truncated]")
                      "(none)")))
      (error (c)
        (warn "Failed to get git context: ~A" c)
        nil))))

(defun format-tool-registry (tool-registry)
  "Return a string describing all registered tools and their schemas."
  (let ((descriptions nil))
    (loop for name being the hash-keys of tool-registry
          using (hash-value tool)
          do
             (let ((desc (claw-lisp.core.protocols:tool-description tool))
                   (schema (claw-lisp.core.protocols:tool-input-schema tool)))
               (when desc
                 (let ((entry (if schema
                                  (format nil "- **~A**: ~A~%  Schema: ~A~%" name desc schema)
                                  (format nil "- **~A**: ~A~%" name desc))))
                   (push entry descriptions)))))
    (when descriptions
      (format nil "## Available Tools~%~%~{~A~}" (nreverse descriptions)))))

(defun format-datetime ()
  "Return the current date/time as a formatted string."
  (multiple-value-bind (sec min hour day month year)
      (decode-universal-time (get-universal-time))
    (format nil "## Current Time~%~%~4,'0D-~2,'0D-~2,'0D ~2,'0D:~2,'0D:~2,'0D UTC~%"
            year month day hour min sec)))

(defun build-system-prompt (&key project-root tool-registry model)
  "Build the complete system prompt for a new conversation.

   PROJECT-ROOT is the project directory for CLAUDE.md discovery and git context.
   TOOL-REGISTRY is the hash table of registered tools.
   MODEL is the model identifier string used to select the appropriate base prompt.

   Returns the assembled system prompt string."
  (let* ((base-prompt (if (member (model-family model) '(:moonshot :qwen))
                          +base-system-prompt-directive+
                          +base-system-prompt+))
         (effective-root (or project-root (uiop:getcwd)))
         (parts (list base-prompt)))
    ;; CLAUDE.md content
    (let ((claude-md (claw-lisp.core.claude-md:load-claude-md-files
                      :project-root effective-root)))
      (when (and claude-md (> (length claude-md) 0))
        (push claude-md parts)))
    ;; Dynamic context
    (push (format-datetime) parts)
    ;; Git context
    (let ((git-context (format-git-context effective-root)))
      (when git-context
        (push git-context parts)))
    ;; Tool descriptions
    (when tool-registry
      (let ((tool-descs (format-tool-registry tool-registry)))
        (when tool-descs
          (push tool-descs parts))))
    ;; Assemble with separators
    (format nil "~{~A~^~%~%---~%~%~}" (nreverse parts))))

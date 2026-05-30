(in-package #:claw-lisp.storage.session-memory)

(defparameter +session-memory-structured-schema-version+ 1
  "Structured session-memory sidecar schema version.
This version is for local/runtime compatibility and may evolve in future phases.")

(defstruct (session-memory-metadata
            (:constructor make-session-memory-metadata
                          (&key update-count
                                last-updated-universal-time
                                budget-chars-used
                                budget-chars-max
                                stale-p
                                tokens-at-last-update
                                tool-count-at-last-update)))
  "Metadata for session memory tracking update count, timestamps, budget, staleness, and freshness."
  (update-count 0 :type integer)
  (last-updated-universal-time 0 :type integer)
  (budget-chars-used 0 :type integer)
  (budget-chars-max 0 :type integer)
  (stale-p nil :type boolean)
  (tokens-at-last-update 0 :type integer)
  (tool-count-at-last-update 0 :type integer))

(defun render-metadata-section (metadata)
  "Render the metadata section as a markdown comment block."
  (format nil "<!--~%; Session Memory Metadata~%- update-count: ~A~%- last-updated-universal-time: ~A~%- budget-chars-used: ~A~%- budget-chars-max: ~A~%- stale-p: ~A~%- tokens-at-last-update: ~A~%- tool-count-at-last-update: ~A~%-->~%"
          (session-memory-metadata-update-count metadata)
          (session-memory-metadata-last-updated-universal-time metadata)
          (session-memory-metadata-budget-chars-used metadata)
          (session-memory-metadata-budget-chars-max metadata)
          (if (session-memory-metadata-stale-p metadata) "true" "false")
          (session-memory-metadata-tokens-at-last-update metadata)
          (session-memory-metadata-tool-count-at-last-update metadata)))

(defun parse-session-memory-header (text)
  "Parse the session memory metadata from the markdown comment header in TEXT.
   Returns a session-memory-metadata struct or NIL if not found."
  (let ((start (search "<!--" text))
        (end (search "-->" text)))
    (when (and start end (> end start))
      (let* ((header (subseq text start end))
             (update-count (let ((pos (search "- update-count:" header)))
                             (when pos
                               (parse-integer header :start (+ pos 15) :junk-allowed t))))
             (last-updated (let ((pos (search "- last-updated-universal-time:" header)))
                             (when pos
                               (parse-integer header :start (+ pos 32) :junk-allowed t))))
             (budget-used (let ((pos (search "- budget-chars-used:" header)))
                            (when pos
                              (parse-integer header :start (+ pos 22) :junk-allowed t))))
             (budget-max (let ((pos (search "- budget-chars-max:" header)))
                           (when pos
                             (parse-integer header :start (+ pos 20) :junk-allowed t))))
             (stale-str (let ((pos (search "- stale-p:" header)))
                          (when pos
                            (string-trim '(#\Space #\Newline)
                                         (subseq header (+ pos 12))))))
             (tokens-at-last-update (let ((pos (search "- tokens-at-last-update:" header)))
                                      (when pos
                                        (parse-integer header :start (+ pos 24) :junk-allowed t))))
             (tool-count-at-last-update (let ((pos (search "- tool-count-at-last-update:" header)))
                                          (when pos
                                            (parse-integer header :start (+ pos 27) :junk-allowed t)))))
        (when (or update-count last-updated budget-used budget-max stale-str tokens-at-last-update tool-count-at-last-update)
          (make-session-memory-metadata
           :update-count (or update-count 0)
           :last-updated-universal-time (or last-updated 0)
           :budget-chars-used (or budget-used 0)
           :budget-chars-max (or budget-max 0)
           :stale-p (string= stale-str "true")
           :tokens-at-last-update (or tokens-at-last-update 0)
           :tool-count-at-last-update (or tool-count-at-last-update 0)))))))

(defun session-memory-stale-p (config metadata session)
  "Return T if session memory is stale: time exceeds max-staleness,
   token growth exceeds 2x threshold, or tool activity exceeds 2x threshold."
  (let* ((max-staleness (runtime-config-session-memory-max-staleness-seconds config))
         (token-threshold (runtime-config-session-memory-update-token-growth-threshold config))
         (tool-threshold (runtime-config-session-memory-update-tool-activity-threshold config))
         (now (get-universal-time))
         (last-update (session-memory-metadata-last-updated-universal-time metadata))
         (tokens-at-last-update (or (session-memory-metadata-tokens-at-last-update metadata) 0))
         (tool-count-at-last-update (or (session-memory-metadata-tool-count-at-last-update metadata) 0))
         (conversation (agent-session-conversation session))
         (current-tokens (claw-lisp.core.token-estimation:estimate-conversation-tokens conversation))
         (current-tool-count (length (conversation-tool-results conversation)))
         (time-stale (> (- now last-update) max-staleness))
         (token-stale (> (- current-tokens tokens-at-last-update) (* 2 token-threshold)))
         (tool-stale (> (- current-tool-count tool-count-at-last-update) (* 2 tool-threshold))))
    (or time-stale token-stale tool-stale)))

;;; ============================================================
;;; Path & Rendering Functions
;;; ============================================================

(defun session-memory-path (config session-id)
  "Return the markdown session-memory pathname for SESSION-ID."
  (merge-pathnames
   (make-pathname :directory '(:relative "session")
                  :name session-id
                  :type "md")
   (uiop:ensure-directory-pathname
    (runtime-config-memory-root config))))

(defun session-memory-structured-path (config session-id)
  "Return the structured JSON session-memory pathname for SESSION-ID."
  (merge-pathnames
   (make-pathname :directory '(:relative "session")
                  :name session-id
                  :type "json")
   (uiop:ensure-directory-pathname
    (runtime-config-memory-root config))))

(defun recent-message-lines (session)
  "Return up to five recent message summary lines for SESSION."
  (let ((messages (conversation-messages (agent-session-conversation session))))
    (loop for message in (last messages (min 5 (length messages)))
          collect
          (format nil "- [~A] ~A"
                  (string-downcase (symbol-name (message-role message)))
                  (message-content-text message)))))

(defun recent-tool-lines (session)
  "Return up to five recent tool-result summary lines for SESSION."
  (let ((results (conversation-tool-results (agent-session-conversation session))))
    (loop for result in (last results (min 5 (length results)))
          collect
          (format nil "- ~A: ~A~@[ (full: ~A)~]"
                  (tool-result-tool-name result)
                  (tool-result-content result)
                  (tool-result-persisted-path result)))))

(defun render-section (title lines &optional (fallback "- none"))
  "Render a markdown section with TITLE and bullet LINES."
  (format nil "## ~A~%~{~A~%~}~%"
          title
          (if (and lines (plusp (length lines))) lines (list fallback))))

(defun render-active-goals (session)
  "Return a list of strings describing active goals/tasks for SESSION.
   Placeholder implementation returns nil — to be implemented in Task 3-5."
  ;; TODO: Replace with real active goals extraction from session
  nil)

(defun render-key-decisions (session)
  "Return a list of strings describing key decisions made in SESSION.
   Placeholder implementation returns nil — to be implemented in Task 6."
  ;; TODO: Replace with real key decisions extraction from session
  nil)

(defun render-working-state (session)
  "Return a list of strings describing current working state of SESSION."
  (let* ((messages (conversation-messages (agent-session-conversation session)))
         (last-message (car (last messages))))
    (if last-message
        (list (format nil "- latest ~A message: ~A"
                      (string-downcase (symbol-name (message-role last-message)))
                      (message-content-text last-message)))
        nil)))

(defun render-recent-activity (session)
  "Return a list of strings describing recent activity (messages and tool results)."
  (append (recent-message-lines session)
          (recent-tool-lines session)))

(defun recent-activity-records (session)
  "Return structured recent activity records for SESSION."
  (let* ((conversation (agent-session-conversation session))
         (messages (conversation-messages conversation))
         (tools (conversation-tool-results conversation))
         (message-records
           (loop for message in (last messages (min 5 (length messages)))
                 collect (list :role (string-downcase (symbol-name (message-role message)))
                               :text (message-content-text message))))
         (tool-records
           (loop for result in (last tools (min 5 (length tools)))
                 collect (list :role "tool"
                               :text (tool-result-content result)
                               :tool_name (tool-result-tool-name result)
                               :call_id (tool-result-call-id result)))))
    (append message-records tool-records)))

(defun render-session-memory-structured (session &key (metadata nil))
  "Render SESSION into a structured session-memory plist for JSON serialization.

Schema contract (v1):
- :schema_version integer
- :session_id string
- :updated_at_universal_time integer
- :source string
- :summary string (human-oriented, not a stable semantic summary contract)
- :recent_activity list of plists with :role/:text and optional tool keys
- :metadata plist when available

This sidecar is additive and non-authoritative; markdown remains the primary
backward-compatible artifact."
  (let ((working-state (render-working-state session)))
    (list :schema_version +session-memory-structured-schema-version+
          :session_id (agent-session-id session)
          :updated_at_universal_time
          (if metadata
              (session-memory-metadata-last-updated-universal-time metadata)
              (get-universal-time))
          :source "session-memory-update"
          :summary (if working-state (first working-state) "")
          :recent_activity (recent-activity-records session)
          :metadata
          (if metadata
              (list :update_count (session-memory-metadata-update-count metadata)
                    :budget_chars_used (session-memory-metadata-budget-chars-used metadata)
                    :budget_chars_max (session-memory-metadata-budget-chars-max metadata)
                    :stale_p (session-memory-metadata-stale-p metadata)
                    :tokens_at_last_update (session-memory-metadata-tokens-at-last-update metadata)
                    :tool_count_at_last_update (session-memory-metadata-tool-count-at-last-update metadata))
              nil))))

(defun write-session-memory-structured (config session &key (metadata nil))
  "Write structured session-memory JSON sidecar for SESSION."
  (let ((structured-path (session-memory-structured-path config (agent-session-id session))))
    (ensure-directories-exist structured-path)
    (with-open-file (stream structured-path
                            :direction :output
                            :if-exists :supersede
                            :if-does-not-exist :create)
      (claw-lisp.providers.http-utils:json-encode
       (render-session-memory-structured session :metadata metadata)
       stream))
    structured-path))

(defun render-session-memory (session &key (metadata nil))
  "Render SESSION into a richer markdown working-memory note.
   If METADATA (session-memory-metadata) is provided, include it in a comment header."
  (with-output-to-string (out)
    ;; Metadata section as comment header
    (when metadata
      (write-string (render-metadata-section metadata) out))
    ;; Title
    (format out "# Session ~A~%~%" (agent-session-id session))
    ;; Provider and model info
    (format out "- provider: ~A~%" (agent-session-provider session))
    (format out "- model: ~A~%~%" (agent-session-model session))
    ;; Sections
    (write-string (render-section "Active Goals" (render-active-goals session)) out)
    (write-string (render-section "Key Decisions" (render-key-decisions session)) out)
    (write-string (render-section "Working State" (render-working-state session)) out)
    (write-string (render-section "Recent Activity" (render-recent-activity session)) out)))

;; Update function remains backward compatible by calling render-session-memory without metadata
(defun update-session-memory (config session &key (metadata nil) (rendered-note-text nil))
  "Rewrite the session working-memory artifact for SESSION.
   If METADATA is provided, include it in the rendered note.
   Optional RENDERED-NOTE-TEXT avoids re-rendering markdown in callers.
   The structured JSON sidecar is best-effort and non-authoritative."
  (let* ((path (session-memory-path config (agent-session-id session)))
         (note-text (or rendered-note-text
                        (render-session-memory session :metadata metadata))))
    (ensure-directories-exist path)
    (with-open-file (stream path
                            :direction :output
                            :if-exists :supersede
                            :if-does-not-exist :create)
      (write-string note-text stream))
    ;; Structured sidecar is best-effort; markdown remains authoritative for compatibility.
    ;; Catching broad ERROR is intentional here to avoid breaking core memory writes.
    (handler-case
        (write-session-memory-structured config session :metadata metadata)
      (error (condition)
        (warn "Failed to write structured session memory for session ~A: ~A"
              (agent-session-id session)
              condition)))
    path))

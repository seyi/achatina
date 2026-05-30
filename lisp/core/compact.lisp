(in-package #:claw-lisp.core.compact)

(defparameter +fallback-summary-message-limit+ 6
  "Maximum number of recent messages to include in deterministic fallback summaries.")

(defun string-prefix-p (prefix string)
  "Return T when STRING begins with PREFIX."
  (and (stringp prefix)
       (stringp string)
       (<= (length prefix) (length string))
       (string= prefix string :end2 (length prefix))))

(defun non-empty-file-text (pathname)
  "Return the file text at PATHNAME when it exists and is non-empty."
  (when (probe-file pathname)
    (let ((text (uiop:read-file-string pathname)))
      (when (> (length text) 0)
        text))))

(defun preserved-recent-messages (session keep-recent)
  "Return the last KEEP-RECENT conversation messages from SESSION."
  (let ((messages (conversation-messages
                   (claw-lisp.core.domain:agent-session-conversation session))))
    (last messages (min keep-recent (length messages)))))

;;; ============================================================
;;; Session-Memory-Assisted Selective Compaction (Phase 5 Task 6)
;;; ============================================================

(defun strip-session-memory-metadata (text)
  "Strip the leading metadata comment block from TEXT if present.
   Returns the body of the session-memory note without the <!-- ... --> header."
  (let ((start (search "<!--" text))
        (end (search "-->" text)))
    (if (and start end (> end start))
        (subseq text (+ end 3)) ; skip past closing -->
        text)))

(defun extract-session-memory-recent-activity-lines (text)
  "Extract bullet lines from the 'Recent Activity' section of a session-memory note TEXT.
   Returns a list of strings (each including the leading '- ')."
  (let* ((lines (split-sequence:split-sequence #\Newline text))
         (in-section nil)
         (result '()))
    (dolist (line lines (nreverse result))
      (cond
        ;; Enter section when we see the header
        ((and (not in-section)
              (string-prefix-p "## Recent Activity" (string-trim '(#\Space) line)))
         (setf in-section t))
        ;; If we are in the section, collect bullet lines until next header
        (in-section
         (cond
           ;; Next markdown header ends the section
           ((and (plusp (length line))
                 (char= (char line 0) #\#))
            (setf in-section nil))
           ;; Bullet line
           ((and (plusp (length line))
                 (char= (char line 0) #\-))
            (push (string-trim '(#\Space #\Tab) line) result))))))))

(defun render-message-bullet-for-coverage (message)
  "Render MESSAGE into the same bullet format used by recent-message-lines
   for coverage comparison."
  (format nil "- [~A] ~A"
          (string-downcase
           (symbol-name (claw-lisp.core.domain:message-role message)))
          (claw-lisp.core.domain:message-content-text message)))

(defun find-covered-message-suffix-length (session session-memory-text)
  "Return the length of the largest contiguous suffix of SESSION messages
   whose rendered bullets all appear in the session-memory Recent Activity section.

   This is used as a heuristic for which messages are already represented in
   session memory."
  (let* ((conversation (claw-lisp.core.domain:agent-session-conversation session))
         (messages (claw-lisp.core.domain:conversation-messages conversation))
         (activity-lines (extract-session-memory-recent-activity-lines session-memory-text))
         (activity-set (and activity-lines (make-hash-table :test 'equal))))
    (when activity-set
      (dolist (line activity-lines)
        (setf (gethash line activity-set) t)))
    (if (or (null activity-set) (zerop (hash-table-count activity-set)))
        0
        (let* ((len (length messages))
               (suffix-len 0))
          ;; Walk from the end backwards while bullets are present in activity-set
          (loop for idx downfrom (1- len) to 0
                for msg = (nth idx messages)
                for bullet = (render-message-bullet-for-coverage msg)
                while (gethash bullet activity-set)
                do (incf suffix-len)
                finally (return suffix-len))))))

(defun build-selective-compaction-summary (session session-memory-text
                                           &key uncovered-messages-count
                                                summarized-messages
                                                preserved-tail-count)
  "Build the combined selective compaction summary string.

   SESSION-MEMORY-TEXT is the full note (including metadata); we strip the
   metadata header and embed the body. SUMMARIZED-MESSAGES is the list of
   older uncovered messages we are summarizing deterministically. PRESERVED-TAIL-COUNT
   is the number of recent messages preserved verbatim."
  (let* ((body (strip-session-memory-metadata session-memory-text))
         (session-id (claw-lisp.core.domain:agent-session-id session)))
    (with-output-to-string (stream)
      (format stream "# Selective Compaction Summary~%~%")
      (format stream "Session: ~A~%~%" session-id)
      (format stream "## Provenance~%")
      (format stream "- source: session-memory-selective~%")
      (format stream "- session_memory_used: true~%")
      (format stream "- uncovered_messages_count: ~D~%"
              (or uncovered-messages-count 0))
      (format stream "- summarized_messages_count: ~D~%"
              (length summarized-messages))
      (format stream "- preserved_tail_count: ~D~%~%"
              (or preserved-tail-count 0))
      (format stream "## Session Memory Snapshot~%")
      (write-string (string-trim '(#\Newline #\Space) body) stream)
      (format stream "~%~%## Selective Summary of Older Messages~%")
      (if summarized-messages
          (dolist (msg summarized-messages)
            (format stream "~A~%"
                    (summarize-message-line msg)))
          (format stream "- none (all relevant history already captured in session memory)~%")))))

(defun session-compaction-depth (session)
  "Return how many times SESSION has been compacted."
  (or (getf (agent-session-state session) :compaction-depth) 0))

(defun session-last-compaction-fingerprint (session)
  "Return the fingerprint of the previous compaction, or NIL."
  (getf (agent-session-state session) :last-compaction-fingerprint))

(defun build-selective-compaction-ir (session session-memory-text
                                      &key uncovered-messages-count
                                           summarized-messages
                                           preserved-tail-count
                                           total-messages-before)
  "Build a compaction-ir for the session-memory-selective path."
  (let* ((body (strip-session-memory-metadata session-memory-text))
         (session-id (claw-lisp.core.domain:agent-session-id session))
         (provenance (make-compaction-ir-provenance
                      :session-memory-used-p t
                      :uncovered-messages-count (or uncovered-messages-count 0)
                      :summarized-messages-count (length summarized-messages)
                      :preserved-tail-count (or preserved-tail-count 0)
                      :total-messages-before (or total-messages-before 0)
                      :compaction-depth (session-compaction-depth session)))
         (prov-section
           (make-compaction-ir-section
            :kind :provenance
            :heading "Provenance"
            :priority :high
            :items (list
                    (make-compaction-ir-item
                     :type :key-value
                     :text "- source: session-memory-selective")
                    (make-compaction-ir-item
                     :type :key-value
                     :text "- session_memory_used: true")
                    (make-compaction-ir-item
                     :type :key-value
                     :text (format nil "- uncovered_messages_count: ~D"
                                   (or uncovered-messages-count 0)))
                    (make-compaction-ir-item
                     :type :key-value
                     :text (format nil "- summarized_messages_count: ~D"
                                   (length summarized-messages)))
                    (make-compaction-ir-item
                     :type :key-value
                     :text (format nil "- preserved_tail_count: ~D"
                                   (or preserved-tail-count 0))))))
         (mem-section
           (make-compaction-ir-section
            :kind :session-memory-snapshot
            :heading "Session Memory Snapshot"
            :priority :high
            :items (list (make-compaction-ir-item
                          :type :raw-text
                          :text (string-trim '(#\Newline #\Space) body)))))
         (msg-section
           (make-compaction-ir-section
            :kind :message-summary
            :heading "Selective Summary of Older Messages"
            :priority :normal
            :items (if summarized-messages
                       (loop for msg in summarized-messages
                             for idx from 0
                             collect (make-compaction-ir-item
                                      :type :bullet
                                      :text (summarize-message-line msg)
                                      :role (message-role msg)
                                      :message-index idx))
                       (list (make-compaction-ir-item
                              :type :bullet
                              :text "- none (all relevant history already captured in session memory)"))))))
    (make-compaction-ir
     :id (format nil "compact-~A-~A" session-id (get-universal-time))
     :source :session-memory-selective
     :session-id session-id
     :predecessor-fingerprint (session-last-compaction-fingerprint session)
     :provenance provenance
     :sections (list prov-section mem-section msg-section))))

(defun try-session-memory-compaction (config session &key (keep-recent-messages 4))
  "Selective session-memory-assisted compaction.

   Steps:
   1. Load session-memory artifact; if missing/empty, return NIL.
   2. If metadata indicates staleness, return NIL (caller will fall back).
   3. Detect which messages are already represented in session memory via
      Recent Activity coverage.
   4. Build a structured IR with provenance, session memory snapshot, and
      deterministic summary of older uncovered messages.
   5. Return a compaction-result with IR and preserved tail messages."
  (let* ((path (session-memory-path config
                                    (claw-lisp.core.domain:agent-session-id session)))
         (summary-text (non-empty-file-text path)))
    (when summary-text
      ;; Parse metadata if present; if stale, do not use session memory.
      (let* ((metadata (claw-lisp.storage.session-memory:parse-session-memory-header
                        summary-text))
             (conversation (claw-lisp.core.domain:agent-session-conversation session)))
        (when (and metadata
                   (claw-lisp.storage.session-memory:session-memory-stale-p
                    config metadata session))
          (return-from try-session-memory-compaction nil))
        (let* ((messages (claw-lisp.core.domain:conversation-messages conversation))
               (total-count (length messages))
               (covered-suffix-len
                 (find-covered-message-suffix-length session summary-text))
               (preserved-tail
                 (preserved-recent-messages session keep-recent-messages))
               (preserved-tail-count (length preserved-tail))
               (uncovered-prefix-count (max 0 (- total-count covered-suffix-len)))
               (summarized-count (max 0 (- uncovered-prefix-count preserved-tail-count)))
               (summarized-messages
                 (subseq messages 0 summarized-count))
               (uncovered-messages-count uncovered-prefix-count)
               (ir (build-selective-compaction-ir
                    session summary-text
                    :uncovered-messages-count uncovered-messages-count
                    :summarized-messages summarized-messages
                    :preserved-tail-count preserved-tail-count
                    :total-messages-before total-count)))
          (estimate-and-trim-ir-sections ir config)
          (make-compaction-result
           :source :session-memory-selective
           :ir ir
           :preserved-messages preserved-tail))))))

(defun summarize-message-line (message)
  "Render MESSAGE as a single compact summary line."
  (format nil "- ~A: ~A"
          (string-downcase
           (symbol-name (claw-lisp.core.domain:message-role message)))
          (claw-lisp.core.domain:message-content-text message)))

(defun summarize-tool-result-line (result)
  "Render RESULT as a single compact summary line."
  (format nil "- ~A (~D bytes): ~A~@[ [persisted]~]"
          (claw-lisp.core.domain:tool-result-tool-name result)
          (claw-lisp.core.domain:tool-result-bytes result)
          (claw-lisp.core.domain:tool-result-content result)
          (and (claw-lisp.core.domain:tool-result-persisted-path result) t)))

(defun fallback-compaction-ir (session)
  "Build a compaction-ir for the deterministic fallback path."
  (let* ((conversation (claw-lisp.core.domain:agent-session-conversation session))
         (messages (conversation-messages conversation))
         (tool-results (claw-lisp.core.domain:conversation-tool-results conversation))
         (recent-messages (last messages
                               (min +fallback-summary-message-limit+
                                    (length messages))))
         (recent-results (last tool-results (min 4 (length tool-results))))
         (session-id (claw-lisp.core.domain:agent-session-id session))
         (provenance (make-compaction-ir-provenance
                      :session-memory-used-p nil
                      :summarized-messages-count (length recent-messages)
                      :total-messages-before (length messages)
                      :tool-results-summarized-count (length recent-results)
                      :compaction-depth (session-compaction-depth session)))
         (msg-section
           (make-compaction-ir-section
            :kind :message-summary
            :heading "Recent messages"
            :priority :normal
            :items (if recent-messages
                       (loop for msg in recent-messages
                             collect (make-compaction-ir-item
                                      :type :bullet
                                      :text (summarize-message-line msg)
                                      :role (message-role msg)))
                       (list (make-compaction-ir-item :type :bullet :text "- none")))))
         (tool-section
           (make-compaction-ir-section
            :kind :tool-result-summary
            :heading "Recent tool results"
            :priority :low
            :items (if recent-results
                       (loop for result in recent-results
                             collect (make-compaction-ir-item
                                      :type :bullet
                                      :text (summarize-tool-result-line result)
                                      :tool-name (tool-result-tool-name result)
                                      :persisted-path
                                      (tool-result-persisted-path result)
                                      :call-id (tool-result-call-id result)
                                      :bytes (tool-result-bytes result)))
                       (list (make-compaction-ir-item :type :bullet :text "- none"))))))
    (make-compaction-ir
     :id (format nil "compact-fallback-~A-~A" session-id (get-universal-time))
     :source :fallback
     :session-id session-id
     :predecessor-fingerprint (session-last-compaction-fingerprint session)
     :provenance provenance
     :sections (list msg-section tool-section))))

(defun fallback-compaction-summary (session)
  "Build a deterministic local compaction summary from the current SESSION state.
   Returns a markdown string (legacy interface)."
  (render-compaction-ir-to-markdown (fallback-compaction-ir session)))

(defun compact-session-locally (config session &key (keep-recent-messages 4))
  "Return a baseline compaction result for SESSION without model-driven summarization.

This conservative path reuses session memory when available. If not, it falls
back to a deterministic local summary derived from the current session state."
  (or (try-session-memory-compaction config
                                     session
                                     :keep-recent-messages keep-recent-messages)
      (let ((ir (fallback-compaction-ir session)))
        (estimate-and-trim-ir-sections ir config)
        (make-compaction-result
         :source :fallback
         :ir ir
         :preserved-messages
         (preserved-recent-messages session keep-recent-messages)))))

(defun make-compaction-boundary-message (result)
  "Render RESULT into a compact boundary message for conversation state."
  (claw-lisp.core.domain:make-message
   :role :system
   :content (format nil "# Compaction Boundary~%~%~A"
                    (compaction-result-rendered-summary result))
   :metadata (list :event "compaction_boundary"
                   :source (claw-lisp.core.domain:compaction-result-source result)
                   :preserved_count
                   (length (claw-lisp.core.domain:compaction-result-preserved-messages result)))))

(defun restored-tool-results (config session)
  "Return the recent tool-result slice preserved after compaction."
  (let* ((conversation (claw-lisp.core.domain:agent-session-conversation session))
         (results (claw-lisp.core.domain:conversation-tool-results conversation))
         (keep-recent (runtime-config-post-compact-keep-recent-tool-results config)))
    (last results (min keep-recent (length results)))))

(defun apply-compaction-result (config session result)
  "Apply RESULT to SESSION by replacing older messages with a boundary plus preserved tail.

This baseline path also restores a bounded recent tool-result slice after the
summary boundary."
  (let* ((conversation (claw-lisp.core.domain:agent-session-conversation session))
         (boundary (make-compaction-boundary-message result))
         (messages (cons boundary
                         (copy-list
                          (claw-lisp.core.domain:compaction-result-preserved-messages result))))
         (results (copy-list (restored-tool-results config session))))
    (setf (claw-lisp.core.domain:conversation-messages conversation) messages)
    (replace-tool-results conversation results)
    conversation))

;;; ============================================================
;;; Structured Compaction IR (Phase 10)
;;; ============================================================

(defun compaction-result-rendered-summary (result)
  "Return the markdown summary string for RESULT.
   Computes and caches from IR if not already present."
  (let ((cached (compaction-result-summary result)))
    (if (and cached (plusp (length cached)))
        cached
        (if (compaction-result-ir result)
            (let ((rendered (render-compaction-ir-to-markdown
                             (compaction-result-ir result))))
              (setf (claw-lisp.core.domain:compaction-result-summary result) rendered)
              rendered)
            ""))))

(defun render-compaction-ir-to-markdown (ir)
  "Render a compaction-ir struct into markdown compatible with the existing
   Compaction Boundary message format."
  (with-output-to-string (stream)
    (format stream "# ~A Compaction Summary~%~%"
            (case (compaction-ir-source ir)
              (:session-memory-selective "Selective")
              (:fallback "Fallback")
              (otherwise "Compaction")))
    (format stream "Session: ~A~%~%" (compaction-ir-session-id ir))
    (dolist (section (compaction-ir-sections ir))
      (unless (compaction-ir-section-trimmed-p section)
        (render-section-to-markdown section stream)))))

(defun render-section-to-markdown (section stream)
  "Render one IR section to STREAM as markdown."
  (format stream "## ~A~%" (compaction-ir-section-heading section))
  (dolist (item (compaction-ir-section-items section))
    (case (compaction-ir-item-type item)
      (:bullet
       (format stream "~A~%" (compaction-ir-item-text item)))
      (:key-value
       (format stream "~A~%" (compaction-ir-item-text item)))
      (:raw-text
       (write-string (compaction-ir-item-text item) stream)
       (terpri stream))))
  (terpri stream))

(defun compaction-ir-to-plist (ir)
  "Serialize IR to a plist suitable for transcript events and JSON export."
  (list :id (compaction-ir-id ir)
        :source (compaction-ir-source ir)
        :created_universal_time (claw-lisp.core.domain:compaction-ir-created-universal-time ir)
        :session_id (compaction-ir-session-id ir)
        :predecessor_fingerprint (compaction-ir-predecessor-fingerprint ir)
        :token_budget (compaction-ir-token-budget ir)
        :tokens_used (compaction-ir-tokens-used ir)
        :provenance (when (compaction-ir-provenance ir)
                      (compaction-ir-provenance-to-plist
                       (compaction-ir-provenance ir)))
        :sections (mapcar #'compaction-ir-section-to-plist
                          (compaction-ir-sections ir))))

(defun compaction-ir-provenance-to-plist (prov)
  "Serialize provenance to a plist."
  (list :session_memory_used_p
        (claw-lisp.core.domain:compaction-ir-provenance-session-memory-used-p prov)
        :uncovered_messages_count
        (claw-lisp.core.domain:compaction-ir-provenance-uncovered-messages-count prov)
        :summarized_messages_count
        (claw-lisp.core.domain:compaction-ir-provenance-summarized-messages-count prov)
        :preserved_tail_count
        (claw-lisp.core.domain:compaction-ir-provenance-preserved-tail-count prov)
        :total_messages_before
        (claw-lisp.core.domain:compaction-ir-provenance-total-messages-before prov)
        :tool_results_summarized_count
        (claw-lisp.core.domain:compaction-ir-provenance-tool-results-summarized-count prov)
        :compaction_depth
        (compaction-ir-provenance-compaction-depth prov)))

(defun compaction-ir-section-to-plist (section)
  "Serialize one section to a plist."
  (list :kind (compaction-ir-section-kind section)
        :heading (compaction-ir-section-heading section)
        :tokens_estimated (compaction-ir-section-tokens-estimated section)
        :trimmed_p (compaction-ir-section-trimmed-p section)
        :priority (compaction-ir-section-priority section)
        :item_count (length (compaction-ir-section-items section))))

(defun compaction-ir-fingerprint (ir)
  "Compute a fingerprint string for IR suitable for within-session provenance
   chaining. Uses sxhash which is not stable across SBCL versions or restarts;
   do not rely on this for cross-session comparison without switching to a
   cryptographic hash."
  (format nil "~16,'0X" (sxhash (render-compaction-ir-to-markdown ir))))

(defun estimate-section-tokens (section)
  "Estimate tokens for all items in SECTION using character heuristics."
  (let ((chars (+ 4 (length (compaction-ir-section-heading section)))))
    (dolist (item (compaction-ir-section-items section))
      (incf chars (+ 1 (length (compaction-ir-item-text item)))))
    (claw-lisp.core.token-estimation:estimate-string-tokens
     (make-string chars :initial-element #\x))))

(defun section-priority-rank (priority)
  "Return numeric rank for section PRIORITY.
Unknown priorities are treated as :normal for backward-compatible trim behavior."
  (let ((order '(:low 0 :normal 1 :high 2)))
    (or (getf order priority) 1)))

(defun section-priority< (a b)
  "Return T if section A should be trimmed before section B.
Lower rank trims earlier (:low before :normal before :high)."
  (< (section-priority-rank (compaction-ir-section-priority a))
     (section-priority-rank (compaction-ir-section-priority b))))

(defun estimate-and-trim-ir-sections (ir config)
  "Estimate tokens for each section in IR and trim lowest-priority sections
   if total exceeds the configured budget. Mutates IR in place."
  (let ((budget (or (compaction-ir-token-budget ir)
                    (runtime-config-compaction-summary-token-budget config)
                    most-positive-fixnum))
        (total 0))
    (dolist (section (compaction-ir-sections ir))
      ;; Always recompute trim state from scratch for deterministic results.
      (setf (claw-lisp.core.domain:compaction-ir-section-trimmed-p section) nil)
      (let ((section-tokens (estimate-section-tokens section)))
        (setf (claw-lisp.core.domain:compaction-ir-section-tokens-estimated section)
              section-tokens)
        (incf total section-tokens)))
    (when (> total budget)
      (let* ((sections (compaction-ir-sections ir))
             (provenance-sections
               (remove-if-not (lambda (section)
                                (eq (compaction-ir-section-kind section) :provenance))
                              sections))
             (non-provenance-sections
               (remove-if (lambda (section)
                            (eq (compaction-ir-section-kind section) :provenance))
                          sections))
             ;; Keep one non-provenance section (best priority) for minimal context.
             (must-keep-non-provenance
               (car (sort (copy-list non-provenance-sections)
                          (lambda (a b)
                            (section-priority< b a)))))
             (trimmable
               (remove-if (lambda (section)
                            (or (member section provenance-sections :test #'eq)
                                (and must-keep-non-provenance
                                     (eq section must-keep-non-provenance))))
                          sections))
             (sorted (sort (copy-list trimmable) #'section-priority<)))
        (dolist (section sorted)
          (when (<= total budget) (return))
          (setf (claw-lisp.core.domain:compaction-ir-section-trimmed-p section) t)
          (decf total (compaction-ir-section-tokens-estimated section)))))
    (setf (claw-lisp.core.domain:compaction-ir-tokens-used ir) total)
    ir))

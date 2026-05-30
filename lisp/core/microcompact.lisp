(in-package #:claw-lisp.core.microcompact)

(defparameter +cleared-tool-result-placeholder+
  "[old tool result content cleared; see persisted-path for full content]")

(defun clearable-tool-result-p (result)
  "Return true when RESULT can be downgraded during microcompact."
  (and (tool-result-persisted-path result)
       (> (length (tool-result-content result)) 0)
       (not (string= (tool-result-content result)
                     +cleared-tool-result-placeholder+))))

(defun clear-tool-result-preview (result)
  "Return RESULT with its preview downgraded to the microcompact placeholder."
  (make-tool-result
   :call-id (tool-result-call-id result)
   :tool-name (tool-result-tool-name result)
   :content +cleared-tool-result-placeholder+
   :persisted-path (tool-result-persisted-path result)
   :truncated-p t
   :bytes (tool-result-bytes result)))

(defun microcompact-conversation-tool-results (config conversation)
  "Downgrade older persisted tool previews while keeping recent ones in memory.

This baseline microcompact only reduces in-memory preview footprint. It never
deletes the persisted artifact; callers can still recover full content via
`persisted-path`."
  (let* ((keep-recent
           (max 1 (runtime-config-microcompact-keep-recent-tool-results config)))
         (results (conversation-tool-results conversation))
         (clearable-seen 0)
         (cleared-count 0)
         (updated
           (reverse
            (loop for result in (reverse results)
                  collect
                  (if (clearable-tool-result-p result)
                      (if (< clearable-seen keep-recent)
                          (progn
                            (incf clearable-seen)
                            result)
                          (progn
                            (incf cleared-count)
                            (clear-tool-result-preview result)))
                      result)))))
    (replace-tool-results conversation updated)
    cleared-count))

(defun tool-result-preview-bytes (result)
  "Return the current in-memory preview size for RESULT."
  (if (string= (tool-result-content result)
               +cleared-tool-result-placeholder+)
      0
      (length (tool-result-content result))))

(defun enforce-tool-result-aggregate-budget (config conversation)
  "Clear older persisted previews until the aggregate preview budget is met.

This baseline pass only downgrades persisted previews. Inline-only results are
left intact even when they exceed the aggregate budget on their own."
  (let* ((budget (runtime-config-tool-result-aggregate-budget-bytes config))
         ;; Baseline eviction assumes conversation tool results are oldest-first.
         (results (conversation-tool-results conversation))
         (total-bytes (reduce #'+ results
                              :key #'tool-result-preview-bytes
                              :initial-value 0))
         (cleared-count 0)
         (updated results))
    (when (> total-bytes budget)
      (setf updated
            (loop for result in results
                  collect
                  (if (and (> total-bytes budget)
                           (clearable-tool-result-p result))
                      (progn
                        (decf total-bytes (tool-result-preview-bytes result))
                        (incf cleared-count)
                        (clear-tool-result-preview result))
                      result)))
      (replace-tool-results conversation updated))
    cleared-count))

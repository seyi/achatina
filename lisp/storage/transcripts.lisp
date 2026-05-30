(in-package #:claw-lisp.storage.transcripts)

;; NOTE: This baseline implementation assumes a single writer per session.
;; If concurrent writers are introduced later, add per-session locking or a
;; serialized writer queue before relying on these append operations.

(defun ensure-directory-path (pathname)
  "Ensure the directory containing PATHNAME exists."
  (ensure-directories-exist pathname)
  pathname)

(defun transcript-path-for-session (config session-id)
  "Return the JSONL transcript pathname for SESSION-ID."
  (let ((root (runtime-config-transcripts-root config)))
    (merge-pathnames
     (make-pathname :name session-id :type "jsonl")
     (uiop:ensure-directory-pathname root))))

(defun append-line (pathname line)
  "Append LINE plus a newline to PATHNAME."
  (ensure-directory-path pathname)
  (with-open-file (stream pathname
                          :direction :output
                          :if-exists :append
                          :if-does-not-exist :create)
    (write-string line stream)
    (terpri stream)))

(defun append-transcript-event (pathname event)
  "Append EVENT as one JSON object line to PATHNAME using yason."
  (let ((json (with-output-to-string (stream)
                (yason:encode (claw-lisp.providers.http-utils:value->json-safe event) stream))))
    (append-line pathname json)))

(defun ensure-session-transcript (config session)
  "Create the session transcript file if needed and record the session-start event."
  (let ((path (transcript-path-for-session config (agent-session-id session))))
    (unless (probe-file path)
      (append-transcript-event
       path
       (list :event "session_start"
             :session_id (agent-session-id session)
             :conversation_id (conversation-id (agent-session-conversation session))
             :provider (agent-session-provider session)
             :model (agent-session-model session))))
    path))

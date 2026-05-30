(in-package #:claw-lisp.providers.sse-parser)

;; --- SSE Event Reader ---
;;
;; Reads Server-Sent Events (SSE) from a stream.
;; Format (W3C SSE spec):
;;   field: value
;;   data: line 1
;;   data: line 2
;;
;; Returns a plist of fields for each event, or NIL on EOF.

(defparameter +known-sse-fields+ '("DATA" "EVENT" "ID" "RETRY")
  "Known SSE field names. Other names are ignored to prevent keyword package pollution.")

(defun field-name->keyword (name)
  "Convert SSE field name to keyword.
   Only known fields are converted; unknown fields are ignored to prevent
   unbounded INTERN into the KEYWORD package."
  (let ((upcased (string-upcase name)))
    (cond
      ((member upcased +known-sse-fields+ :test #'string=)
       (intern upcased "KEYWORD"))
      (t
       ;; Log a warning for unknown fields but don't intern
       (warn "Unknown SSE field: ~A" name)
       nil))))

(defun read-sse-event (stream)
  "Read one complete SSE event from STREAM.
   
   STREAM is typically a dexador response stream.
   
   Returns a plist of the event fields, e.g.:
     (:data \"{...}\" :event \"message_start\" :id \"123\")
   Returns NIL if the stream reaches end-of-file before any data is read."
  (let ((fields nil)
        (current-field nil)
        (current-value ""))
    (labels ((finalize-field ()
               (when current-field
                 (let ((key (field-name->keyword current-field)))
                   (when key
                     (if (eq key :data)
                         ;; For 'data', the SSE spec says multiple data: lines
                         ;; are concatenated with newlines.
                         (let ((existing (getf fields :data)))
                           (if existing
                               (setf (getf fields :data)
                                     (format nil "~A~%~A" existing current-value))
                               (setf (getf fields :data) current-value)))
                         ;; Other fields: first one wins, subsequent are ignored per spec
                         (unless (getf fields key)
                           (setf (getf fields key) current-value)))))
                 (setf current-field nil current-value "")))
             (try-read-line ()
               (handler-case (read-line stream nil nil)
                 (error (c)
                   (warn "SSE stream error: ~A" c)
                   nil)))
             (process-line (line)
               ;; Returns :continue, :event, or :eof
               (cond
                 ;; EOF
                 ((null line)
                  (finalize-field)
                  :eof)
                 ;; Empty line signals end of event
                 ((zerop (length line))
                  (if fields
                      (progn (finalize-field) :event)
                      :continue)) ;; skip leading empty lines
                 ;; Comment lines are ignored (start with :)
                 ((char= (char line 0) #\:)
                  :continue)
                 ;; Field: value line
                 (t
                  (let ((colon-pos (position #\: line)))
                    (when colon-pos
                      (let* ((field-name (string-trim '(#\Space) (subseq line 0 colon-pos)))
                             (raw-start (1+ colon-pos))
                             ;; SSE spec: if value starts with space, remove it
                             (value-start (if (and (< raw-start (length line))
                                                   (char= (char line raw-start) #\Space))
                                              (1+ raw-start)
                                              raw-start))
                             (value (subseq line value-start)))
                        ;; Finalize previous field
                        (finalize-field)
                        (setf current-field field-name
                              current-value value))))
                  :continue))))
      (loop
        (let ((line (try-read-line)))
          (case (process-line line)
            (:eof (return (when fields fields)))
            (:event (return fields))
            (:continue nil)))))))

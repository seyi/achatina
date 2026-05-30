(in-package #:claw-lisp.providers.stream-accumulator)

;; --- Stream Accumulator ---
;;
;; Accumulates Anthropic streaming events into a transport-response.
;;
;; Anthropic streaming events (in order):
;;   message_start → content_block_start → content_block_delta (N times)
;;     → content_block_stop → ... → message_delta → message_stop
;;
;; This accumulator handles text blocks and tool-use blocks.

(defun process-stream-event (acc event)
  "Process one SSE event and update the accumulator ACC.

   EVENT is a plist from the SSE parser, e.g.:
     (:event \"content_block_delta\" :data \"{...}\")

   Returns ACC (modified in place).

   Callback contract:
   The on-event slot (if non-nil) must be a function of two arguments:
     (event-type data)
   where EVENT-TYPE is a string such as \"message_start\", \"content_block_delta\",
   \"content_block_stop\", \"message_delta\", or \"message_stop\", and DATA is the
   plist decoded from the SSE JSON payload.

   Errors raised by the callback are caught and written to *ERROR-OUTPUT* so that
   UI or logging failures cannot abort the core streaming pipeline."
  (let* ((data-str (getf event :data))
         (data (when data-str
                 (handler-case
                     (claw-lisp.providers.http-utils:json-decode data-str)
                   (error () nil)))))
    (when data
      (let ((type (getf data :type)))
        (cond
          ((string= type "message_start")
           (let ((message (getf data :message)))
             (when message
               (setf (claw-lisp.core.domain:stream-accumulator-message-id acc)
                     (getf message :id)
                     (claw-lisp.core.domain:stream-accumulator-model acc)
                     (getf message :model)))))

          ((string= type "content_block_start")
           (let ((block (getf data :content_block)))
             (when block
               (let ((block-type (getf block :type)))
                 (when (string= block-type "tool_use")
                   (setf (claw-lisp.core.domain:stream-accumulator-current-tool-use acc)
                         (list :id (getf block :id)
                               :name (getf block :name)
                               :input-json "")))))))

          ((string= type "content_block_delta")
           (let ((delta (getf data :delta)))
             (when delta
               (let ((delta-type (getf delta :type)))
                 (cond
                   ((string= delta-type "text_delta")
                    (let ((text (getf delta :text)))
                      (when text
                        (setf (claw-lisp.core.domain:stream-accumulator-text acc)
                              (concatenate 'string
                                           (claw-lisp.core.domain:stream-accumulator-text acc)
                                           text)))))
                   ((string= delta-type "input_json_delta")
                    (let ((partial-json (getf delta :partial_json)))
                      (when (and partial-json
                                 (claw-lisp.core.domain:stream-accumulator-current-tool-use acc))
                        (setf (getf (claw-lisp.core.domain:stream-accumulator-current-tool-use acc) :input-json)
                              (concatenate 'string
                                           (getf (claw-lisp.core.domain:stream-accumulator-current-tool-use acc) :input-json)
                                           partial-json))))))))))

          ((string= type "content_block_stop")
           (let ((tool-use (claw-lisp.core.domain:stream-accumulator-current-tool-use acc)))
             (when tool-use
               (let ((input (handler-case
                                (claw-lisp.providers.http-utils:json-decode
                                 (getf tool-use :input-json))
                              (error () nil))))
                 (setf (getf tool-use :input) input)
                 (remf tool-use :input-json)
                 (push tool-use (claw-lisp.core.domain:stream-accumulator-tool-use-blocks acc))
                 (setf (claw-lisp.core.domain:stream-accumulator-current-tool-use acc) nil)))))

          ((string= type "message_delta")
           (let ((delta (getf data :delta)))
             (when delta
               (setf (claw-lisp.core.domain:stream-accumulator-stop-reason acc)
                     (getf delta :stop_reason)
                     (claw-lisp.core.domain:stream-accumulator-stop-sequence acc)
                     (getf delta :stop_sequence))))
           (let ((usage (getf data :usage)))
             (when usage
               (setf (claw-lisp.core.domain:stream-accumulator-usage acc) usage))))

          ((string= type "message_stop")
           (setf (claw-lisp.core.domain:stream-accumulator-done acc) t))))

      ;; Call the on-event callback if provided (with error isolation)
      (let ((callback (claw-lisp.core.domain:stream-accumulator-on-event acc)))
        (when (and callback (functionp callback))
          (handler-case
              (funcall callback (getf data :type) data)
            (error (e)
              (format *error-output*
                      "Warning: on-event callback error: ~A~%"
                      e))))))
    acc))

(defun accumulator->transport-response (acc &key provider)
  "Convert a completed STREAM-ACCUMULATOR into a transport-response.

   Returns a transport-response struct suitable for the runtime's
   response handling. The tool-calls are extracted from accumulated
   tool-use blocks."
  (let ((tool-calls
          (loop for tu in (reverse (claw-lisp.core.domain:stream-accumulator-tool-use-blocks acc))
                collect
                (list :id (getf tu :id)
                      :name (getf tu :name)
                      :input (getf tu :input)))))
    (claw-lisp.core.domain:make-transport-response
     :ok-p t
     :status 200
     :assistant-text (claw-lisp.core.domain:stream-accumulator-text acc)
     :raw-response ""
     :provider (or provider "anthropic")
     :tool-calls tool-calls
     :metadata (list :stop-reason (claw-lisp.core.domain:stream-accumulator-stop-reason acc)
                     :usage (claw-lisp.core.domain:stream-accumulator-usage acc)
                     :message-id (claw-lisp.core.domain:stream-accumulator-message-id acc)))))

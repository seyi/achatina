(in-package #:claw-lisp.tests)

(defun %overwrite-file-bytes (path octets)
  (with-open-file (stream path
                          :direction :output
                          :if-exists :supersede
                          :if-does-not-exist :create
                          :element-type '(unsigned-byte 8))
    (write-sequence octets stream)
    (finish-output stream)))

(defun test-verify-cas-object-integrity-success ()
  (%with-temp-cas-root cas-root
    (let* ((cas-hash (claw-lisp.storage.cas:cas-put cas-root "integrity-ok"))
           (report (claw-lisp.cas.integrity:verify-cas-object-integrity cas-root cas-hash)))
      (%assert (claw-lisp.cas.integrity:integrity-report-ok-p report)
               "Expected clean object integrity report")
      (%assert (= 1 (claw-lisp.cas.integrity:integrity-report-verified-count report))
               "Expected one verified object, got ~S"
               (claw-lisp.cas.integrity:integrity-report-verified-count report))
      (%assert (zerop (claw-lisp.cas.integrity:integrity-report-failure-count report))
               "Expected zero failures, got ~S"
               (claw-lisp.cas.integrity:integrity-report-failure-count report))))
  (format t "~&+ test-verify-cas-object-integrity-success passed~%")
  t)

(defun test-verify-cas-object-integrity-detects-missing-and-corrupt ()
  (%with-temp-cas-root cas-root
    (let* ((missing-hash "sha256:0000000000000000000000000000000000000000000000000000000000000000")
           (missing-report (claw-lisp.cas.integrity:verify-cas-object-integrity
                            cas-root missing-hash)))
      (%assert (= 1 (claw-lisp.cas.integrity:integrity-report-failure-count missing-report))
               "Expected one missing-object failure")
      (%assert (eq :missing-object
                   (claw-lisp.cas.integrity:integrity-failure-kind
                    (first (claw-lisp.cas.integrity:integrity-report-failures missing-report))))
               "Expected missing-object failure kind")
      (let ((signaled nil))
        (handler-case
            (claw-lisp.cas.integrity:verify-cas-object-integrity cas-root missing-hash :error-p t)
          (claw-lisp.cas.integrity:cas-integrity-missing-object-error ()
            (setf signaled t)))
        (%assert signaled "Expected missing object verification to signal")))
    (let* ((cas-hash (claw-lisp.storage.cas:cas-put cas-root "integrity-corrupt"))
           (object-path (claw-lisp.storage.cas:cas-object-path cas-root cas-hash)))
      (%overwrite-file-bytes object-path
                             (make-array 3 :element-type '(unsigned-byte 8)
                                         :initial-contents '(1 2 3)))
      (let ((report (claw-lisp.cas.integrity:verify-cas-object-integrity cas-root cas-hash)))
        (%assert (= 1 (claw-lisp.cas.integrity:integrity-report-failure-count report))
                 "Expected one corrupt-object failure")
        (%assert (eq :corrupt-object
                     (claw-lisp.cas.integrity:integrity-failure-kind
                      (first (claw-lisp.cas.integrity:integrity-report-failures report))))
                 "Expected corrupt-object failure kind"))
      (let ((signaled nil))
        (handler-case
            (claw-lisp.cas.integrity:verify-cas-object-integrity cas-root cas-hash :error-p t)
          (claw-lisp.cas.integrity:cas-integrity-corrupt-object-error ()
            (setf signaled t)))
        (%assert signaled "Expected corrupt object verification to signal"))))
  (format t "~&+ test-verify-cas-object-integrity-detects-missing-and-corrupt passed~%")
  t)

(defun test-verify-cas-ref-integrity ()
  (%with-temp-cas-root cas-root
    (let* ((ref-root (merge-pathnames "refs/" cas-root))
           (cas-hash (claw-lisp.storage.cas:cas-put cas-root "ref-ok")))
      (claw-lisp.storage.cas-ref:write-cas-ref ref-root "session/tool-result" cas-hash)
      (let ((report (claw-lisp.cas.integrity:verify-cas-ref-integrity
                     ref-root cas-root "session/tool-result")))
        (%assert (claw-lisp.cas.integrity:integrity-report-ok-p report)
                 "Expected clean ref integrity report")
        (%assert (= 2 (claw-lisp.cas.integrity:integrity-report-verified-count report))
                 "Expected ref and object to verify, got ~S"
                 (claw-lisp.cas.integrity:integrity-report-verified-count report)))
      (claw-lisp.storage.cas:cas-delete cas-root cas-hash)
      (let ((report (claw-lisp.cas.integrity:verify-cas-ref-integrity
                     ref-root cas-root "session/tool-result")))
        (%assert (= 1 (claw-lisp.cas.integrity:integrity-report-failure-count report))
                 "Expected one dangling-ref failure")
        (%assert (eq :dangling-ref
                     (claw-lisp.cas.integrity:integrity-failure-kind
                      (first (claw-lisp.cas.integrity:integrity-report-failures report))))
                 "Expected dangling-ref failure kind"))
      (let ((signaled nil))
        (handler-case
            (claw-lisp.cas.integrity:verify-cas-ref-integrity
             ref-root cas-root "session/tool-result" :error-p t)
          (claw-lisp.cas.integrity:cas-integrity-dangling-ref-error ()
            (setf signaled t)))
        (%assert signaled "Expected dangling ref verification to signal"))))
  (format t "~&+ test-verify-cas-ref-integrity passed~%")
  t)

(defun test-verify-manifest-graph-integrity-success ()
  (%with-temp-cas-root cas-root
    (let* ((entry-a-hash (claw-lisp.storage.cas:cas-put cas-root "artifact-a"))
           (entry-b-hash (claw-lisp.storage.cas:cas-put cas-root "artifact-b"))
           (manifest (claw-lisp.cas.manifest:make-manifest
                      :entries (list (claw-lisp.cas.manifest:make-manifest-entry
                                      :role :artifact
                                      :cas-hash entry-a-hash
                                      :type :text)
                                     (claw-lisp.cas.manifest:make-manifest-entry
                                      :role :artifact
                                      :cas-hash entry-b-hash
                                      :type :text))
                      :metadata '(:session-id "graph-ok")))
           (manifest-hash (claw-lisp.cas.manifest:store-manifest cas-root manifest))
           (report (claw-lisp.cas.integrity:verify-manifest-graph-integrity
                    cas-root manifest-hash)))
      (%assert (claw-lisp.cas.integrity:integrity-report-ok-p report)
               "Expected clean manifest graph report")
      (%assert (= 3 (claw-lisp.cas.integrity:integrity-report-verified-count report))
               "Expected manifest object plus two entries to verify, got ~S"
               (claw-lisp.cas.integrity:integrity-report-verified-count report))
      (%assert (equal '(:entry-count 2 :verify-signature-p nil)
                      (claw-lisp.cas.integrity:integrity-report-metadata report))
               "Unexpected manifest report metadata: ~S"
               (claw-lisp.cas.integrity:integrity-report-metadata report))))
  (format t "~&+ test-verify-manifest-graph-integrity-success passed~%")
  t)

(defun test-verify-manifest-graph-integrity-detects-broken-entries ()
  (%with-temp-cas-root cas-root
    (let* ((entry-a-hash (claw-lisp.storage.cas:cas-put cas-root "artifact-a"))
           (entry-b-hash (claw-lisp.storage.cas:cas-put cas-root "artifact-b"))
           (manifest (claw-lisp.cas.manifest:make-manifest
                      :entries (list (claw-lisp.cas.manifest:make-manifest-entry
                                      :role :artifact
                                      :cas-hash entry-a-hash
                                      :type :text)
                                     (claw-lisp.cas.manifest:make-manifest-entry
                                      :role :artifact
                                      :cas-hash entry-b-hash
                                      :type :text))
                      :metadata '(:session-id "graph-broken")))
           (manifest-hash (claw-lisp.cas.manifest:store-manifest cas-root manifest)))
      (claw-lisp.storage.cas:cas-delete cas-root entry-b-hash)
      (let ((report (claw-lisp.cas.integrity:verify-manifest-graph-integrity
                     cas-root manifest-hash)))
        (%assert (= 1 (claw-lisp.cas.integrity:integrity-report-failure-count report))
                 "Expected one broken entry failure")
        (%assert (eq :missing-object
                     (claw-lisp.cas.integrity:integrity-failure-kind
                      (first (claw-lisp.cas.integrity:integrity-report-failures report))))
                 "Expected missing-object failure for deleted manifest entry"))
      (let ((signaled nil))
        (handler-case
            (claw-lisp.cas.integrity:verify-manifest-graph-integrity
             cas-root manifest-hash :error-p t)
          (claw-lisp.cas.integrity:cas-integrity-missing-object-error ()
            (setf signaled t)))
        (%assert signaled "Expected broken manifest entry verification to signal"))))
  (format t "~&+ test-verify-manifest-graph-integrity-detects-broken-entries passed~%")
  t)

(defun test-verify-manifest-graph-integrity-detects-corrupt-manifest-object ()
  (%with-temp-cas-root cas-root
    (let* ((entry-hash (claw-lisp.storage.cas:cas-put cas-root "artifact-a"))
           (manifest (claw-lisp.cas.manifest:make-manifest
                      :entries (list (claw-lisp.cas.manifest:make-manifest-entry
                                      :role :artifact
                                      :cas-hash entry-hash
                                      :type :text))
                      :metadata '(:session-id "graph-corrupt")))
           (manifest-hash (claw-lisp.cas.manifest:store-manifest cas-root manifest))
           (manifest-path (claw-lisp.storage.cas:cas-object-path cas-root manifest-hash)))
      (%overwrite-file-bytes manifest-path
                             (make-array 4 :element-type '(unsigned-byte 8)
                                         :initial-contents '(9 8 7 6)))
      (let ((report (claw-lisp.cas.integrity:verify-manifest-graph-integrity
                     cas-root manifest-hash)))
        (%assert (= 1 (claw-lisp.cas.integrity:integrity-report-failure-count report))
                 "Expected one corrupt manifest failure")
        (%assert (eq :corrupt-object
                     (claw-lisp.cas.integrity:integrity-failure-kind
                      (first (claw-lisp.cas.integrity:integrity-report-failures report))))
                 "Expected corrupt-object failure for tampered manifest object"))))
  (format t "~&+ test-verify-manifest-graph-integrity-detects-corrupt-manifest-object passed~%")
  t)

(defun run-cas-integrity-tests ()
  (format t "~&=== Phase 10 CAS Integrity Tests ===~%")
  (test-verify-cas-object-integrity-success)
  (test-verify-cas-object-integrity-detects-missing-and-corrupt)
  (test-verify-cas-ref-integrity)
  (test-verify-manifest-graph-integrity-success)
  (test-verify-manifest-graph-integrity-detects-broken-entries)
  (test-verify-manifest-graph-integrity-detects-corrupt-manifest-object)
  (format t "~&=== All CAS integrity tests passed ===~%"))

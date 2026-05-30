(in-package #:claw-lisp.tests)

;;; ============================================================
;;; Test Isolation Helper
;;; ============================================================

(defmacro %with-temp-manifest-cas-root (var &body body)
  `(let ((,var (merge-pathnames
                (format nil "manifest-test-~D-~D/"
                        (sb-posix:getpid)
                        (get-internal-real-time))
                #P"/tmp/")))
     (unwind-protect
         (progn
           (ensure-directories-exist
            (uiop:ensure-directory-pathname ,var))
           ,@body)
       (when (probe-file ,var)
         (uiop:delete-directory-tree
          (uiop:ensure-directory-pathname ,var) :validate t)))))

;;; ============================================================
;;; Manifest Tests
;;; ============================================================

(defun test-manifest-roundtrip ()
  (%with-temp-manifest-cas-root cas-root
    (let* ((entry1 (claw-lisp.cas.manifest:make-manifest-entry
                    :role :session-memory
                    :cas-hash "sha256:0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef"
                    :type :markdown))
           (entry2 (claw-lisp.cas.manifest:make-manifest-entry
                    :role :compaction-ir
                    :cas-hash "sha256:fedcba9876543210fedcba9876543210fedcba9876543210fedcba9876543210"
                    :type :sexp))
           (metadata '(:session-id "test-session" :timestamp 1234567890))
           (manifest (claw-lisp.cas.manifest:make-manifest
                      :entries (list entry1 entry2)
                      :metadata metadata))
           (serialized (claw-lisp.cas.manifest:serialize-manifest manifest))
           (deserialized (claw-lisp.cas.manifest:deserialize-manifest serialized)))
      (%assert (claw-lisp.cas.manifest:verify-manifest-integrity deserialized)
               "Deserialized manifest should pass integrity check")
      (%assert (equal (claw-lisp.cas.manifest:manifest-metadata manifest)
                      (claw-lisp.cas.manifest:manifest-metadata deserialized))
               "Metadata should match after roundtrip")
      (%assert (= (length (claw-lisp.cas.manifest:manifest-entries manifest))
                  (length (claw-lisp.cas.manifest:manifest-entries deserialized)))
               "Entry count should match")))
  (format t "~&+ test-manifest-roundtrip passed~%")
  t)

(defun test-manifest-tamper-detection ()
  (let* ((entry (claw-lisp.cas.manifest:make-manifest-entry
                 :role :tool-result
                 :cas-hash "sha256:0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef"
                 :type :json))
         (manifest (claw-lisp.cas.manifest:make-manifest
                    :entries (list entry)
                    :metadata '(:test t))))
    (%assert (claw-lisp.cas.manifest:verify-manifest-integrity manifest)
             "Initial manifest should be valid")
    
    ;; Tamper with metadata
    (setf (claw-lisp.cas.manifest:manifest-metadata manifest) '(:test nil))
    (%assert (not (claw-lisp.cas.manifest:verify-manifest-integrity manifest))
             "Manifest should be invalid after metadata tampering")
    
    ;; Reset and tamper with entries
    (setf (claw-lisp.cas.manifest:manifest-metadata manifest) '(:test t))
    (setf (claw-lisp.cas.manifest:manifest-entry-cas-hash (car (claw-lisp.cas.manifest:manifest-entries manifest)))
          "sha256:ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff")
    (%assert (not (claw-lisp.cas.manifest:verify-manifest-integrity manifest))
             "Manifest should be invalid after entry tampering"))
  (format t "~&+ test-manifest-tamper-detection passed~%")
  t)

(defun test-manifest-signature ()
  (let* ((entry (claw-lisp.cas.manifest:make-manifest-entry
                 :role :hitl-decision
                 :cas-hash "sha256:0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef"
                 :type :sexp))
         (manifest (claw-lisp.cas.manifest:make-manifest :entries (list entry) :metadata nil)))
    
    ;; No signature by default
    (%assert (null (claw-lisp.cas.manifest:manifest-signature manifest))
             "Default manifest should have no signature")
    (%assert (claw-lisp.cas.crypto:verify-manifest-root-signature
              (claw-lisp.cas.manifest:manifest-root-digest manifest)
              nil)
             "Empty signature should verify when key is NIL")

    ;; Sign with key
    (let ((claw-lisp.cas.crypto:*manifest-signing-key* "secret"))
      (let* ((sig (claw-lisp.cas.crypto:sign-manifest-root
                   (claw-lisp.cas.manifest:manifest-root-digest manifest)))
             (signed-manifest (claw-lisp.cas.manifest:make-manifest
                               :entries (list entry)
                               :metadata nil
                               :signature sig)))
        (%assert (string= sig (claw-lisp.cas.manifest:manifest-signature signed-manifest))
                 "Signature should be stored in manifest")
        (%assert (claw-lisp.cas.crypto:verify-manifest-root-signature
                  (claw-lisp.cas.manifest:manifest-root-digest signed-manifest)
                  sig)
                 "Valid signature should verify")
        (%assert (not (claw-lisp.cas.crypto:verify-manifest-root-signature
                       (claw-lisp.cas.manifest:manifest-root-digest signed-manifest)
                       "wrong-sig"))
                 "Invalid signature should not verify"))))
  (format t "~&+ test-manifest-signature passed~%")
  t)

(defun test-manifest-store-load ()
  (%with-temp-manifest-cas-root cas-root
    (let* ((entry (claw-lisp.cas.manifest:make-manifest-entry
                   :role :artifact
                   :cas-hash (claw-lisp.storage.cas:cas-put cas-root "artifact-content")
                   :type :text))
           (manifest (claw-lisp.cas.manifest:make-manifest :entries (list entry) :metadata nil))
           (manifest-hash (claw-lisp.cas.manifest:store-manifest cas-root manifest))
           (loaded (claw-lisp.cas.manifest:load-manifest cas-root manifest-hash)))
      (%assert loaded "Manifest should be loadable from CAS")
      (%assert (claw-lisp.cas.manifest:verify-manifest-integrity loaded)
               "Loaded manifest should pass integrity check")
      (%assert (string= (claw-lisp.cas.manifest:manifest-root-digest manifest)
                        (claw-lisp.cas.manifest:manifest-root-digest loaded))
               "Root digest should match after store/load")))
  (format t "~&+ test-manifest-store-load passed~%")
  t)

(defun test-manifest-security-verification ()
  (%with-temp-manifest-cas-root cas-root
    (let* ((entry (claw-lisp.cas.manifest:make-manifest-entry
                   :role :security-test
                   :cas-hash (claw-lisp.storage.cas:cas-put cas-root "secure")
                   :type :text))
           (manifest (claw-lisp.cas.manifest:make-manifest :entries (list entry) :metadata nil))
           (serialized (claw-lisp.cas.manifest:serialize-manifest manifest))
           ;; Tamper with the serialized text directly (change a hash char)
           (tampered (substitute #\f #\1 serialized))
           (manifest-hash (claw-lisp.storage.cas:cas-put cas-root tampered))
           (integrity-signaled nil)
           (signature-signaled nil))
      
      ;; 1. Integrity failure on load
      (handler-case
          (claw-lisp.cas.manifest:load-manifest cas-root manifest-hash)
        (claw-lisp.cas.manifest:cas-manifest-integrity-error ()
          (setf integrity-signaled t)))
      (%assert integrity-signaled "Expected integrity error on tampered load")

      ;; 2. Signature failure on load
      (let ((claw-lisp.cas.crypto:*manifest-signing-key* "secret"))
        (let* ((sig (claw-lisp.cas.crypto:sign-manifest-root
                     (claw-lisp.cas.manifest:manifest-root-digest manifest)))
               (signed-manifest (claw-lisp.cas.manifest:make-manifest
                                 :entries (list entry)
                                 :metadata nil
                                 :signature sig))
               (signed-hash (claw-lisp.cas.manifest:store-manifest cas-root signed-manifest)))
          
          ;; Load with wrong key should signal signature error
          (let ((claw-lisp.cas.crypto:*manifest-signing-key* "wrong-secret"))
            (handler-case
                (claw-lisp.cas.manifest:load-manifest cas-root signed-hash :verify-signature-p t)
              (claw-lisp.cas.manifest:cas-manifest-signature-error ()
                (setf signature-signaled t))))
          (%assert signature-signaled "Expected signature error with wrong key")))))
  (format t "~&+ test-manifest-security-verification passed~%")
  t)

(defun test-manifest-malformed-input-rejected ()
  (dolist (text '("not-json"
                  "[]"
                  "{\"entries\":[],\"metadata\":{},\"signature\":null}"
                  "{\"root-digest\":123,\"entries\":[],\"metadata\":{},\"signature\":null}"
                  "{\"root-digest\":\"sha256:0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef\",\"entries\":[{\"role\":\"artifact\",\"cas-hash\":\"not-a-hash\",\"type\":\"text\"}],\"metadata\":{},\"signature\":null}"))
    (let ((signaled nil))
      (handler-case
          (claw-lisp.cas.manifest:deserialize-manifest text)
        (claw-lisp.cas.manifest:cas-manifest-parse-error ()
          (setf signaled t)))
      (%assert signaled
               "Expected malformed manifest input to signal parse error: ~S" text)))
  (format t "~&+ test-manifest-malformed-input-rejected passed~%")
  t)

(defun test-manifest-determinism ()
  "Verify that manifest digest and serialization are stable regardless of input metadata order."
  (let* ((entry (claw-lisp.cas.manifest:make-manifest-entry
                 :role :test
                 :cas-hash "sha256:0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef"
                 :type :text
                 :metadata '(:z 1 :a 2)))
         ;; manifest-a: metadata ordered (:a 1 :z 2)
         (manifest-a (claw-lisp.cas.manifest:make-manifest
                      :entries (list entry)
                      :metadata '(:a 1 :z 2)))
         ;; manifest-b: metadata ordered (:z 2 :a 1)
         (manifest-b (claw-lisp.cas.manifest:make-manifest
                      :entries (list entry)
                      :metadata '(:z 2 :a 1))))
    (%assert (string= (claw-lisp.cas.manifest:manifest-root-digest manifest-a)
                      (claw-lisp.cas.manifest:manifest-root-digest manifest-b))
             "Manifests with different metadata order should have identical root digests")
    (%assert (string= (claw-lisp.cas.manifest:serialize-manifest manifest-a)
                      (claw-lisp.cas.manifest:serialize-manifest manifest-b))
             "Manifests with different metadata order should produce identical serialized strings"))
  (format t "~&+ test-manifest-determinism passed~%")
  t)

;;; ============================================================
;;; Test Runner
;;; ============================================================

(defun run-cas-manifest-tests ()
  (format t "~&=== Phase 10 CAS Manifest Tests ===~%")
  (test-manifest-roundtrip)
  (test-manifest-tamper-detection)
  (test-manifest-signature)
  (test-manifest-store-load)
  (test-manifest-security-verification)
  (test-manifest-malformed-input-rejected)
  (test-manifest-determinism)
  (format t "~&=== All CAS manifest tests passed ===~%"))

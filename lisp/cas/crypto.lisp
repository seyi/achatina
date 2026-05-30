(defpackage #:claw-lisp.cas.crypto
  (:use #:cl)
  (:export
   #:sign-manifest-root
   #:verify-manifest-root-signature
   #:*manifest-signing-key*))

(in-package #:claw-lisp.cas.crypto)

(defparameter *manifest-signing-key* nil
  "Opaque key material for manifest signing. When NIL, signing is disabled.")

;; WARNING: non-cryptographic Phase 10 placeholder.
;; This is not suitable for security or trust decisions.
;; Phase 11 must replace this with a real signing primitive.
(defun sign-manifest-root (root-digest)
  "Return a signature blob for ROOT-DIGEST, or NIL if signing disabled.
WARNING: this is a non-cryptographic placeholder and must not be used
for security decisions."
  (when *manifest-signing-key*
    (concatenate 'string "sig:" (princ-to-string *manifest-signing-key*) ":" root-digest)))

(defun verify-manifest-root-signature (root-digest signature)
  "Return T if SIGNATURE is valid for ROOT-DIGEST."
  (if (null *manifest-signing-key*)
      (null signature)
      (string= signature (sign-manifest-root root-digest))))

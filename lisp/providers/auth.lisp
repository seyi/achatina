(in-package #:claw-lisp.providers.auth)

;;; ============================================================
;;; Step 5: Consolidated credential validation
;;; ============================================================

(defun credentials-configured-p (creds)
  "Return T if CREDS has sufficient information for API calls."
  (typecase creds
    (null nil)
    (claw-lisp.config:provider-credentials
     (let ((key (claw-lisp.config:provider-credentials-api-key creds)))
       (and key (plusp (length key)))))
    (claw-lisp.config:bedrock-credentials
     (let ((ak (claw-lisp.config:bedrock-credentials-access-key creds))
           (sk (claw-lisp.config:bedrock-credentials-secret-key creds)))
       (and ak (plusp (length ak))
            sk (plusp (length sk)))))
    (t nil)))

(defun get-missing-config (provider-name creds)
  "Return list of missing configuration items for provider."
  (typecase creds
    (null
     (list (format nil "No credentials configured for ~A" provider-name)))
    (claw-lisp.config:bedrock-credentials
     (let ((missing nil))
       (unless (and (claw-lisp.config:bedrock-credentials-access-key creds)
                    (plusp (length (claw-lisp.config:bedrock-credentials-access-key creds))))
         (push "AWS_ACCESS_KEY_ID" missing))
       (unless (and (claw-lisp.config:bedrock-credentials-secret-key creds)
                    (plusp (length (claw-lisp.config:bedrock-credentials-secret-key creds))))
         (push "AWS_SECRET_ACCESS_KEY" missing))
       missing))
    (claw-lisp.config:provider-credentials
     (unless (and (claw-lisp.config:provider-credentials-api-key creds)
                  (plusp (length (claw-lisp.config:provider-credentials-api-key creds))))
       (list (format nil "API key for ~A" provider-name))))
    (t (list (format nil "Invalid credentials type for ~A: ~A"
                     provider-name (type-of creds))))))

(in-package #:claw-lisp.providers.http-json)

;; Shared JSON serialization helpers used by both http-utils and transcripts.
;; Lives in its own file so transcripts.lisp can depend on it without pulling
;; in the full http-utils (which depends on dexador).

(defun value->json-safe (v)
  "Convert a Lisp value to a JSON-safe form.
   Keywords become strings. Plists become hash tables.
   Lists become vectors. Strings, numbers, booleans pass through."
  (cond
    ((null v) nil)
    ((stringp v) v)
    ((numberp v) v)
    ((eq v t) t)
    ((keywordp v) (string-downcase (symbol-name v)))
    ((vectorp v)
     ;; Vectors become JSON arrays
     (map 'vector #'value->json-safe v))
    ((and (listp v) (evenp (length v)) (keywordp (first v)))
     ;; Treat as plist → hash table
     (plist-to-json-object v))
    ((listp v)
     ;; Treat as array
     (map 'vector #'value->json-safe (coerce v 'vector)))
    (t (princ-to-string v))))

(defun plist-to-json-object (plist)
  "Convert a plist (possibly nested) to a hash table for yason JSON object encoding."
  (let ((ht (make-hash-table :test 'equal)))
    (loop for (k v) on plist by #'cddr
          do (setf (gethash (string-downcase (symbol-name k)) ht)
                   (value->json-safe v)))
    ht))

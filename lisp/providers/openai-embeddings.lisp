;;;; lisp/providers/openai-embeddings.lisp
;;;;
;;;; OpenAI Embedding Provider (Stub Implementation)
;;;;
;;;; Deterministic pseudo-random embeddings for testing.

(in-package :claw-lisp.providers)

;;; ============================================================
;;; OpenAI Embedding Provider (Stub Implementation)
;;; ============================================================

(defparameter *openai-default-embedding-dimension* 128
  "Default embedding dimension used by the OpenAI stub provider when
RUNTIME-CONFIG-EMBEDDING-DEFAULT-DIMENSION is NIL.")

(defun openai-supported-embedding-models ()
  "Return a list of supported embedding model names for the OpenAI provider.

This is a stub implementation; in a real provider this would likely be
a static list or fetched from the API."
  '("text-embedding-3-small"
    "text-embedding-3-large"))

(defun %openai-stub-embedding-dimension ()
  "Return the embedding dimension to use for the stub implementation."
(or (claw-lisp.config:runtime-config-embedding-default-dimension
     claw-lisp.config:*runtime-config*)
      *openai-default-embedding-dimension*))

(defun %hash-string-to-floats (text dimension)
  "Deterministically map TEXT to a list of DIMENSION single-float values.

This is used by the stub provider to generate stable pseudo-random
embeddings without external dependencies."
  (let ((result (make-array dimension :element-type 'single-float)))
    (loop
      with len = (length text)
      for i from 0 below dimension
      for ch = (if (> len 0) (char-code (char text (mod i len))) 0)
      for value = (/ (mod (+ ch (* i 31)) 1000.0) 1000.0)
      do (setf (aref result i) (coerce value 'single-float)))
    (coerce result 'list)))

(defun openai-compute-embeddings (texts &key model timeout-seconds)
  "Stub OpenAI embedding implementation.

INPUTS:
  - texts: list of strings.
  - model: string model name (ignored by stub except for validation).
  - timeout-seconds: integer timeout hint (ignored by stub).

OUTPUT:
  - list of embeddings, one per TEXT, where each embedding is a list of
    single-float values of fixed dimension.

BEHAVIOR:
  - Deterministically maps each TEXT to a pseudo-random embedding vector
    based on its content, so repeated calls with the same TEXT and MODEL
    produce the same result.

ERRORS:
  - Signals EMBEDDING-ERROR if MODEL is not in OPENAI-SUPPORTED-EMBEDDING-MODELS."
  (declare (ignore timeout-seconds))
  (let ((model (or model (claw-lisp.config:runtime-config-embedding-model
                          claw-lisp.config:*runtime-config*))))
    (unless (member model (openai-supported-embedding-models) :test #'string=)
      (error 'embedding-error
             :provider :openai
             :model model
             :reason (format nil "Unsupported OpenAI embedding model: ~S" model)
             :underlying-condition nil))
    (let ((dimension (%openai-stub-embedding-dimension)))
      (mapcar (lambda (text)
                (%hash-string-to-floats (or text "") dimension))
              texts))))

;; Register provider at load time
(register-embedding-provider
 :openai
 :compute-fn #'openai-compute-embeddings
 :models-fn #'openai-supported-embedding-models)

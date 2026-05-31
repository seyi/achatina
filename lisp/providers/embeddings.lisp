;;;; lisp/providers/embeddings.lisp
;;;;
;;;; Embedding Provider Protocol and Public API
;;;;
;;;; Provider-neutral embedding API for Phase 7 Semantic Retrieval.

(in-package :claw-lisp.providers)

;;; ============================================================
;;; Embedding Provider Protocol and Public API
;;; ============================================================

(defparameter *embedding-providers* (make-hash-table :test 'eq)
  "Registry mapping provider keyword → embedding provider implementation.

Each value is a plist with at least:
  :compute-fn  - function of (texts &key model timeout-seconds)
  :models-fn   - function of () returning list of supported model names.")

;;; ------------------------------------------------------------
;;; Embedding Error Condition
;;; ------------------------------------------------------------

(define-condition embedding-error (error)
  ((provider :initarg :provider :reader embedding-error-provider)
   (model :initarg :model :reader embedding-error-model)
   (reason :initarg :reason :reader embedding-error-reason)
   (underlying-condition :initarg :underlying-condition
                         :reader embedding-error-underlying-condition))
  (:report (lambda (condition stream)
             (format stream "Embedding error (provider=~S model=~S): ~A"
                     (embedding-error-provider condition)
                     (embedding-error-model condition)
                     (embedding-error-reason condition)))))

;;; ------------------------------------------------------------
;;; Provider Registration and Resolution
;;; ------------------------------------------------------------

(defun register-embedding-provider (provider-keyword &key compute-fn models-fn)
  "Register an embedding provider implementation.

PROVIDER-KEYWORD is a keyword identifying the provider (e.g., :openai).
COMPUTE-FN is a function of (texts &key model timeout-seconds) that returns
a list of embedding vectors (each a list of single-float).
MODELS-FN is a function of no arguments that returns a list of supported
model names (strings), or NIL if unknown."
  (setf (gethash provider-keyword *embedding-providers*)
        (list :compute-fn compute-fn
              :models-fn models-fn))
  provider-keyword)

(defun resolve-embedding-provider (provider-keyword)
  "Return the provider implementation plist for PROVIDER-KEYWORD.

The returned plist contains at least:
  :compute-fn  - function of (texts &key model timeout-seconds)
  :models-fn   - function of () → list of model names.

Signals EMBEDDING-ERROR if the provider is unknown."
  (or (gethash provider-keyword *embedding-providers*)
      (error 'embedding-error
             :provider provider-keyword
             :model nil
             :reason (format nil "Unknown embedding provider: ~S" provider-keyword)
             :underlying-condition nil)))

;;; ------------------------------------------------------------
;;; Provider Dispatch
;;; ------------------------------------------------------------

(defun provider-compute-embeddings (provider-keyword texts &key model timeout-seconds)
  "Low-level provider dispatch for embedding computation.

INPUTS:
  - provider-keyword: keyword identifying the provider (e.g., :openai).
  - texts: list of strings to embed.
  - model: string model name; may be NIL to use provider default.
  - timeout-seconds: integer timeout hint; provider may ignore.

OUTPUT:
  - list of embeddings, one per TEXT, where each embedding is a list of
    single-float values.

ERRORS:
  - Signals EMBEDDING-ERROR if the provider is unknown or fails."
  (let* ((provider (resolve-embedding-provider provider-keyword))
         (compute-fn (getf provider :compute-fn)))
    (unless compute-fn
      (error 'embedding-error
             :provider provider-keyword
             :model model
             :reason "Provider has no :compute-fn registered"
             :underlying-condition nil))
    (handler-case
        (funcall compute-fn texts :model model :timeout-seconds timeout-seconds)
      (embedding-error (e)
        (signal e))
      (error (e)
        (error 'embedding-error
               :provider provider-keyword
               :model model
               :reason "Unhandled error in provider-compute-embeddings"
               :underlying-condition e)))))

;;; ------------------------------------------------------------
;;; Public API: compute-embeddings
;;; ------------------------------------------------------------

(defun compute-embeddings (texts &key provider model batch-size timeout-seconds
                                  (signal-errors-p t))
  "Compute embeddings for a list of TEXTS using the configured provider.

INPUTS:
  - texts: list of strings.
  - provider: keyword identifying the provider (e.g., :openai). When NIL,
    uses (claw-lisp.config:current-embedding-provider).
  - model: string model name. When NIL, uses (claw-lisp.config:current-embedding-model).
  - batch-size: maximum number of texts per provider call. When NIL,
    uses (claw-lisp.config:current-embedding-max-batch-size).
  - timeout-seconds: integer timeout hint per provider call. When NIL,
    uses (claw-lisp.config:runtime-config-embedding-timeout-seconds
          claw-lisp.config:*runtime-config*).
  - signal-errors-p: when T (default), embedding errors are signaled as
    EMBEDDING-ERROR. When NIL, errors are logged (if enabled) and this
    function returns NIL.

OUTPUT:
  - list of embeddings (one per input text), where each embedding is a
    list of single-float values; or NIL if an error occurred and
    SIGNAL-ERRORS-P is NIL.

ERRORS:
  - EMBEDDING-ERROR when provider resolution or provider call fails and
    SIGNAL-ERRORS-P is T."
  (let* ((texts (or texts '()))
         (provider (or provider (claw-lisp.config:current-embedding-provider)))
         (model (or model (claw-lisp.config:current-embedding-model)))
         (batch-size (or batch-size (claw-lisp.config:current-embedding-max-batch-size)))
         (timeout-seconds (or timeout-seconds
                              (claw-lisp.config:runtime-config-embedding-timeout-seconds
                               claw-lisp.config:*runtime-config*))))
    (cond
      ((null texts)
       '())
      ((not (claw-lisp.config:current-embedding-enabled-p))
       (if signal-errors-p
           (error 'embedding-error
                  :provider provider
                  :model model
                  :reason "Embeddings are disabled by configuration"
                  :underlying-condition nil)
           nil))
      (t
       (handler-case
           (compute-embeddings-batched texts
                                       :provider provider
                                       :model model
                                       :batch-size batch-size
                                       :timeout-seconds timeout-seconds)
         (embedding-error (e)
           (when (claw-lisp.config:runtime-config-embedding-log-errors-p
                  claw-lisp.config:*runtime-config*)
             (log-embedding-error e))
           (if signal-errors-p
               (signal e)
               nil)))))))

;;; ------------------------------------------------------------
;;; Batching Utility
;;; ------------------------------------------------------------

(defun compute-embeddings-batched (texts &key provider model batch-size timeout-seconds)
  "Compute embeddings for TEXTS in batches.

Splits TEXTS into chunks of size at most BATCH-SIZE and calls
PROVIDER-COMPUTE-EMBEDDINGS for each chunk, concatenating the results.

INPUTS:
  - texts: list of strings.
  - provider: keyword provider id (required).
  - model: string model name (required).
  - batch-size: positive integer; when NIL or <= 0, no batching is applied.
  - timeout-seconds: integer timeout hint per batch.

OUTPUT:
  - list of embeddings, one per TEXT, in the same order.

ERRORS:
  - EMBEDDING-ERROR if any provider call fails."
  (let ((batch-size (or batch-size (length texts))))
    (labels ((chunk (lst n)
               (when lst
                 (cons (subseq lst 0 (min n (length lst)))
                       (chunk (nthcdr n lst) n)))))
      (let ((chunks (chunk texts batch-size))
            (results '()))
        (dolist (chunk-texts chunks (nreverse results))
          (let ((chunk-embeddings
                  (provider-compute-embeddings provider chunk-texts
                                               :model model
                                               :timeout-seconds timeout-seconds)))
            ;; Basic sanity check: provider must return same length
            (unless (= (length chunk-texts) (length chunk-embeddings))
              (error 'embedding-error
                     :provider provider
                     :model model
                     :reason "Provider returned mismatched number of embeddings"
                     :underlying-condition nil))
            (setf results (nconc (nreverse chunk-embeddings) results))))))))

;;; ------------------------------------------------------------
;;; Logging Utility
;;; ------------------------------------------------------------

(defun log-embedding-error (condition)
  "Log an EMBEDDING-ERROR CONDITION using the standard logging facility.

This is a thin wrapper so logging behavior can be customized in one place."
  ;; Replace with your logging macro/function, e.g. (log-error ...)
  (format *error-output* "~&[EMBEDDING-ERROR] ~A~%" condition))

;;; ------------------------------------------------------------
;;; Supported Models Query
;;; ------------------------------------------------------------

(defun embedding-provider-supported-models (&optional provider-keyword)
  "Return the list of supported embedding models for PROVIDER-KEYWORD.

When PROVIDER-KEYWORD is NIL, uses (claw-lisp.config:current-embedding-provider).
Returns NIL if the provider does not expose model information."
  (let* ((provider-keyword (or provider-keyword
                               (claw-lisp.config:current-embedding-provider)))
         (provider (resolve-embedding-provider provider-keyword))
         (models-fn (getf provider :models-fn)))
    (when models-fn
      (funcall models-fn)))

;;; ------------------------------------------------------------
;;; Embedding Result Struct (for future use)
;;; ------------------------------------------------------------

(defstruct embedding-result
  "Result of an embedding computation for a single text.

FIELDS:
  TEXT       - original input string.
  EMBEDDING  - list of single-float values representing the embedding vector.
  MODEL      - string model name used to compute the embedding.
  PROVIDER   - keyword identifying the provider.
  METADATA   - optional plist with provider-specific metadata (e.g., token usage)."
  (text "" :type string)
  (embedding nil :type (or null list))
  (model "" :type string)
  (provider :unknown :type keyword)
  (metadata nil :type list))
)

(in-package #:claw-lisp.core.model-registry)

;; --- Model Registry ---

(defstruct model-registry
  "Per-model capability registry with alias and fallback resolution.
   MODELS: hash table of model-id → model-capabilities
   ALIASES: hash table of alias → canonical model-id
   PROVIDER-DEFAULTS: hash table of provider → fallback model-capabilities"
  (models (make-hash-table :test 'equal))
  (aliases (make-hash-table :test 'equal))
  (provider-defaults (make-hash-table :test 'equal)))

(defun register-model (registry capabilities)
  "Register MODEL-CAPABILITIES in REGISTRY under its :name."
  (setf (gethash (model-capabilities-name capabilities)
                 (model-registry-models registry))
        capabilities)
  registry)

(defun register-alias (registry alias model-id)
  "Map ALIAS string to canonical MODEL-ID in REGISTRY."
  (setf (gethash alias (model-registry-aliases registry))
        model-id)
  registry)

(defun register-provider-default (registry provider capabilities)
  "Set CAPABILITIES as the fallback for unknown models on PROVIDER."
  (setf (gethash provider (model-registry-provider-defaults registry))
        capabilities)
  registry)

(defun resolve-model (registry model-id)
  "Resolve MODEL-ID to a model-capabilities struct.
   Resolution chain:
   1. Exact match in models table
   2. Alias lookup then exact match
   3. Prefix match against registered model names
   4. Provider default (extracted from model-id prefix)
   5. Minimal defaults with inferred provider"
  ;; 1. Exact match
  (let ((caps (gethash model-id (model-registry-models registry))))
    (when caps (return-from resolve-model caps)))
  ;; 2. Alias lookup
  (let ((canonical (gethash model-id (model-registry-aliases registry))))
    (when canonical
      (let ((caps (gethash canonical (model-registry-models registry))))
        (when caps (return-from resolve-model caps)))))
  ;; 3. Prefix match
  (let ((caps (find-by-prefix registry model-id)))
    (when caps (return-from resolve-model caps)))
  ;; 4. Provider default
  (let* ((provider (infer-provider model-id))
         (caps (gethash provider (model-registry-provider-defaults registry))))
    (when caps (return-from resolve-model caps)))
  ;; 5. No match — return minimal defaults
  (make-model-capabilities :name model-id :provider (infer-provider model-id)))

(defun find-by-prefix (registry model-id)
  "Find the registered model whose name is the longest prefix of MODEL-ID.
   Returns NIL if no registered name is a prefix of MODEL-ID."
  (let ((models (model-registry-models registry))
        (best nil)
        (best-len 0))
    (loop for key being the hash-keys of models
          using (hash-value caps)
          for pos = (search key model-id)
          ;; Only match when key is a true prefix (starts at position 0)
          when (and pos (zerop pos) (> (length key) best-len))
          do (setf best caps best-len (length key)))
    best))

(defun infer-provider (model-id)
  "Infer the provider from MODEL-ID heuristics."
  (cond
    ((search "bedrock" model-id :test #'string=) :bedrock)
    ((search "vertex" model-id :test #'string=) :vertex)
    ((search "/" model-id) :openrouter)  ;; prefixed like anthropic/claude-sonnet
    (t :anthropic)))

(defun model-supports-p (registry model-id feature)
  "Check if MODEL-ID supports FEATURE via REGISTRY.
   FEATURE is one of :tools, :streaming, :thinking, :adaptive-thinking,
   :json-output, :vision, :prompt-caching."
  (let ((caps (resolve-model registry model-id)))
    (ecase feature
      (:tools (model-capabilities-tools-p caps))
      (:streaming (model-capabilities-streaming-p caps))
      (:thinking (model-capabilities-thinking-p caps))
      (:adaptive-thinking (model-capabilities-adaptive-thinking-p caps))
      (:json-output (model-capabilities-json-output-p caps))
      (:vision (model-capabilities-vision-p caps))
      (:prompt-caching (model-capabilities-prompt-caching-p caps)))))

(defun model-context-window (registry model-id)
  "Return the context window size for MODEL-ID via REGISTRY."
  (model-capabilities-context-window (resolve-model registry model-id)))

(defun model-max-output-tokens (registry model-id)
  "Return the max output tokens for MODEL-ID via REGISTRY."
  (model-capabilities-max-output-tokens (resolve-model registry model-id)))

(defun model-translate-name (registry provider model-id)
  "Translate MODEL-ID for the given PROVIDER.
   Anthropic: bare name
   OpenRouter: anthropic/<name>
   Bedrock: us.anthropic.<name>-v1 (heuristic)
   Other providers: pass through unchanged."
  (let* ((caps (resolve-model registry model-id))
         (bare (model-capabilities-name caps)))
    (case provider
      (:anthropic bare)
      (:openrouter (format nil "anthropic/~A" bare))
      (:bedrock (format nil "us.anthropic.~A-v1" bare))
      (:vertex bare)
      (otherwise model-id))))

;; --- Built-in Capabilities ---

(defun register-known-models (registry)
  "Register known Anthropic and common model capabilities in REGISTRY."
  ;; Claude Sonnet 4.6
  (register-model registry
    (make-model-capabilities
     :name "claude-sonnet-4-6" :provider :anthropic
     :context-window 200000 :max-output-tokens 16384
     :tools-p t :streaming-p t :thinking-p t
     :adaptive-thinking-p t :json-output-p t
     :vision-p t :prompt-caching-p t
     :input-price-per-mtok 3.0 :output-price-per-mtok 15.0
     :cache-read-price-per-mtok 0.3 :cache-write-price-per-mtok 3.75))
  ;; Claude Opus 4.6
  (register-model registry
    (make-model-capabilities
     :name "claude-opus-4-6" :provider :anthropic
     :context-window 200000 :max-output-tokens 32000
     :tools-p t :streaming-p t :thinking-p t
     :adaptive-thinking-p t :json-output-p t
     :vision-p t :prompt-caching-p t
     :input-price-per-mtok 15.0 :output-price-per-mtok 75.0
     :cache-read-price-per-mtok 1.5 :cache-write-price-per-mtok 18.75))
  ;; Claude Haiku 4.5
  (register-model registry
    (make-model-capabilities
     :name "claude-haiku-4-5" :provider :anthropic
     :context-window 200000 :max-output-tokens 8192
     :tools-p t :streaming-p t :thinking-p nil
     :adaptive-thinking-p nil :json-output-p t
     :vision-p t :prompt-caching-p t
     :input-price-per-mtok 0.8 :output-price-per-mtok 4.0
     :cache-read-price-per-mtok 0.08 :cache-write-price-per-mtok 1.0))
  ;; Aliases
  (register-alias registry "claude-sonnet" "claude-sonnet-4-6")
  (register-alias registry "claude-opus" "claude-opus-4-6")
  (register-alias registry "claude-haiku" "claude-haiku-4-5")
  ;; Provider defaults
  (register-provider-default registry :anthropic
    (make-model-capabilities
     :name "claude-sonnet-4-6" :provider :anthropic
     :context-window 200000 :max-output-tokens 16384
     :tools-p t :streaming-p t :thinking-p t))
  (register-provider-default registry :openrouter
    (make-model-capabilities
     :name "claude-sonnet-4-6" :provider :openrouter
     :context-window 200000 :max-output-tokens 16384
     :tools-p t :streaming-p t :thinking-p t))
  registry)

(defun make-default-model-registry ()
  "Create a model registry with all known models and aliases."
  (register-known-models (make-model-registry)))

(in-package #:claw-lisp.core.protocols)

(defclass provider ()
  ((name
    :initarg :name
    :accessor provider-name
    :documentation "Human-readable provider identifier.")
   (api-key
    :initarg :api-key
    :initform nil
    :accessor provider-api-key
    :documentation "Provider credential string when configured.")
   (base-url
    :initarg :base-url
    :initform nil
    :accessor provider-base-url
    :documentation "Provider endpoint base URL.")
   (model-registry
    :initarg :model-registry
    :initform (claw-lisp.core.model-registry:make-default-model-registry)
    :reader provider-model-registry
    :documentation "Cached model registry for resolving model capabilities.")))

(defgeneric send-turn (provider conversation &key model tools)
  (:documentation
   "Submit CONVERSATION to PROVIDER and return a provider-native response."))

(defgeneric stream-turn (provider conversation &key model tools on-event)
  (:documentation
   "Stream a turn from PROVIDER. ON-EVENT receives accumulated text deltas."))

(defgeneric count-tokens (provider messages &key model)
  (:documentation
   "Return the best token-count estimate for MESSAGES under MODEL."))

(defgeneric normalize-response (provider response)
  (:documentation
   "Convert a provider-native RESPONSE into the runtime's internal shape."))

(defclass tool ()
  ((name
    :initarg :name
    :accessor tool-name
    :documentation "Stable tool name exposed to the model.")
   (description
    :initarg :description
    :accessor tool-description
    :documentation "User-facing tool description.")))

(defgeneric tool-input-schema (tool)
  (:documentation
   "Return a JSON Schema (as a property list) describing the tool's input parameters.
    The schema follows the JSON Schema draft-07 format with a :type, :properties,
    and :required keys."))

(defgeneric validate-tool-input (tool input)
  (:documentation "Validate tool INPUT before execution."))

(defgeneric execute-tool (tool input runtime)
  (:documentation "Execute TOOL with INPUT in the context of RUNTIME."))

(defgeneric normalize-tool-result (tool result)
  (:documentation "Normalize RESULT into the runtime's tool-result shape."))

(defmethod validate-tool-input ((tool tool) input)
  (declare (ignore tool))
  input)

(defmethod normalize-tool-result ((tool tool) result)
  (declare (ignore tool))
  result)

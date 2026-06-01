(asdf:defsystem "achatina"
  :description "Compatibility alias for the Achatina runtime system."
  :author "seyiakadri@gmail.com"
  :license "BSL-1.1 (see LICENSE)"
  :version "0.1.0"
  :depends-on ("claw-lisp"))

(asdf:defsystem "achatina/test"
  :description "Compatibility alias for the Achatina test system."
  :author "seyiakadri@gmail.com"
  :license "BSL-1.1 (see LICENSE)"
  :version "0.1.0"
  :depends-on ("claw-lisp/test"))

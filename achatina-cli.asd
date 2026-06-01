(asdf:defsystem "achatina-cli"
  :description "Compatibility alias for the Achatina CLI system."
  :author "seyiakadri@gmail.com"
  :license "BSL-1.1 (see LICENSE)"
  :version "0.1.0"
  :depends-on ("claw-lisp-cli")
  :entry-point "claw-lisp.cli:main-entry-point")

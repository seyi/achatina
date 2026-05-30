(asdf:defsystem "claw-lisp-cli"
  :description "CLI entrypoint for the standalone claw-lisp runtime."
  :author "seyiakadri@gmail.com"
  :license "BSL-1.1 (see LICENSE)"
  :version "0.1.0"
  :depends-on ("claw-lisp")
  :serial t
  :components
  ((:file "lisp/cli/main"))
  :entry-point "claw-lisp.cli:main-entry-point")

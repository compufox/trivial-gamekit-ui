;;;; trivial-gamekit-ui.asd

(asdf:defsystem #:trivial-gamekit-ui-example
  :description "Describe trivial-gamekit-ui here"
  :author "ava fox"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :depends-on (#:trivial-gamekit #:trivial-gamekit-ui)
  :components ((:file "example")))

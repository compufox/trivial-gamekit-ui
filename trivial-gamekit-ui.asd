;;;; trivial-gamekit-ui.asd

(asdf:defsystem #:trivial-gamekit-ui
  :description "Describe trivial-gamekit-ui here"
  :author "ava fox"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :depends-on (#:trivial-gamekit #:trivial-gamekit-colors)
  :components ((:file "package")
               (:file "util")
               (:file "widgets")
               (:file "making")
               (:file "drawing")
               (:file "helpers")))

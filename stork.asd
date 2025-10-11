;;;; stork.asd

(asdf:defsystem #:stork
  :description "Describe stork here"
  :author "Your Name <your.name@example.com>"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :components ((:file "package")
               (:file "stork")
               (:file "entity")
               (:file "event")
               (:file "logic")
               (:file "map"))

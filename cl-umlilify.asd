;;;; cl-umlilify.asd

(asdf:defsystem #:cl-umlilify
  :serial t
  :depends-on (#:closer-mop)
  :components ((:file "package")
               (:file "cl-umlilify")))


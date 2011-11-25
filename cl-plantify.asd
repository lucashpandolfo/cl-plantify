;;;; cl-plantify.asd

(asdf:defsystem #:cl-plantify
  :serial t
  :depends-on (#:closer-mop)
  :components ((:file "package")
               (:file "cl-plantify")))


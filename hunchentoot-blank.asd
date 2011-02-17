;;;; hunchentoot-blank.asd

(asdf:defsystem #:hunchentoot-blank
  :serial t
  :depends-on (#:hunchentoot
               #:clsql
               #:cl-who
               #:ironclad)
  :components ((:file "package")
               (:file "hunchentoot-blank")))


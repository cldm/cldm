(asdf:defsystem #:cldm
  :serial t
  :description "Common Lisp Dependency Manager"
  :author "Mariano Montone"
  :license "MIT"
  :depends-on (#:alexandria #:ironclad #:md5 #:cl-ppcre #:cl-syntax)
  :components ((:file "package")
               (:file "cldm")
	       (:file "version")))

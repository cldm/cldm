(asdf:defsystem #:cldm
  :serial t
  :description "Common Lisp Dependency Manager"
  :author "Mariano Montone"
  :license "MIT"
  :depends-on (#:alexandria #:ironclad #:md5 #:cl-ppcre #:cl-syntax #:esrap)
  :components ((:file "package")
               (:file "cldm")
	       (:file "util")
	       (:file "version")
	       (:file "interval")
	       (:file "requirement"))
  :serial t)

(asdf:defsystem #:cldm
  :serial t
  :description "Common Lisp Dependency Manager"
  :author "Mariano Montone"
  :license "MIT"
  :components ((:module :src
			:components
			((:file "package")
			 (:file "cldm")
			 (:file "util")
			 (:file "version")
			 (:file "interval")
			 (:file "requirement")
			 (:file "library")
			 (:file "repository")
			 (:file "pool")
			 (:file "deflibrary"))
			:serial t))
    :depends-on (#:alexandria
		 #:ironclad
		 #:md5
		 #:cl-ppcre
		 #:cl-syntax
		 #:esrap
		 #:external-program
		 #:puri))

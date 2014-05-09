(asdf:defsystem #:cldm-test
  :serial t
  :description "Common Lisp Dependency Manager tests"
  :author "Mariano Montone"
  :license "MIT"
  :depends-on (#:cldm #:stefil)
  :components ((:module "test"
			:components
			((:file "package")
			 (:file "test")
			 (:file "version"))
			:serial t)))

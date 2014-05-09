(asdf:defsystem #:cldm-test
  :serial t
  :description "Common Lisp Dependency Manager tests"
  :author "Mariano Montone"
  :license "MIT"
  :depends-on (#:cldm #:alexandria #:ironclad #:md5 #:stefil)
  :components ((:module "test"
			:components
			((:file "test")))))

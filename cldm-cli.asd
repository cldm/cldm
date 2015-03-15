(asdf:defsystem #:cldm-cli
  :serial t
  :description "CLDM command line tool"
  :author "Mariano Montone"
  :components ((:module :src
			:components
			((:module :cli
				  :components
				  ((:file "package")
				   (:file "init")
				   (:file "config")
				   (:file "repository")
				   (:file "cli"))
				  :serial t))))
  :depends-on (#:cldm
	       #:com.dvlsoft.clon
	       #:trivial-backtrace))

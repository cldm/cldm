(asdf:defsystem #:cldm
  :serial t
  :description "Common Lisp Dependency Manager"
  :author "Mariano Montone"
  :license "Copyright (c) 2014 Mariano Montone

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the \"Software\"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED \"AS IS\", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE."
  :components ((:module :src
			:components
			((:file "package")
			 (:file "util")
			 (:file "config")
			 (:file "interval")
			 (:file "requirement")			 
			 (:file "library")
			 (:file "cld")
			 (:file "repository-address")
			 (:module :repository
				  :components
				  ((:file "repository")
				   (:file "http-repository")
				   (:file "ssh-repository")
				   (:file "directory-repository")
				   (:file "registry-repository")
				   (:file "cached-repository")
				   (:file "indexed-repository"))
				  :serial t)
			 (:file "project")
			 (:file "pbo")
			 (:file "deflibrary")
			 (:file "asdf")
			 (:file "cldm")
			 (:file "commands")
			 (:file "cldm-user"))
			:serial t))
    :depends-on (#:cl-semver
		 #:alexandria
		 #:anaphora
		 #:md5
		 #:cl-ppcre
		 #:cl-syntax
		 #:esrap
		 #:trivial-shell
		 #:puri
		 #:split-sequence
		 #:cl-fad
		 #:osicat
		 #:montezuma
		 #:drakma
		 #:cl-json
		 ;#:cldm-loader
		 ))

(defmethod perform ((o test-op) (c (eql (find-system 'cldm))))
  (oos 'load-op 'cldm-test)
  (oos 'test-op 'cldm-test))

(ql:quickload :cl-embdoc)
(ql:quickload :cldm)

(defpackage :cldm.doc
  (:use :cl :cldm :embdoc)
  (:export :generate))

(in-package :cldm.doc)

(defparameter *files* 
  (list "cldm.lisp"
	"library.lisp"
	"pbo.lisp"))

(defun get-files ()
  (loop for file in *files*
     collect (asdf:system-relative-pathname :cldm (format nil "src/~A" file))))

(defun gendoc (pathname &key prelude postlude)
  (let ((fragments
	 (loop for file in (get-files)
	    appending
	      (embdoc:parse-lisp-source (embdoc:file-to-string file)))))
    (with-open-file (f pathname :direction :output 
		       :if-exists :supersede
		       :if-does-not-exist :create)
      (when prelude
	(write-string 
	 (if (pathnamep prelude)
	     (embdoc:file-to-string prelude)
	     prelude)
	 f))
      (write-string (embdoc:gen-sphinx-doc fragments) f)
      (when postlude
	(write-string (if (pathnamep postlude)
			  (embdoc::file-to-string postlude)
			  postlude)
		      f)))))

(defun generate ()
  (gendoc (asdf:system-relative-pathname :cldm "doc/developer/index.rst")))

(ql:quickload :erudite)
(ql:quickload :cldm)

(defpackage :cldm.doc
  (:use :cl :cldm)
  (:export :generate))

(in-package :cldm.doc)

(defparameter *files* 
  (list "cldm.lisp"
	"library.lisp"
	"pbo.lisp"))

(defun get-files ()
  (loop for file in *files*
     collect (asdf:system-relative-pathname :cldm (format nil "src/~A" file))))


(defun generate ()
  (erudite:gen-sphinx-doc 
   (asdf:system-relative-pathname :cldm "doc/developer/index.rst")
   (get-files)))

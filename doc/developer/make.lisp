(ql:quickload :erudite)
(ql:quickload :cldm)

(defpackage :cldm.doc
  (:use :cl :cldm :erudite)
  (:export :generate))

(in-package :cldm.doc)

(defparameter *files* 
  (list "cldm.lisp"
	"library.lisp"
	"deflibrary.lisp"
	"pbo.lisp"
	"repository/repository.lisp"
	"repository/download-session.lisp"
	"repository-address.lisp"
	"project.lisp"))

(defun get-files ()
  (loop for file in *files*
     collect (asdf:system-relative-pathname :cldm (format nil "src/~A" file))))

(defun generate ()
  (erudite:erudite 
   (asdf:system-relative-pathname :cldm "doc/developer/cldm.tex")
   (get-files)
   :title "CLDM developer manual"
   :author "Mariano Montone"
   :input-type :latex
   :output-type :latex))

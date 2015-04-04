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

(defun gendoc (pathname &key title author)
  (let ((fragments
	 (loop for file in (get-files)
	    appending
	      (embdoc:parse-lisp-source (embdoc:file-to-string file)))))
    (with-open-file (f pathname :direction :output 
		       :if-exists :supersede
		       :if-does-not-exist :create)
      (format f "\\documentclass[11pt,pdflatex,makeidx]{book}              % Book class in 11 points~%")
      (format f "\\usepackage{makeidx}~%")
      (format f "\\usepackage{hyperref}~%")
      (format f "\\usepackage{listings}~%")
      (format f "\\lstloadlanguages{Lisp}~%")
      (format f "\\lstset{frame=L,language=Lisp,
  stringstyle=\\ttfamily\\small,basicstyle=\\ttfamily\\footnotesize,
  showstringspaces=false,breaklines}~%")
      (format f "\\lstnewenvironment{code}{}{}~%")
      (format f "\\parindent0pt  \\parskip10pt             % make block paragraphs~%")
      (format f "\\raggedright                            % do not right justify~%")
      (format f "\\title{\\bf ~A}    % Supply information~%" title)
      (format f "\\author{~A}              %   for the title page.~%" author)
      (format f "\\date{\today}                           %   Use current date.~%")
      (format f "% Note that book class by default is formatted to be printed back-to-back.~%")
      (format f "\\makeindex~%")
      (format f "\\begin{document}                        % End of preamble, start of text.~%")
      (format f "\\frontmatter                            % only in book class (roman page #s)~%")
      (format f "\\maketitle                              % Print title page.~%")
      (format f "\\tableofcontents                        % Print table of contents~%")
      (format f "\\mainmatter                             % only in book class (arabic page #s)~%")
      (format f "\\long\\def\\ignore#1{}~%")
      (write-string (embdoc:gen-latex-doc fragments) f)
      (format f "~%\\printindex~%")
      (write-string "\\end{document}" f))))

(defun generate ()
  (gendoc (asdf:system-relative-pathname :cldm "doc/developer/cldm.tex")
	  :title "CLDM developer manual"
	  :author "Mariano Montone"))

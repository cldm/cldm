#|

\chapter{Pseudo-Boolean Optimization}

\section{Overview}

For solving versions constraints Pseudo-Boolean Optimization (PBO) is used.

See: \url{http://www.mancoosi.org/papers/ase10.pdf}

`minisat+ \url{https://github.com/niklasso/minisatp>} is the PBO solver being used at the moment.

|#

(in-package :cldm)

(defparameter *pbo-environment* nil)

(defparameter *constraint-variable-counter* 1)


#|

A \verb+pbo-constraint+ is a constraint with:

\begin{itemize}
\item Terms: x1, x2, ..., xn
\item Comparision: A comparison operator
\item Result: The equation result
\item Comment: a comment that appears in the resulting .pbo file for debugging purposes mostly.
\end{itemize}

|#
(defstruct (pbo-constraint
             (:print-function print-pbo-constraint))
  terms comparison result comment)

(defstruct optimization-function
  terms)

(defun print-pbo-constraint (pbo-constraint stream depth)
  (format stream "[~{~A~} ~A ~A ~S]"
          (pbo-constraint-terms pbo-constraint)
          (pbo-constraint-comparison pbo-constraint)
          (pbo-constraint-result pbo-constraint)
          (pbo-constraint-comment pbo-constraint)))

(defun make-pbo-constraint* (terms comparison result &optional comment)
  (make-pbo-constraint :terms terms
                       :comparison comparison
                       :result result
                       :comment comment))

#|
\section{Algorithm}

Each dependent library and version is encoded as a PBO variable.

Example: hunchentoot-1.0 is x1, and hunchentoot-2.0 is x2

|#

(defun gen-pbo-variable (thing)
  "Return a existing PBO variable, or generate a new one"
  (if (assoc thing *pbo-environment* :test #'library-version=)
      (cdr (assoc thing *pbo-environment* :test #'library-version=))
      ;; else
      (let ((var (make-keyword (format nil "X~A"
                                       *constraint-variable-counter* ))))
        (push (cons thing var) *pbo-environment*)
        (incf *constraint-variable-counter*)
        var)))

#|

An intermediate representation is used. A list of PBO terms with this form:

\verb'dep1 + dep2 + ... + depn - lib >= 0'

where dep1 .. depn are library versions or a dependent library.

|#

(defun encode-dependency (library-version dependency)
  (let* ((dependency-library (find-library (library-name dependency) nil)))
    ;; Note: we allow the dependency library not to exist here
    ;; This is because the library is not available for some reason, but we rely
    ;; on that the library is availabe in the user local system (i.e. via Quicklisp)
    ;; When the library does not exist, we don't encode any depedencies
    (when dependency-library
      (let ((library-versions (find-library-versions dependency-library dependency)))
        (let ((terms (append
                      (loop for library-version in library-versions
                         collect `(+ 1 ,(gen-pbo-variable
                                         library-version)))
                      `((- 1 ,(gen-pbo-variable library-version))))))
          (make-pbo-constraint* terms
                                '>= 0
                                (format nil "~A dependency: ~A"
                                        (library-version-unique-name library-version)
                                        (print-requirement-to-string dependency))))))))

#|

Conflicts are encoded like: 

\verb'lib1 + lib2 <= 1'

|#

(defun encode-conflict (library-version-1 library-version-2)
  (make-pbo-constraint*
   `((+ 1 ,(gen-pbo-variable library-version-1))
     (+ 1 ,(gen-pbo-variable library-version-2)))
   '<=
   1
   (format nil "Conflict between ~A and ~A"
           (library-version-unique-name library-version-1)
           (library-version-unique-name library-version-2))))

#|

A library install is encoded like:

\verb'lib >= 1'

|#

(defun encode-install (library-version)
  (make-pbo-constraint*
   `((+ 1 ,(gen-pbo-variable library-version)))
   '>=
   1
   (format nil "Install ~A" (library-version-unique-name library-version))))

(defun library-versions-conflict-p (library-version-1 library-version-2)
  (and (equalp (library-name library-version-1)
               (library-name library-version-2))
       (and
        (version/== (version library-version-1)
                    (version library-version-2))
        (not (and (equalp (version library-version-1) :max-version)
                  (equalp (version library-version-2) :max-version))))))

(defun encode-library-versions-conflicts (library-versions)
  (loop for library-version-1 in library-versions
     appending
       (loop for library-version-2 in (cdr library-versions)
          when (library-versions-conflict-p library-version-1
                                            library-version-2)
          collect (encode-conflict library-version-1
                                   library-version-2))))

(defun encode-library-version-dependencies (library-version)
  (let ((dependency-constraints
         (remove-if #'null
                    (loop for dependency in (dependencies library-version)
                       collect
                         (encode-dependency library-version dependency)))))
    dependency-constraints))

(defun encode-install-library-version (library-version library-versions-involved)
  (let ((install-constraint (encode-install library-version))
        (dependencies-constraints
         (loop for library-version in library-versions-involved
            appending (encode-library-version-dependencies library-version)))
        (conflicts-constraints (encode-library-versions-conflicts
                                library-versions-involved)))
    (let ((all-constraints (append (list install-constraint)
                                   dependencies-constraints
                                   conflicts-constraints)))
      (values
       all-constraints
       *pbo-environment*
       *constraint-variable-counter*
       (length all-constraints)))))

(defun encode-install-library-versions (library-versions library-versions-involved)
  (let ((install-constraints (loop for library-version in library-versions
                                collect (encode-install library-version)))
        (dependencies-constraints
         (loop for library-version in library-versions-involved
            appending (encode-library-version-dependencies library-version)))
        (conflicts-constraints (encode-library-versions-conflicts
                                library-versions-involved)))
    (let ((all-constraints (append install-constraints
                                   dependencies-constraints
                                   conflicts-constraints)))
      (values
       all-constraints
       *pbo-environment*
       *constraint-variable-counter*
       (length all-constraints)))))

#|

\section{Serialization}

PBO constraints are then serialized to a Minisat file:

|#

(defun serialize-pbo-constraints (pbo-constraints stream)
  (loop for pbo-constraint in pbo-constraints
     do
       (progn
         (serialize-pbo-constraint pbo-constraint stream)
         (format stream "~%"))))

(defun serialize-pbo-constraint (pbo-constraint stream)
  (format stream "* ~A *~%" (pbo-constraint-comment pbo-constraint))
  (loop for term in (pbo-constraint-terms pbo-constraint)
     do (destructuring-bind (sign constant var) term
          (format stream "~A~A*~A " sign constant
                  (string-downcase (symbol-name var)))))
  (format stream "~A ~A ;"
          (pbo-constraint-comparison pbo-constraint)
          (pbo-constraint-result pbo-constraint)))

#|

The purpose of all this is to solve an optimization function so that the "best"
library versions are chosen:

|#

(defun create-optimization-function (library-versions-involved)
  (flet ((sort-library-versions-by-freshness (library-versions)
           (sort library-versions #'version> :key #'version)))
    (let ((grouped-library-versions
           (mapcar #'sort-library-versions-by-freshness
                   (group-by library-versions-involved
                             :key #'library-name
                             :test #'equalp))))
      (loop for versions-group in grouped-library-versions
         appending
           (loop for library-version in versions-group
              for wi = 0 then (1+ wi)
              collect `(+ ,wi ,(gen-pbo-variable
                                library-version)))))))

(defun serialize-optimization-function (optimization-function stream)
  (loop for term in optimization-function
     do (destructuring-bind (sign constant var) term
          (format stream "~A~A*~A " sign constant
                  (string-downcase (symbol-name var))))))

#|

PBO equations are serialized to a temporal \verb+deps.pbo+ file.

|#

(defun pbo-solve-library-versions (library-version library-versions-involved)
  (let ((*pbo-environment* nil)
        (*constraint-variable-counter* 1))
    (multiple-value-bind (constraints pbo-environment
                                      variables-number constraints-number)
        (encode-install-library-version 
	 library-version 
	 library-versions-involved)
      (let ((optimization-function
             (create-optimization-function library-versions-involved)))
        (let ((pbo-file #p"/tmp/deps.pbo"))
          (with-open-file (stream pbo-file
                                  :direction :output
                                  :if-does-not-exist :create
                                  :if-exists :supersede)
            (format stream "* #variable= ~A #constraint= ~A~%"
                    variables-number
                    constraints-number)
            (format stream "min: ")
            (serialize-optimization-function optimization-function stream)
            (format stream " ;~%" )
            (serialize-pbo-constraints constraints stream))
          (multiple-value-bind (result error status)
              (trivial-shell:shell-command
               (format nil "~A ~A -v0" *minisat+-binary* pbo-file))
	    (when (equalp status 20)
	      (error "Dependencies are not satisfiable"))
            (when (not (or (zerop status)
			   (equalp status 30)))
              (error "Error executing ~A ~A -v0" *minisat+-binary* pbo-file))
            (flet ((find-environment-library-version (var)
                     (car (rassoc var pbo-environment))))
              (cl-ppcre:register-groups-bind (vars-string)
                  ("\v (.*)" result)
                (let ((vars (remove-if #'null
                                       (mapcar (compose #'find-environment-library-version
                                                        #'make-keyword
                                                        #'string-upcase)
                                               (split-sequence:split-sequence #\  vars-string)))))
                  vars)))))))))

(defun pbo-solve-install-library-versions (library-versions library-versions-involved)
  (let ((*pbo-environment* nil)
        (*constraint-variable-counter* 1))
    (multiple-value-bind (constraints pbo-environment
                                      variables-number constraints-number)
        (encode-install-library-versions library-versions library-versions-involved)
      (let ((optimization-function
             (create-optimization-function library-versions-involved)))
        (let ((pbo-file #p"/tmp/deps.pbo"))
          (with-open-file (stream pbo-file
                                  :direction :output
                                  :if-does-not-exist :create
                                  :if-exists :supersede)
            (format stream "* #variable= ~A #constraint= ~A~%"
                    variables-number
                    constraints-number)
            (format stream "min: ")
            (serialize-optimization-function optimization-function stream)
            (format stream " ;~%" )
            (serialize-pbo-constraints constraints stream))
          (multiple-value-bind (result error status)
              (trivial-shell:shell-command
               (format nil "~A ~A -v0" *minisat+-binary* pbo-file))
            (when (not (zerop status))
              (error "Error executing ~A ~A -v0" *minisat+-binary* pbo-file))
            (when (cl-ppcre:scan "UNSATISFIABLE" result)
              (error "Could not satisfy dependencies: ~{~A~^, ~}"
                     (mapcar #'pbo-constraint-comment constraints)))
            (flet ((find-environment-library-version (var)
                     (car (rassoc var pbo-environment))))
              (cl-ppcre:register-groups-bind (vars-string)
                  ("\v (.*)" result)
                (let ((vars (remove-if #'null
                                       (mapcar (compose #'find-environment-library-version
                                                        #'make-keyword
                                                        #'string-upcase)
                                               (split-sequence:split-sequence #\  vars-string)))))
                  vars)))))))))

#|

Here is an example \verb+deps.pbo+ file for installing Hunchentoot library:

\begin{verbatim}

     * #variable= 20 #constraint= 30
     min: +0*x1 +0*x2 +1*x3 +0*x4 +0*x5 +0*x6 +0*x8 +0*x16 +0*x19 +0*x18 +0*x7 +0*x14 +0*x17 +0*x9 +0*x10 +0*x11 +0*x12 +0*x13 +0*x15  ;
     * Install hunchentoot-1.2.26 *
     +1*x1 >= 1 ;
     * hunchentoot-1.2.26 dependency: chunga *
     +1*x2 +1*x3 -1*x1 >= 0 ;
     * hunchentoot-1.2.26 dependency: cl-base64 *
     +1*x4 -1*x1 >= 0 ;
     * hunchentoot-1.2.26 dependency: cl-fad *
     +1*x5 -1*x1 >= 0 ;
     * hunchentoot-1.2.26 dependency: cl-ppcre *
     +1*x6 -1*x1 >= 0 ;
     * hunchentoot-1.2.26 dependency: flexi-streams *
     +1*x7 -1*x1 >= 0 ;
     * hunchentoot-1.2.26 dependency: cl+ssl *
     +1*x8 -1*x1 >= 0 ;
     * hunchentoot-1.2.26 dependency: md5 *
     +1*x9 -1*x1 >= 0 ;
     * hunchentoot-1.2.26 dependency: rfc2388 *
     +1*x10 -1*x1 >= 0 ;
     * hunchentoot-1.2.26 dependency: trivial-backtrace *
     +1*x11 -1*x1 >= 0 ;
     * hunchentoot-1.2.26 dependency: usocket *
     +1*x12 -1*x1 >= 0 ;
     * hunchentoot-1.2.26 dependency: bordeaux-threads *
     +1*x13 -1*x1 >= 0 ;
     * chunga-1.1.5 dependency: trivial-gray-streams *
     +1*x14 -1*x2 >= 0 ;
     * chunga-1.1.1 dependency: trivial-gray-streams *
     +1*x14 -1*x3 >= 0 ;
     * cl-fad-0.7.2 dependency: bordeaux-threads *
     +1*x13 -1*x5 >= 0 ;
     * cl-fad-0.7.2 dependency: alexandria *
     +1*x15 -1*x5 >= 0 ;
     * cl+ssl-latest dependency: cffi *
     +1*x16 -1*x8 >= 0 ;
     * cl+ssl-latest dependency: trivial-gray-streams *
     +1*x14 -1*x8 >= 0 ;
     * cl+ssl-latest dependency: flexi-streams *
     +1*x7 -1*x8 >= 0 ;
     * cl+ssl-latest dependency: bordeaux-threads *
     +1*x13 -1*x8 >= 0 ;
     * cl+ssl-latest dependency: trivial-garbage *
     +1*x17 -1*x8 >= 0 ;
     * cffi-0.12.0 dependency: alexandria *
     +1*x15 -1*x16 >= 0 ;
     * cffi-0.12.0 dependency: trivial-features *
     +1*x18 -1*x16 >= 0 ;
     * cffi-0.12.0 dependency: babel *
     +1*x19 -1*x16 >= 0 ;
     * babel-0.3.0 dependency: trivial-features *
     +1*x18 -1*x19 >= 0 ;
     * babel-0.3.0 dependency: alexandria *
     +1*x15 -1*x19 >= 0 ;
     * flexi-streams-1.0.11 dependency: trivial-gray-streams *
     +1*x14 -1*x7 >= 0 ;
     * bordeaux-threads-0.8.3 dependency: alexandria *
     +1*x15 -1*x13 >= 0 ;
     * Conflict between chunga-1.1.5 and chunga-1.1.1 *
     +1*x2 +1*x3 <= 1 ;
     * Conflict between chunga-1.1.1 and chunga-1.1.5 *
     +1*x3 +1*x2 <= 1 ;
\end{verbatim}
|#

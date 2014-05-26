(in-package :cldm)

(defstruct (pbo-constraint
	     (:print-function print-pbo-constraint))
  terms comparison result comment)

(defstruct optimization-function
  terms)

(defparameter *pbo-environment* nil)
(defparameter *constraint-variable-counter* 0)

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

(defun gen-pbo-variable (thing)
  (if (assoc thing *pbo-environment* :test #'equalp)
      (cdr (assoc thing *pbo-environment* :test #'equalp))
      ;; else
      (let ((var (make-keyword (format nil "X~A"
				       *constraint-variable-counter* ))))
	(push (cons thing var) *pbo-environment*)
	(incf *constraint-variable-counter*)
	var)))

(defun encode-dependency (library-version dependency)
  (let* ((dependency-library (find-library (library-name dependency)))
	 (library-versions (find-library-versions dependency-library dependency)))
    (let ((terms (append
		  (loop for library-version in library-versions
		     collect `(+ 1 ,(gen-pbo-variable
				     (library-version-unique-name library-version))))
		  `((- 1 ,(gen-pbo-variable (library-version-unique-name library-version)))))))    
    (make-pbo-constraint* terms
			  '>= 0
			  (format nil "~A dependency: ~A"
				  (library-version-unique-name library-version)
				  (library-name dependency))))))

(defun encode-conflict (library-version-1 library-version-2)
  (make-pbo-constraint*
   `((+ 1 ,(gen-pbo-variable
	    (library-version-unique-name library-version-1)))
     (+ 1 ,(gen-pbo-variable
	    (library-version-unique-name library-version-2))))
   '<
   1
   (format nil "Conflict between ~A and ~A"
	   (library-version-unique-name library-version-1)
	   (library-version-unique-name library-version-2))))

(defun encode-install (library-version)
  (make-pbo-constraint*
   `((+ 1 ,(gen-pbo-variable (library-version-unique-name library-version))))
   '>=
   1
   (format nil "Install ~A" (library-version-unique-name library-version))))

(defun encode-library-versions (library-versions)
  (let ((grouped-library-versions
	 (group-by 
	  (remove-duplicates library-versions
			     :key #'library-version-unique-name
			     :test #'equalp)
	  :key (compose #'library-unique-name #'library)
	  :test #'equalp)))
    (let ((pbo-constraints
	   (loop for library-versions-group in grouped-library-versions
		appending
		(loop for library-version in library-versions-group
		     collect (encode-library-version library-version)))))
      pbo-constraints)))

(defun encode-library-versions-conflicts (library-versions)
  (loop for library-version-1 in library-versions
       for library-version-2 in (cdr library-versions)
       when (and (equalp (library-name library-version-1)
			 (library-name library-version-2))
		 (version/== (version library-version-1)
			     (version library-version-2)))
       collect (encode-conflict library-version-1
				library-version-2)))  

(defun encode-library-version-dependencies (library-version)
  (let ((dependency-constraints
	 (loop for dependency in (dependencies library-version)
	    collect 
	      (encode-dependency library-version dependency))))
    dependency-constraints))

;; (defun libraries-involved (library-versions)
;;   (loop with libraries = nil
;;        for library-version in library-versions
;;        when (not (member (library library-version)
;; 			 libraries))
			 
;;        do (push (library library-version) libraries)
;;        finally (return libraries)))

(defun encode-install-library-version (library-version)
  (let ((*pbo-environment* nil)
	(*constraint-variable-counter* 0))
    (let* ((complete-library-versions (cons library-version (calculate-library-versions library-version))))	   
      (let ((install-constraint (encode-install library-version))
	    (dependencies-constraints
	     (loop for library-version in complete-library-versions
		appending (encode-library-version-dependencies library-version)))
	    (conflicts-constraints (encode-library-versions-conflicts
				    complete-library-versions)))
	(let ((all-constraints (append (list install-constraint)
				       dependencies-constraints
				       conflicts-constraints)))
	  (values
	   all-constraints
	   *pbo-environment*
	   *constraint-variable-counter*
	   (length all-constraints)))))))

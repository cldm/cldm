(in-package :cldm)

(defclass library ()
  ((id :initarg :id
       :initform nil
       :accessor library-id
       :type integer
       :documentation "The library id")
   (name :initarg :name
	 :initform (error "Provide the library name")
	 :accessor library-name
	 :type string
	 :documentation "Name of the library (i.e. distribution name)")
   (version :initarg :version
	    :initform (error "Provide the library version")
	    :accessor library-version
	    :type version
	    :documentation "The library version (instance of type version)")
   (dependencies :initarg :dependencies
		 :initform nil
		 :accessor library-dependencies
		 :documentation "The library dependencies (list of requirement objects)")
   (provides :initarg :provides
	     :initform nil
	     :accessor library-provides
	     :documentation "List of requirements the library provides")
   (conflicts :initarg :conflicts
	      :initform nil
	      :accessor library-conflicts
	      :documentation "List of requirements the library is in conflict with")
   (replaces :initarg :replaces
	     :initform nil
	     :accessor library-replaces
	     :documentation "List of requirements the library replaces")
   (suggests :initarg :suggests
	     :initform nil
	     :accessor library-suggests
	     :documentation "List of requirements the library suggests")

   (repository :initarg :repository
	       :initform nil ;(error "Provide the library repository")
	       :accessor library-repository
	       :type repository
	       :documentation "The library repository"))
  (:documentation "library-info instances contain all the metadata needed for dependency management"))

(defrule requirement-type (or "depends" "provides" "suggests" "conflicts" "replaces")
  (:function (lambda (match)
	       (make-keyword (string-upcase match)))))

(defrule library-requirements
    (and requirement-type
	 spaces
	 #\(
	 distribution-constraint
	 (* (and #\, spaces distribution-constraint))
	 #\))
  (:function (lambda (match)
	       (destructuring-bind (requirement-type
				    spaces
				    open-paren
				    constraint
				    constraints
				    close-paren) match
		 (list requirement-type (cons constraint (mapcar #'third constraints)))))))

(defrule library (and library-unique-name (* (and #\; spaces library-requirements)))
  (:function (lambda (match)
	       (destructuring-bind (name requirements) match
		   (list name (mapcar #'third requirements))))))

(defun read-library-from-string (string)
  (destructuring-bind (unique-name requirements)
      (parse 'library string)
    (make-instance 'library
		   :name (first unique-name)
		   :version (second unique-name)
		   :dependencies
		   (let ((depends (cadar
				   (remove-if-not (lambda (reqs)
						    (equalp (first reqs) :depends))
						  requirements))))
		     (loop for constraint in depends
			collect (progn
					;(break "~A" constraint)
				  (make-requirement (first constraint) (second constraint)))))
		   :provides
		   (let ((provides (cadar
				    (remove-if-not (lambda (reqs)
						     (equalp (first reqs) :provides))
						   requirements))))
		     (loop for constraint in provides
			collect (make-requirement (first constraint) (second constraint))))
		   :conflicts
		   (let ((conflicts (cadar
				     (remove-if-not (lambda (reqs)
						      (equalp (first reqs) :conflicts))
						    requirements))))
		     (loop for constraint in conflicts
			collect (make-requirement (first constraint) (second constraint))))
		   :suggests
		   (let ((suggests (cadar
				    (remove-if-not (lambda (reqs)
						     (equalp (first reqs) :suggests))
						   requirements))))
		     (loop for constraint in suggests
			collect (make-requirement (first constraint) (second constraint))))
		   :replaces
		   (let ((replaces (cadar
				    (remove-if-not (lambda (reqs)
						     (equalp (first reqs) :replaces))
						   requirements))))
		     (loop for constraint in replaces
			collect (make-requirement (first constraint) (second constraint)))))))

(defun library-unique-name (library)
  (format nil "~A~@[-~A~]"
	  (library-name library)
	  (when (library-version library)
	    (print-version-to-string (library-version library)))))

(defun valid-library-name-p (string)
  (not (null
	(ignore-errors
	  (parse 'library-unique-name string)))))

(defmethod describe-object ((library library) stream)
  (format stream "~A library ~%~%" (library-unique-name library))
  (format stream "Dependencies: ~{~a~^, ~}~%"
	  (or
	   (mapcar #'print-requirement-to-string
		   (library-dependencies library))
	   (list "None")))
  (format stream "Provides: ~{~a~^, ~}~%"
	  (or
	   (mapcar #'print-requirement-to-string
		   (library-provides library))
	   (list "None")))
  (format stream "Conflicts: ~{~a~^, ~}~%"
	  (or
	   (mapcar #'print-requirement-to-string
		   (library-dependencies library))
	   (list "None")))
  (format stream "Replaces: ~{~a~^, ~}~%"
	  (or
	   (mapcar #'print-requirement-to-string
		   (library-replaces library))
	   (list "None")))
  (format stream "Suggests: ~{~a~^, ~}~%"
	  (or
	   (mapcar #'print-requirement-to-string
		   (library-suggests library))
	   (list "None"))))

(defun print-library (library stream)
  (format stream "~A" (library-unique-name library))
  (when (library-dependencies library)
    (format stream "; depends (~{~a~^, ~})"
	    (mapcar #'print-requirement-to-string
		    (library-dependencies library))))
  (when (library-provides library)
    (format stream "; provides (~{~a~^, ~})"
	    (mapcar #'print-requirement-to-string
		    (library-provides library))))
  (when (library-replaces library)
    (format stream "; replaces (~{~a~^, ~})"
	    (mapcar #'print-requirement-to-string
		    (library-replaces library))))
  )

(defun print-library-to-string (library)
  (with-output-to-string (s)
    (print-library library s)))

(defmethod print-object ((library library) stream)
  (print-unreadable-object (library stream :type t :identity t)
    (print-library library stream)))

(defmethod library-matches ((library library) (requirement requirement))
  "Checks whether the candidate library matches the requirement, either directly or through provides.

  Returns: - :match-name if only the name matches
           - :match if both name and version match
           - :match-provide if the match is through the library provides
           - nil if there's no match"
  (let ((library-requirement (read-requirement-from-library-string
			      (library-unique-name library))))
    (if (equalp (requirement-name requirement)
		(library-name library))
      	(if (or (requirement-universal-p requirement)
		(requirement-matches library-requirement requirement))
	    (values :match nil)
					;else
	    (values :match-name (requirement-name requirement)))
					;else
	(progn
	  ;; look for providers
	  (loop for provide in (library-provides library)
	     when (requirement-matches requirement provide)
	     do (return-from library-matches
		  (values :match-provide provide)))

	  ;; look for replaces
	  (loop for replace in (library-replaces library)
	     when (requirement-matches requirement replace)
	     do (return-from library-matches
		  (values :match-replace replace)))))))

(defun library= (lib1 lib2)
  (and (equalp (library-unique-name lib1)
	       (library-unique-name lib2))
       (set-equal (library-dependencies lib1)
		  (library-dependencies lib2)
		  :test #'requirement=)
       (set-equal (library-provides lib1)
		  (library-provides lib2)
		  :test #'requirement=)
       (set-equal (library-suggests lib1)
		  (library-suggests lib2)
		  :test #'requirement=)
       (set-equal (library-replaces lib1)
		  (library-replaces lib2)
		  :test #'requirement=)
       (set-equal (library-conflicts lib1)
		  (library-conflicts lib2)
		  :test #'requirement=)))

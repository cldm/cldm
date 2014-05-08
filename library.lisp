(in-package :cldm)

(defclass library ()
  ((id :initarg :id
       :initform -1
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
	       :initform (error "Provide the library repository")
	       :accessor library-repository
	       :type repository
	       :documentation "The library repository"))

  (:documentation "library-info instances contain all the metadata needed for dependency management"))

(defun parse-library-string (string))

(defun library-unique-name (library)
  (format nil "~A - ~A"
	  (library-name library)
	  (library-version library)))

(defmethod describe-object ((library library) stream)
  (format stream "~A~%~%" (library-unique-name library))
  (format stream "Dependencies: ~{~a~^, ~}~%"
	  (mapcar #'requirement-string
		  (library-dependencies library)))
  (format stream "Provides: ~{~a~^, ~}~%"
	  (mapcar #'requirement-string
		  (library-provides library)))
  (format stream "Conflicts: ~{~a~^, ~}~%"
	  (mapcar #'requirement-string
		  (library-dependencies library)))
  (format stream "Replaces: ~{~a~^, ~}~%"
	  (mapcar #'requirement-string
		  (library-replaces library)))
  (format stream "Suggests: ~{~a~^, ~}~%"
	  (mapcar #'requirement-string
		  (library-suggests library))))

(defmethod library-matches ((library library) (requirement requirement))
  "Checks whether the candidate library matches the requirement, either directly or through provides.

  Returns: - :match-name if only the name matches
           - :match if both name and version match
           - :match-provide if the match is through the library provides
           - nil if there's no match"
  (let ((library-requirement (read-requirement-from-string
			      (library-unique-name library))))
    (if (equalp (requirement-name requirement)
		(library-name library))
      	(if (or (universal-p requirement)
		(requirement-matches library-requirement requirement))
	    :match
					;else
	    :match-name)
					;else
	(progn
	  ;; look for providers
	  (loop for provide in (library-provides library)
	     when (requirement-matches requirement provide)
	     do (return-from library-matches :match-provide))

	  ;; look for replaces
	  (loop for replace in (library-replaces library)
	     when (requirement-matches requirement replace)
	     do (return-from library-matches :match-replace))))))

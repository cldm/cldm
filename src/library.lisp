(in-package :cldm)

(defparameter *libraries* (make-hash-table :test #'equalp))

(defun find-library (name &optional (error-p t))
  (or (gethash name *libraries*)
      (when error-p
        (error "Library ~A not found" name))))

(defun find-library-versions (library requirement)
  (loop for library-version in (library-versions library)
       when (library-version-matches library-version requirement)
       collect library-version))

(defun list-libraries ()
  (loop for library being the hash-values of *libraries*
     collect library))

(defun register-library (library)
  (setf (gethash (library-name library) *libraries*) library))

(defun find-library-version (library version &optional (error-p t))
  (loop for library-version in (library-versions library)
     when (equalp (version library-version) version)
     do (return-from find-library-version library-version))
  (when error-p
    (error "~A version ~A not found" library version)))

(defclass library ()
  ((name :initarg :name
         :initform (error "Provide the library name")
         :accessor library-name
         :documentation "The library name")
   (author :initarg :author
           :initform nil
           :accessor library-author
           :documentation "The library author")
   (maintainer :initarg :maintainer
               :initform nil
               :accessor library-maintainer
               :documentation "The library maintainer")
   (description :initarg :description
                :initform nil
                :accessor library-description
                :documentation "The library description")
   (licence :initarg :licence
            :initform nil
            :accessor library-licence
            :documentation "The library licence")
   (cld :initarg :cld
        :initform (error "Provide the cld")
        :accessor library-cld
        :documentation "The library meta description address. Can be a pathname or an url")
   (versions :initarg :versions
             :initform (error "Provide a library version at least")
             :accessor library-versions
             :documentation "The library versions")
   (tags :initarg :tags
         :initform nil
         :accessor library-tags
         :documentation "Library tags"))
  (:documentation "A library meta description"))

(defmethod print-object ((library library) stream)
  (print-unreadable-object (library stream :type t :identity t)
    (format stream "~A (~A)"
            (library-name library)
            (library-cld library))))

(defmethod initialize-instance :after ((library library) &rest initargs)
  (declare (ignore initargs))

  ;; Assign the library to the versions
  (loop for version in (library-versions library)
     do (setf (library version) library))

  ;; Register the library
  (register-library library))

(defclass library-version ()
  ((library :initarg :library
            :initform nil
            :accessor library
            :documentation "The library")
   (version :initarg :version
            :initform (error "Provide the version")
            :accessor version
            :documentation "The library version")
   (description :initarg :description
                :initform nil
                :accessor description
                :documentation "Library version description")
   (stability :initarg :stability
              :initform nil
              :accessor stability
              :documentation "Library version stability. One of :stable, :beta, :alpha")
   (repositories :initarg :repositories
                 :initform (error "Provide a repository at least")
                 :accessor repositories
                 :documentation "Library version repositories")
   (dependencies :initarg :dependencies
                 :initform nil
                 :accessor dependencies
                 :documentation "The library version dependencies (list of requirement objects)")
   (provides :initarg :provides
	     :initform nil
	     :accessor provides
	     :documentation "List of requirements the library provides")
   (conflicts :initarg :conflicts
	      :initform nil
	      :accessor conflicts
	      :documentation "List of requirements the library is in conflict with")
   (replaces :initarg :replaces
	     :initform nil
	     :accessor replaces
	     :documentation "List of requirements the library replaces")
   (suggests :initarg :suggests
	     :initform nil
	     :accessor suggests
	     :documentation "List of requirements the library suggests")
   )
  (:documentation "A library version description"))

(defmethod initialize-instance :after ((library-version library-version) &rest initargs)
  (declare (ignore initargs))

  ;; Validate the version has a repository at least
  (assert (plusp (length (repositories library-version)))
          nil
          "~A version needs to define a repository at least" (version library-version))

  ;; Assign the version to the repositories
  (loop for repository in (repositories library-version)
     do (setf (library-version repository) library-version)))

(defmethod library-name ((library-version library-version))
  (library-name (library library-version)))

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

(defun read-library-version-from-string (string)
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

(defun library-version-unique-name (library-version)
  (format nil "~A~@[-~A~]"
	  (library-name library-version)
	  (when (version library-version)
	    (print-version-to-string (version library-version)))))

(defun valid-library-name-p (string)
  (not (null
	(ignore-errors
	  (parse 'library-unique-name string)))))

(defmethod describe-object ((library-version library-version) stream)
  (format stream "~A library~%~%" (library-version-unique-name library-version))
  (format stream "Dependencies: ~{~a~^, ~}~%"
	  (or
	   (mapcar #'print-requirement-to-string
		   (dependencies library-version))
	   (list "None")))
  (format stream "Provides: ~{~a~^, ~}~%"
	  (or
	   (mapcar #'print-requirement-to-string
		   (provides library-version))
	   (list "None")))
  (format stream "Conflicts: ~{~a~^, ~}~%"
	  (or
	   (mapcar #'print-requirement-to-string
		   (conflicts library-version))
	   (list "None")))
  (format stream "Replaces: ~{~a~^, ~}~%"
	  (or
	   (mapcar #'print-requirement-to-string
		   (replaces library-version))
	   (list "None")))
  (format stream "Suggests: ~{~a~^, ~}~%"
	  (or
	   (mapcar #'print-requirement-to-string
		   (suggests library-version))
	   (list "None"))))

(defun print-library-version (library-version stream)
  (format stream "~A" (library-version-unique-name library-version))
  (when (dependencies library-version)
    (format stream "; depends (~{~a~^, ~})"
	    (mapcar #'print-requirement-to-string
		    (dependencies library-version))))
  (when (provides library-version)
    (format stream "; provides (~{~a~^, ~})"
	    (mapcar #'print-requirement-to-string
		    (provides library-version))))
  (when (replaces library-version)
    (format stream "; replaces (~{~a~^, ~})"
	    (mapcar #'print-requirement-to-string
		    (replaces library-version)))))

(defun print-library-version-to-string (library-version)
  (with-output-to-string (s)
    (print-library-version library-version s)))

(defmethod print-object ((library-version library-version) stream)
  (print-unreadable-object (library-version stream :type t :identity t)
    (print-library-version library-version stream)))

(defmethod library-version-matches ((library-version library-version) (requirement requirement))
  "Checks whether the candidate library-version matches the requirement, either directly or through provides.

  Returns: - :match-name if only the name matches
           - :match if both name and version match
           - :match-provide if the match is through the library-version provides
           - nil if there's no match"
  (let ((library-version-requirement (read-requirement-from-library-version-string
				      (library-version-unique-name library-version))))
    (if (equalp (library-name requirement)
		(library-name library-version))
      	(if (or (requirement-universal-p requirement)
		(requirement-matches library-version-requirement requirement))
	    (values :match nil)
					;else
	    (values :match-name (library-name requirement)))
					;else
	(progn
	  ;; look for providers
	  (loop for provide in (provides library-version)
	     when (requirement-matches requirement provide)
	     do (return-from library-version-matches
		  (values :match-provide provide)))

	  ;; look for replaces
	  (loop for replace in (replaces library-version)
	     when (requirement-matches requirement replace)
	     do (return-from library-version-matches
		  (values :match-replace replace)))))))

(defun library-version= (lib1 lib2)
  (and (equalp (library-version-unique-name lib1)
	       (library-version-unique-name lib2))
       (set-equal (dependencies lib1)
		  (dependencies lib2)
		  :test #'requirement=)
       (set-equal (provides lib1)
		  (provides lib2)
		  :test #'requirement=)
       (set-equal (suggests lib1)
		  (suggests lib2)
		  :test #'requirement=)
       (set-equal (replaces lib1)
		  (replaces lib2)
		  :test #'requirement=)
       (set-equal (conflicts lib1)
		  (conflicts lib2)
		  :test #'requirement=)))

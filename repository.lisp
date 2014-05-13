(in-package :cldm)

(defclass repository ()
  ((name :initarg :name
	 :initform (error "Provide the repository name")
	 :accessor repository-name
	 :documentation "The repository name")
   (libraries :initarg :libraries
	      :initform nil
	      :accessor repository-libraries
	      :documentation "The repository libraries list"))
  (:documentation "A repository is a container of libraries"))

(defmethod find-library ((repository repository) name version)
  "Find library with the given name and version (exact match)"
  (let ((lib (make-instance 'library
			    :name name
			    :version version)))
    (loop for library in (repository-libraries repository)
       when (equalp (library-unique-name library)
		    (library-unique-name lib))
	 do (return-from find-library library))))

(defmethod find-libraries ((repository repository) name)
  "Returns a list of libraries with the given name"
  (loop for library in (repository-libraries repository)
     when (equalp (library-name library) name)
     return library))

(defmethod has-library-named ((repository repository) name)
  (not (null (find-libraries repository name))))

(defmethod has-library ((repository repository) library)
  (find-library repository
		(library-name library)
		(library-version library)))

(defmethod repository-length ((repository repository))
  (length (repository-libraries repository)))

(defmethod print-object ((repository repository) stream)
  (print-unreadable-object (repository stream :type t :identity t)
    (format stream "~A (~A libraries)"
	    (repository-name repository)
	    (repository-length repository))))

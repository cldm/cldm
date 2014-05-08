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
  (loop for library in (repository-libraries repository)
       when (and (equalp (library-name library) name)
		 (version= (library-version library) version))
       return library))

(defmethod find-libraries ((repository repository) name)
  "Returns a list of libraries with the given name"
  (loop for library in (repository-libraries repository)
     when (equalp (library-name library) name)
     return library))

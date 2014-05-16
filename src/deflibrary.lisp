(in-package :cldm)

(unless *load-truename*
  (error "This file must be LOADed to set up CLDM."))

(defvar *cldm-home*
  (make-pathname :name nil :type nil
                 :defaults *load-truename*))

(defparameter *cld-libraries* (make-hash-table :test #'equalp))

(defun find-cld-library (name)
  (gethash name *cld-libraries*))

(defun list-cld-libraries ()
  (loop for library being the hash-values of *cld-libraries*
       collect library))

(defun register-cld-library (library)
  (setf (gethash (library-name library) *cld-libraries*) library))

(defclass cld-library ()
  ((name :initarg :name
	 :initform (error "Provide the library name")
	 :accessor library-name
	 :documentation "The library name")
   (author :initarg :author
	   :initform nil
	   :accessor author
	   :documentation "The library author")
   (description :initarg :description
		:initform nil
		:accessor description
		:documentation "The library description")
   (cld :initarg :cld
	:initform (error "Provide the cld")
	:accessor cld
	:documentation "The library meta description address. Can be a pathname or an url")
   (versions :initarg :versions
	     :initform (error "Provide a library version at least")
	     :accessor versions
	     :documentation "The library versions"))
  (:documentation "A library meta description"))

(defmethod print-object ((library cld-library) stream)
  (print-unreadable-object (library stream :type t :identity t)
    (format stream "~A" (library-name library))))

(defmethod initialize-instance :after ((library cld-library) &rest initargs)
  (declare (ignore initargs))

  ;; Assign the library to the versions
  (loop for version in (versions library)
       do (setf (library version) library))

  ;; Register the library
  (register-cld-library library))

(defclass cld-library-version ()
  ((library :initarg :library
	    :initform nil
	    :accessor library
	    :documentation "The cld-library")
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
		 :documentation "The library version dependencies"))
  (:documentation "A library version description"))

(defun print-library-version (library-version stream)
  (format stream "~A-~A~@[ (~A)~]"
	  (library-name (library library-version))
	  (version library-version)
	  (stability library-version)))

(defmethod print-object ((library-version cld-library-version) stream)
  (print-unreadable-object (library-version stream :type t :identity t)
    (print-library-version library-version stream)))

(defclass cld-library-version-repository ()
  ((library-version :initarg :library-version
		    :initform nil
		    :accessor library-version
		    :documentation "The library version of the repository")
   (name :initarg :name
	 :initform (error "Provide the repository name")
	 :accessor name
	 :documentation "The repository name")
   (address :initarg :address
	    :initform (error "Provide the repository address")
	    :accessor address
	    :documentation "The repository address. Can be a pathname, an url or a git reference"))
  (:documentation "A library version repository"))

(defmethod print-object ((version-repository cld-library-version-repository) stream)
  (print-unreadable-object (version-repository stream :type t :identity t)
    (print-library-version (library-version version-repository)
			   stream)
    (format stream " ~A ~A"
	    (name version-repository)
	    (address version-repository))))	    

(defclass cld-library-version-dependency ()
  ((library-name :initarg :library-name
		 :initform (error "Provide the dependency library name")
		 :accessor library-name
		 :documentation "The library name")
   (library-version :initarg :version
		    :initform nil
		    :accessor library-version
		    :documentation "The library version")
   (cld :initarg :cld
	:initform nil
	:accessor cld
	:documentation "The version meta information address. Can be a pathname or url"))
  (:documentation "A library version dependency"))

(defmethod initialize-instance :after ((version-dependency cld-library-version-dependency) &rest initargs)
  (declare (ignore initargs))
  (if (and (library-version version-dependency)
	   (not (cld version-dependency)))
      (error "Provide a cld for ~A" version-dependency)))

(defmethod print-object ((version-dependency cld-library-version-dependency) stream)
  (print-unreadable-object (version-dependency stream :type t :identity t)
    (format stream "~A-~A (~A)"
	    (library-name version-dependency)
	    (library-version version-dependency)
	    (cld version-dependency))))	    

(defmacro deflibrary (name &body options)
  (destructuring-bind (&key author description
			    cld versions)
      options
    `(make-instance 'cld-library
		    :name ',(if (symbolp name)
				(string-downcase (symbol-name name))
				name)
		    :author ,author
		    :description ,description
		    :cld ,cld
		    :versions ,(parse-cld-library-versions versions))))

(defun parse-cld-library-versions (versions)
  `(list
    ,@(loop for version in versions
	 collect
	   (destructuring-bind (version-keyword version &key stability description repositories depends-on)
	       version
	     (assert (equalp version-keyword :version) nil "Invalid token ~A" version-keyword)
	     `(make-instance 'cld-library-version
			     :version ,version
			     :stability ,stability
			     :description ,description
			     :repositories ,(parse-version-repositories repositories)
			     :dependencies ,(parse-version-dependencies depends-on))))))

(defun parse-version-repositories (repositories)
  `(list
    ,@(loop for repository in repositories
	   collect
	   (destructuring-bind (name address) repository
	     `(make-instance 'cld-library-version-repository
			     :name ,name
			     :address ,(parse-version-repository-address address))))))

(defun parse-version-dependencies (dependencies)
  `(list
    ,@(loop for dependency in dependencies
	   collect
	   (if (listp dependency)
	       (destructuring-bind (library-name &key version cld) dependency
		 `(make-instance 'cld-library-version-dependency
				 :library-name ,library-name
				 :version ,version
				 :cld ,cld))
	       ;else
	       `(make-instance 'cld-library-version-dependency
			       :library-name ,dependency)))))

(defclass repository-address ()
  ()
  (:documentation "Repository address"))

(defclass directory-repository-address (repository-address)
  ((directory :initarg :directory
	     :initform (error "Provide the filesystem directory")
	     :accessor repository-directory
	     :documentation "The repository filesystem directory"))
  (:documentation "A repository in a file system directory"))

(defclass git-repository-address (repository-address)
  ((url :initarg :url
	:initform (error "Provide the git url")
	:accessor url
	:documentation "The git url")
   (branch :initarg :branch
	   :initform "master"
	   :accessor branch
	   :documentation "Git repository branch")
   (commit :initarg :commit
	   :initform nil
	   :accessor commit
	   :documentation "Commit to fetch"))
  (:documentation "A git repository"))

(defclass url-repository-address (repository-address)
  ((url :initarg :url
	:initform (error "Provide the repository package url")
	:accessor url
	:documentation "The repository url"))
  (:documentation "A remote repository package"))

(defun parse-version-repository-address (address)
  (cond
    ((pathnamep address)
     `(make-instance 'directory-repository-address
		     :directory ,address))
    ((and (listp address)
	  (equalp (first address) :directory))
     `(make-instance 'directory-repository-address
		     :directory ,(second address)))
    ((and (listp address)
	  (equalp (first address) :git))
     (destructuring-bind (url &rest args)
	 (rest address)
       `(make-instance 'git-repository-address
		       :url ,url
		       ,@args)))
    ((and (listp address)
	  (equalp (first address)
		  :url))
     `(make-instance 'url-repository-address
		     :url ,(second address)))
    (t (error "Invalid repository ~A" address))))

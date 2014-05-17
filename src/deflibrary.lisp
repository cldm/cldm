(in-package :cldm)

;; (unless *load-truename*
;;   (error "This file must be LOADed to set up CLDM."))

;; (defvar *cldm-home*
;;   (make-pathname :name nil :type nil
;;                  :defaults *load-truename*))

(defparameter *cld-libraries* (make-hash-table :test #'equalp))

(defun find-cld-library (name &optional (error-p t))
  (or (gethash name *cld-libraries*)
      (when error-p
	(error "Library ~A not found" name))))

(defun list-cld-libraries ()
  (loop for library being the hash-values of *cld-libraries*
       collect library))

(defun register-cld-library (library)
  (setf (gethash (library-name library) *cld-libraries*) library))

(defun find-cld-library-version (library version &optional (error-p t))
  (loop for version in (library-versions library)
       when (equalp (version version) version)
       do (return-from find-cld-library-version version))
  (when error-p
    (error "~A version ~A not found" library version)))

(defclass cld-library ()
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
   (license :initarg :license
	    :initform nil
	    :accessor library-license
	    :documentation "The library license")
   (cld :initarg :cld
	:initform (error "Provide the cld")
	:accessor library-cld
	:documentation "The library meta description address. Can be a pathname or an url")
   (versions :initarg :versions
	     :initform (error "Provide a library version at least")
	     :accessor library-versions
	     :documentation "The library versions"))
  (:documentation "A library meta description"))

(defmethod print-object ((library cld-library) stream)
  (print-unreadable-object (library stream :type t :identity t)
    (format stream "~A (~A)"
	    (library-name library)
	    (library-cld library))))

(defmethod initialize-instance :after ((library cld-library) &rest initargs)
  (declare (ignore initargs))

  ;; Assign the library to the versions
  (loop for version in (library-versions library)
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
	    :accessor repository-address
	    :documentation "The repository address. Can be a pathname, an url or a git reference"))
  (:documentation "A library version repository"))

(defmethod print-object ((version-repository cld-library-version-repository) stream)
  (print-unreadable-object (version-repository stream :type t :identity t)
    (print-library-version (library-version version-repository)
			   stream)
    (format stream " ~A ~A"
	    (name version-repository)
	    (repository-address version-repository))))

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
  (destructuring-bind (&key author maintainer description
			    license cld versions)
      options
    `(make-instance 'cld-library
		    :name ',(if (symbolp name)
				(string-downcase (symbol-name name))
				name)
		    :author ,author
		    :maintainer ,maintainer
		    :description ,description
		    :license ,license
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
				 :library-name ',(if (symbolp library-name)
						     (string-downcase (symbol-name library-name))
						     library-name)
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

(defun setup (library-name &optional version)
  (let ((library (find-cld-library library-name)))
    (format t "Loading ~A.~%" library)
    (let ((library-version (if version
			       (find-cld-library-version library version)
			       (first (library-versions library)))))
      ;; Load libraries metadata
      (load-library-version library-version)

      ;; Calculate list of library-versions to load
      (let ((library-versions
	     (calculate-library-versions library-version)))

	;; Validate the library versions list
	(validate-library-versions-list library-versions)

	;; Check the version existance and download if not
	;; After that, push to asdf:*central-registry*
	(loop for version in library-versions
	   do
	     (let ((pathname (cache-library-version library-version)))
	       (push pathname asdf:*central-registry*))))))
  (format t "Done.~%"))

(defun load-library-version (library-version)
  (format t "Loading ~A.~%" library-version)
  (loop for dependency in (dependencies library-version)
     do (progn
	  (format t "Handling ~A.~%" dependency)
	  (if (find-cld-library (library-name dependency) nil)
	      (progn
		(format t "Metadata for ~A is already loaded~%" (library-name dependency)))
	      ;; else
	      (let ((cld (cld dependency)))
		(if (not cld)
		    (format t "Not metadata to load.")
					;else
		    (progn
		      (format t "Loading metadata from ~A~%" cld)
		      (load-cld cld)
		      (let ((library (find-cld-library (library-name dependency))))
			(let ((library-version (find-cld-library-version library
									 (library-version dependency))))
			  (load-library-version library-version))))))))))


(defun calculate-library-versions (library-version &optional visited)
  (loop for dependency in (library-dependencies library-version)
       appending
       (if (find (library-name dependency) visited
		 :key #'library-name
		 :test #'equalp)
	   (error "Cyclic dependency on ~A" dependency)
					;else
	   (let ((library (find-cld-library (library-name dependency))))
	     (let ((library-version
		    (find-cld-library-version
		     library
		     (library-version dependency))))
	       (calculate-library-versions library-version (cons dependency visited)))))))

(defun validate-library-versions-list (versions-list)
  (loop for i from 0 to (1- (length versions-list))
       for vi = (elt versions-list i)
       do
       (loop for j from 1 to (1- (length versions-list))
	    for vj = (elt versions-list j)
	    when (and (not (equalp i j))
		      (and (equalp (library-name (library vi))
				   (library-name (library vj)))
			   (and (version vi)
				(version vj)
				(not (equalp (version vi)
					     (version vj))))))
	    do (error "Cannot load ~A and ~A" vi vj))))

(defparameter *repositories-directory*
  (asdf:system-relative-pathname :cldm "cache/repositories"))

(defun cache-library-version (library-version)
  (ensure-directories-exist *repositories-directory*)
  (let ((repository-name (format nil "~A-~A"
				 (library-name (library library-version))
				 (version library-version))))
    (let ((repository-directory (merge-pathnames
				 (pathname (format nil "~A/" repository-name))
				 *repositories-directory*)))
      (if (probe-file repository-directory)
	  (format t "Repository for ~A already exists in ~A"
		  library-version
		  repository-directory)
					;else
	  (progn
	    (format t "Repository does not exist for ~A. Caching..." library-version)
	    (let ((done nil))
	      (loop for repository in (repositories library-version)
		 while (not done)
		 do (setf done (cache-repository repository repository-directory)))
	      (if (not done)
		  (error "Couldn't cache repository from ~{~A~}" (repositories library-version))
					;else
		  (return-from cache-library-version repository-directory))))))))

(defmethod cache-repository (repository directory)
  (multiple-value-bind (pathspec created-p)
      (ensure-directories-exist directory)
    (when (not created-p)
      (error "Couldn't create directory ~A" directory))

    (cache-repository-from-address (repository-address repository)
				   repository directory)))

(defmethod cache-repository-from-address (repository-address repository directory)
  nil)

(defmethod cache-repository-from-address ((repository-address directory-repository-address)
					  repository directory)
  (multiple-value-bind (result code)
      (external-program:run "ln"
			    (list "-s"
				  (princ-to-string (repository-directory repository-address)) ;; target directory
				  (princ-to-string directory)))
    (and (equalp result :exited)
	 (zerop code))))

(defun load-cld (pathname)
  (load pathname))

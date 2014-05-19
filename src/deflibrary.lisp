(in-package :cldm)

;; (unless *load-truename*
;;   (error "This file must be LOADed to set up CLDM."))

;; (defvar *cldm-home*
;;   (make-pathname :name nil :type nil
;;                  :defaults *load-truename*))

(defparameter *cld-libraries* (make-hash-table :test #'equalp))

(defparameter *repositories-directory*
  (asdf:system-relative-pathname :cldm "cache/repositories/"))

(defparameter *address-cache-operation* :symlink "What to do when caching a local file system directory. Can be either :symlink or :copy (copy the directory recursively). Default is :symlink")

(defparameter *github-cld-repository*
  `(make-instance 'http-cld-repository
		  :name "Github repository"
		  :url "http://mmontone.github.io/cldm-repo/cld"))

(defparameter *cld-repositories* (list *github-cld-repository*))

(defun call-with-repositories-directory (pathname function)
  (let ((*repositories-directory* pathname))
    (funcall function)))

(defmacro with-repositories-directory (pathname &body body)
  `(call-with-repositories-directory
    ,pathname
    (lambda ()
      ,@body)))

(defun call-with-cld-repositories (repositories function)
  (let ((*cld-repositories* repositories))
    (funcall function)))

(defmacro with-cld-repositories (repositories &body body)
  `(call-with-cld-repositories
    (list ,@repositories)
    (lambda ()
      ,@body)))

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
  (loop for library-version in (library-versions library)
       when (equalp (version library-version) version)
       do (return-from find-cld-library-version library-version))
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
	     :documentation "The library versions")
   (tags :initarg :tags
	 :initform nil
	 :accessor library-tags
	 :documentation "Library tags"))
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

(defmethod initialize-instance :after ((library-version cld-library-version) &rest initargs)
  (declare (ignore initargs))

  ;; Validate the version has a repository at least
  (assert (plusp (length (repositories library-version)))
	  nil
	  "~A version needs to define a repository at least" (version library-version))
  
  ;; Assign the version to the repositories
  (loop for repository in (repositories library-version)
       do (setf (library-version repository) library-version)))

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
			    license cld versions tags)
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
		    :versions ,(parse-cld-library-versions versions)
		    :tags ',tags)))

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
			     ,@(let ((repositories (parse-version-repositories repositories)))
				    (when repositories
				      (list :repositories repositories)))
			     ,@(let ((dependencies (parse-version-dependencies depends-on)))
				    (when dependencies
				      (list :dependencies dependencies))))))))

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
				 ,@(when version
					 (list :version version))
				 ,@(when cld
					 (list :cld cld))))
	       ;else
	       `(make-instance 'cld-library-version-dependency
			       :library-name ,(if (symbolp dependency)
						  (string-downcase (symbol-name dependency))
						  dependency))))))

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
  "Setup an already loaded (cld) library and its dependencies"
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
	(format t "Libraries to load: ~A~%" library-versions)

	;; Validate the library versions list
	(validate-library-versions-list library-versions)

	;; Remove duplicates in depdency list
	(setf library-versions
	      (clean-library-versions-list library-versions))

	;; Check the version existance and download if not
	;; After that, push to asdf:*central-registry*
	(loop for version in (cons library-version library-versions)
	   do
	     (let ((pathname (cache-library-version version)))
	       (push pathname asdf:*central-registry*))))))
  (format t "Done.~%"))

(defun load-library (library-name &key version cld)
  "Tries to find a cld for the library and load it.
   Then setup the library and its dependencies"
  
  (if cld
      (progn
	(load-cld cld)
	(setup library-name version))
      ;; else
      (if (find-cld-library library-name nil)
	  (setup library-name version)
	  ;; else
	  (progn
	    (loop 
	       for cld-repository in *cld-repositories*
	       while (not cld)
	       do (let ((cld-repository (eval cld-repository)))
		    (setf cld (find-cld cld-repository
					library-name))
		    (when cld
		      (format t "~A cld found in ~A~%"
			      library-name
			      cld-repository))))
	    (if cld
		(progn
		  (load-cld cld)
		  (setup library-name version))
		(error "Couldn't find a cld for ~S library~%" library-name))))))

(defun load-library-version (library-version &key reload)
  (format t "Loading ~A.~%" library-version)
  (labels ((load-dependency-cld (dependency cld)
	     (format t "Loading cld: ~A~%" cld)
	     (load-cld cld)
	     (let ((library (find-cld-library (library-name dependency))))
	       (let ((library-version
		      (if (not (library-version dependency))
			  ;; No library version specified in the dependency
			  ;; Use the latest version
			  (first (library-versions library))
			  ;; else, use the library version specified
			  (find-cld-library-version
			   library
			   (library-version dependency)))))
		 (load-library-version library-version :reload reload))))
	   (load-dependency (dependency)
	     (let ((cld (cld dependency)))
	       (if (not cld)
		   (progn
		     (format t "No cld specified.~%")
		     (loop 
			for cld-repository in *cld-repositories*
			while (not cld)
			do (let ((cld-repository (eval cld-repository)))
			     (setf cld (find-cld cld-repository
						 (library-name dependency)))
			     (when cld
			       (format t "~A cld found in ~A~%"
				       (library-name dependency)
				       cld-repository))))
		     (when cld
		       (load-dependency-cld dependency cld)))
					;else
		   (load-dependency-cld dependency cld)))))
    (loop for dependency in (dependencies library-version)
       do (progn
	    (format t "Handling ~A.~%" dependency)
	    (if (find-cld-library (library-name dependency) nil)
		(progn
		  (format t "Metadata for ~A is already loaded~%" (library-name dependency))
		  (when reload
		    (format t "Reloading...")
		    (load-dependency dependency)))
		;; else
		(load-dependency dependency))))))

(defun calculate-library-versions (library-version &optional visited)
  (loop for dependency in (dependencies library-version)
       appending
       (if (find (library-name dependency) visited
		 :key #'library-name
		 :test #'equalp)
	   (error "Cyclic dependency on ~A" dependency)
					;else
	   (let ((library (find-cld-library (library-name dependency) nil)))
	     (if library
		 (let ((library-version
			(if (library-version dependency)
			    (find-cld-library-version
			     library
			     (library-version dependency))
			    ; else, use the latest version
			    (first (library-versions library)))))
		       (cons library-version
			     (calculate-library-versions library-version (cons dependency visited))))
					;else
		 (format t "WARNING: no ASDF system is being loaded by CLDM for ~A~%"
			 dependency))))))

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

(defun clean-library-versions-list (versions-list)
  ;; TODO
  versions-list
  )

(defun cache-library-version (library-version)
  (ensure-directories-exist *repositories-directory*)
  (let ((repository-name (format nil "~A-~A"
				 (library-name (library library-version))
				 (version library-version))))
    (let ((repository-directory (merge-pathnames
				 (pathname (format nil "~A/" repository-name))
				 *repositories-directory*)))
      (format t "Repository directory: ~A~%" repository-directory)
      (if (probe-file repository-directory)
	  (format t "Repository for ~A already exists in ~A~%"
		  library-version
		  repository-directory)
					;else
	  (progn
	    (format t "Repository does not exist for ~A. Caching...~%" library-version)
	    (let ((done nil))
	      (loop for repository in (repositories library-version)
		 while (not done)
		 do (progn
		      (format t "Trying with ~A..." repository)
		      (setf done (cache-repository repository repository-directory))
		      (if (not done)
			  (format t "Failed.~%")
			  (format t "Success.~%"))))
	      (when (not done)
		(error "Couldn't cache repository from ~{~A~}~%"
		       (repositories library-version))))))
      repository-directory)))

(defmethod cache-repository (repository directory)
  (cache-repository-from-address (repository-address repository)
				 repository directory))

(defun symlink (target linkname)
  (trivial-shell:shell-command
   (format nil "ln -s ~A ~A"
	   (princ-to-string target) ;; target directory
	   (princ-to-string linkname))))

(defun copy-directory (from to)
  (trivial-shell:shell-command
   (format nil "cp -r ~A ~A"
	   (princ-to-string from)
	   (princ-to-string to))))

(defgeneric cache-repository-from-address (repository-address repository target-directory)
  (:documentation "Cache the given repository from repository-address to target-directory.
                   Methods of this generic function should return T on success, or NIL on failure."))  

(defmethod cache-repository-from-address ((repository-address directory-repository-address)
					  repository target-directory)
  (multiple-value-bind (result code status)
      (ecase *address-cache-operation*
	(:symlink
	 (let ((linkname (subseq (princ-to-string target-directory)
				 0
				 (1- (length (princ-to-string target-directory))))))
	   (format t "Symlinking ~A to ~A~%"
		   (repository-directory repository-address)
		   linkname)
	   (symlink (repository-directory repository-address)
		    linkname)))
	(:copy
	 (format t "Copying directory ~A to ~A~%"
		 (repository-directory repository-address)
		 target-directory)
	 (copy-directory (repository-directory repository-address)
			 target-directory)))
    (declare (ignore result))
    (zerop status)))

(defmethod cache-repository-from-address ((repository-address url-repository-address)
					  repository target-directory)
  (flet ((run-or-fail (&rest args)
	   (multiple-value-bind (result code status)
	       (apply #'trivial-shell:shell-command args)
	     (when (not (zerop status))
	       (return-from cache-repository-from-address nil)))))
    (let ((temporal-file
	   (merge-pathnames
	    (file-namestring (url repository-address))
	    #p"/tmp/")))
      (format t "Downloading ~A...~%" (url repository-address))
      (run-or-fail (format nil "wget -O ~A ~A" temporal-file (url repository-address)))
      (format t "Extracting...~%")
      (run-or-fail (format nil "mkdir ~A" (princ-to-string target-directory)))
      (run-or-fail (format nil "tar zxf ~A --strip=1 -C ~A"
			   temporal-file
			   (princ-to-string target-directory))))
    t))

(defmethod cache-repository-from-address ((repository-address git-repository-address)
					  repository target-directory)
  (flet ((run-or-fail (command)
	   (multiple-value-bind (result code status)
	       (trivial-shell:shell-command command)
	     (declare (ignore result code))
	     (when (not (zerop status))
	       (return-from cache-repository-from-address nil)))))
    (format t "Cloning repository: ~A...~%" (url repository-address))
    (run-or-fail (format nil "git clone ~A ~A"
			 (url repository-address)
			 (princ-to-string target-directory)))
    (when (commit repository-address)
      (format t "Checking out commit ~A~%" (commit repository-address))
      (let ((command  (format nil "cd ~A; git checkout ~A"
			   (princ-to-string target-directory)
			   (commit repository-address))))
	(format t "~A~%" command)
	(run-or-fail command)))
    t))

(defclass cld-repository ()
  ((name :initarg :name
	 :accessor name
	 :initform nil
	 :documentation "The cld repository name"))
  (:documentation "A .cld files repository"))

(defclass directory-cld-repository (cld-repository)
  ((directory :initarg :directory
	     :initform (error "Provide the cld repository directory")
	     :accessor repository-directory
	     :documentation "The cld repository directory"))
  (:documentation "A local (directory) cld repository"))

(defmethod print-object ((cld-repository directory-cld-repository) stream)
  (print-unreadable-object (cld-repository stream :type t :identity t)
    (format stream "~A : ~A"
	    (name cld-repository)
	    (repository-directory cld-repository))))

(defclass http-cld-repository (cld-repository)
  ((url :initarg :url
	:initform (error "Provide the cld repository url address")
	:accessor repository-url
	:documentation "The cld repository url address"))
  (:documentation "A cld repository on http"))

(defmethod print-object ((cld-repository http-cld-repository) stream)
  (print-unreadable-object (cld-repository stream :type t :identity t)
    (format stream "~A : ~A"
	    (name cld-repository)
	    (repository-url cld-repository))))

(defclass cached-http-cld-repository (http-cld-repository)
  ((cache-directory :initarg :cache-directory
		    :initform (error "Provide the cache directory")
		    :accessor cache-directory
		    :documentation "The cache directory"))
  (:documentation "A cld repository in which a cache is maintained in a local directory"))

(defmethod find-cld ((cld-repository directory-cld-repository) library-name)
  (let ((cld-file (merge-pathnames (pathname (format nil "~A.cld" library-name))
				   (repository-directory cld-repository))))
    (format t "Checking if ~A exists~%" cld-file)
    (probe-file cld-file)))

(defmethod find-cld ((cld-repository http-cld-repository) library-name)
  (let* ((cld-url-address (format nil "~A/~A.cld"
				 (repository-url cld-repository)
				 library-name))
	 (temporal-directory #p"/tmp/")
	 (temporal-file (merge-pathnames (pathname (format nil "~A.cld" library-name))
					 temporal-directory)))
    (format t "Trying to fetch ~A~%" cld-url-address)
    (let ((command (format nil "wget -O ~A ~A"
			   temporal-file
			   cld-url-address)))
      (format t "~A~%" command)
      (multiple-value-bind (result error status)
	  (trivial-shell:shell-command command)
	(declare (ignore result error))
	(if (equalp status 0)
	    (progn
	      (format t "~A downloaded.~%" cld-url-address)
	      temporal-file)
	    ; else
	    (format t "Failed.~%"))))))

(defclass cld-address ()
  ()
  (:documentation "A cld address"))

(defclass git-cld-address (cld-address)
  ((git-url :initarg :git-url
	    :initform (error "Provide the git url")
	    :accessor git-url
	    :documentation "The git repository address. i.e. git://git.foo.com/project.git")
   (pathname :initarg :pathname
	     :initform (error "Provide the pathname to the cld file")
	     :accessor cld-pathname
	     :documentation "The pathname to the cld file"))
  (:documentation "A cld in a git repository"))

(defmethod print-object ((cld-address git-cld-address) stream)
  (print-unreadable-object (cld-address stream :type t :identity t)
    (format stream "~A:~A"
	    (git-url cld-address)
	    (cld-pathname cld-address))))

(defclass pathname-cld-address (cld-address)
  ((pathname :initarg :pathname
	     :initform (error "Provide the cld pathname")
	     :accessor cld-pathname
	     :documentation "The pathname of the cld file"))
  (:documentation "A cld in a local filesystem pathname"))

(defmethod print-object ((cld-address pathname-cld-address) stream)
  (print-unreadable-object (cld-address stream :type t :identity t)
    (format stream "~A" (cld-pathname cld-address))))

(defclass http-cld-address (cld-address)
  ((url :initarg :url
	:initform (error "Provide the cld url") 
	:accessor cld-url
	:documentation "The cld http url"))
  (:documentation "A cld in an http url"))

(defmethod print-object ((cld-address http-cld-address) stream)
  (print-unreadable-object (cld-address stream :type t :identity t)
    (format stream "~A" (cld-url cld-address))))

(defgeneric parse-cld-address (cld-address)
  (:method ((cld-address pathname))
    cld-address)
  (:method ((cld-address cld-address))
    cld-address)
  (:method ((cld-address string))
    (cond
      ((cl-ppcre:scan "^git://.*:.*$" cld-address)
       (cl-ppcre:register-groups-bind
	   (git-url pathname)
	   ("^git://(.*):(.*)$" cld-address)
	 (make-instance 'git-cld-address
			:git-url git-url
			:pathname (pathname pathname))))
      ((cl-ppcre:scan "http://.*$" cld-address)
       (make-instance 'http-cld-address :url cld-address))
      (t
       (make-instance 'pathname-cld-address :pathname (pathname cld-address)))))
  (:method ((cld-address list))
    (ecase (first cld-address)
      (:file (make-instance 'pathname-cld-address :pathname (second cld-address)))
      (:url (make-instance 'http-cld-address :url (second cld-address)))
      (:git (make-instance 'git-cld-address
			   :git-url (second cld-address)
			   :pathname (pathname (third cld-address)))))))

(defgeneric load-cld (cld-address)
  (:method :around (cld-address)
	   (let ((pathname (call-next-method)))
	     (when pathname
	       (let ((*package* (find-package :cldm)))
		 (load pathname)))))
  (:method ((cld-address pathname-cld-address))
    (cld-pathname cld-address))
  (:method ((cld-address git-cld-address))
    (let ((file-namestring (file-namestring (cld-pathname cld-address)))
	  (file-directory (let ((dir (directory-namestring (cld-pathname cld-address))))
			    (when (plusp (length dir))
			      dir))))
      (let ((command (format nil "cd /tmp; git archive --remote=~A ~@[HEAD:~A ~] ~A | tar -x"
			     (cld-url cld-address)
			     file-directory
			     file-namestring)))
	(format t command)
	(trivial-shell:shell-command command)
	(pathname (format nil "/tmp/~A" file-namestring)))))
  (:method ((cld-address http-cld-address))
    (cl-ppcre:register-groups-bind (url-path filename)
	("^(http://.*)/(.*)$" (cld-url cld-address))
      (declare (ignore url-path))
      (let* ((temporal-file (pathname (format nil "/tmp/~A" filename)))
	     (command (format nil "wget -O ~A ~A" temporal-file (cld-url cld-address))))
	(format t command)
	(trivial-shell:shell-command command)
	temporal-file)))
  (:method (cld-address)
    (parse-cld-address cld-address)))

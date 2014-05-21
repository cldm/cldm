(in-package :cldm)

(defparameter *verbose-mode* t)
(defparameter *cld-libraries* (make-hash-table :test #'equalp))

(defparameter *libraries-directory*
  (pathname "~/.cldm/cache/libraries/"))

(defparameter *address-cache-operation* :symlink "What to do when caching a local file system directory. Can be either :symlink or :copy (copy the directory recursively). Default is :symlink")

(defparameter *cldm-repo*
  `(make-instance 'cached-http-cld-repository
                  :name "cldm-repo"
                  :url "http://mmontone.github.io/cldm-repo/cld"
		  :cache-directory (pathname "~/.cldm/cache/cld-repositories/cldm-repo/")))

(defparameter *cld-repositories* (list *cldm-repo*))

(defparameter *solving-mode* :lenient "One of :strict, :lenient. If :strict, errors are signaled if a cld cannot be found, or a dependency version is not specified. If :lenient, signal warnings and try to solve dependencies loading latest versions and the like.")

(defparameter *clean-asdf-environment* nil "If T, load libraries in a clean ASDF environment")

(defun verbose-msg (msg &rest args)
  (when *verbose-mode*
    (apply #'format t (cons msg args))))

(defun call-with-libraries-directory (pathname function)
  (let ((*libraries-directory* pathname))
    (funcall function)))

(defmacro with-libraries-directory (pathname &body body)
  `(call-with-libraries-directory
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
          (if (version library-version)
	      (version library-version)
	      "<without version>")
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
            (if (library-version version-dependency)
		(library-version version-dependency)
		"<without version>")
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
                    :cld (parse-cld-address ',cld)
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
                                         (list :cld `(parse-cld-address ',cld)))))
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
           :initform nil
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
    (verbose-msg "Loading ~A.~%" library)
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

        ;; Pick library versions
        (setf library-versions
              (pick-library-versions library-versions))

	(verbose-msg "Libraries to load: ~A~%" library-versions)

        ;; Check the version existance and download if not
        ;; After that, push to asdf:*central-registry*
        (loop for version in (cons library-version library-versions)
           do
             (let ((pathname (cache-library-version version)))
               (push pathname asdf:*central-registry*))))))
  (verbose-msg "Done.~%"))

(defun load-library (library-name
		     &key
		       version
		       cld
		       (verbose *verbose-mode*)
		       (solving-mode *solving-mode*)
		       (clean-asdf-environment *clean-asdf-environment*))
  "Tries to find a cld for the library and load it.
   Then setup the library and its dependencies"
  (let ((*verbose-mode* verbose)
	(*solving-mode* solving-mode))
    (when clean-asdf-environment
      (setf asdf:*central-registry* nil)
      (asdf:clear-source-registry)
      (asdf:clear-configuration)
      (setf asdf:*system-definition-search-functions* (list 'ASDF/FIND-SYSTEM:SYSDEF-CENTRAL-REGISTRY-SEARCH)))
    (let ((cld (and cld (load-cld cld))))
      (if cld
	  (progn
	    (setup library-name version)
	    (asdf:operate 'asdf:load-op library-name)
	    t)
	  ;; else
	  (if (find-cld-library library-name nil)
	      (setup library-name version)
	      ;; else
	      (progn
		(loop
		   for cld-repository in *cld-repositories*
		   while (not cld)
		   do (let ((cld-repository (eval cld-repository)))
			(let ((repository-cld (find-cld cld-repository
							library-name)))
			  (setf cld (and repository-cld 
					 (load-cld repository-cld)))
			  (when cld
			    (verbose-msg "~A cld found in ~A~%"
					 library-name
					 cld-repository)))))
		(if cld
		    (progn
		      (setup library-name version)
		      (asdf:operate 'asdf:load-op library-name)
		      t)
		    (error "Couldn't find a cld for ~S library~%" library-name))))))))

(defun load-library-version (library-version &key reload)
  "Load a library version dependencies clds"
  (verbose-msg "Loading ~A.~%" library-version)
  (labels ((load-dependency (dependency)
	     "Load a dependency cld, and the cld of dependencies of the dependency"
	     (let ((library (find-cld-library (library-name dependency))))
               (let ((library-version
                      (if (not (library-version dependency))
                          ;; No library version specified in the dependency
                          ;; Use the latest version
			  (ecase *solving-mode*
			    (:lenient
			     (warn "Library version not specified for ~A. Calculating dependencies with the latest version!!" dependency)
			     (first (library-versions library)))
			    (:strict
			     (error "Library version not specified for ~A." dependency)))
                          ;; else, use the library version specified
                          (find-cld-library-version
                           library
                           (library-version dependency)))))
                 (load-library-version library-version :reload reload))))
           (load-dependency-cld (dependency)
	     (let ((cld (and (cld dependency)
			     (load-cld (cld dependency)))))
               (if (not cld)
                   (progn
                     (verbose-msg "No cld could be loaded.~%")
                     (loop
                        for cld-repository in *cld-repositories*
                        while (not cld)
                        do (let ((cld-repository (eval cld-repository)))
			     (let ((repository-cld (find-cld cld-repository
							     (library-name dependency))))
			       (setf cld (and repository-cld (load-cld repository-cld)))
			       (when cld
				 (verbose-msg "~A cld found in ~A~%"
					      (library-name dependency)
					      cld-repository)))))
                     (if cld
			 ;; A cld for the dependency was found, load the dependency
			 (load-dependency dependency)
			 ;; else, no dependency was found. What to do in this case??
			 ;; we can signal an error, or ignore this (signal a warning), as
			 ;; the library version may be loadable from the user system repository
			 ;; anyway (.i.e. Quicklisp)
			 (ecase *solving-mode*
			   (:lenient (warn "Couldn't find a cld for ~A" dependency))
			   (:strict (error "Couldn't find a cld for ~A" dependency)))))
                                        ;else
                   (load-dependency dependency)))))
    (loop for dependency in (dependencies library-version)
       do (progn
            (verbose-msg "Handling ~A.~%" dependency)
	    ;; For each dependency, try to load its cld, if it is not already loaded
            (if (find-cld-library (library-name dependency) nil)
                (progn
                  (verbose-msg "Metadata for ~A is already loaded~%" (library-name dependency))
                  (when reload
                    (verbose-msg "Reloading...")
                    (load-dependency-cld dependency)))
                ;; else
                (load-dependency-cld dependency))))))

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
		 (let ((dependency-library-version
			(if (library-version dependency)
			    (find-cld-library-version
                             library
                             (library-version dependency))
                            ;; else, use a generic library version
			    ;; assuming the dependencies of the latest library version
			    (let ((latest-library-version (first (library-versions library))))
			      (make-instance 'cld-library-version
					     :library library
					     :version nil ;; This is important
					     :dependencies (dependencies latest-library-version)
					     :repositories (repositories latest-library-version))))))			    
		   (cons dependency-library-version
			 (calculate-library-versions dependency-library-version
						     (cons dependency visited))))
                                        ;else
                 (ecase *solving-mode*
		   (:lenient (warn "No ASDF system is being loaded by CLDM for ~A~%"
				   dependency))
		   (:strict (error "Coudn't load ~A" dependency))))))))

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

(defun group-by (list &key
			(key #'identity)
			(test #'equal))
  (let ((groups nil))
    (loop for item in list
	 do
	 (let ((item-key (funcall key item)))
	   (block find-group
	     ;; Look for a group for the item
	     (loop for group in groups
		   for i from 0
		when (funcall test item-key (funcall key (first group)))
		do (progn
		     (setf (nth i groups) (push item group))
		     (return-from find-group)))
	     ;; Group not found for item, create one
	     (push (list item) groups))))
    (nreverse groups)))

(defun pick-library-version (library-versions)
  "Picks a version from a library list of versions"
  (let ((library-version (first library-versions)))
    (loop for lib-version in (rest library-versions)
	 do (setf library-version (best-library-version library-version lib-version)))
    library-version))

(defun best-library-version (v1 v2)
  (cond
    ((not (version v2))
     v1)
    ((not (version v1))
     v2)
    (t
     (assert (equalp (version v1) (version v2)) nil "This should not have happened")
     v1)))  

(defun pick-library-versions (versions-list)
  (flet ((pick-latest-version (library-version)
	   ;; If the library version is not specified, pick the latest version available
	   (if (not (version library-version))
	       (let ((latest-library-version
		      (first (library-versions (library library-version)))))
		 (verbose-msg "No specific library version specified for ~A. Picking latest library version: ~A~%"
			      library-version
			      latest-library-version)
		 latest-library-version)	       
	       library-version)))
    (mapcar (compose #'pick-latest-version #'pick-library-version)
	    (group-by versions-list
		      :key (compose #'library-name #'library)
		      :test #'equalp))))

(defun cache-library-version (library-version)
  (ensure-directories-exist *libraries-directory*)
  (let ((repository-name (format nil "~A-~A"
                                 (library-name (library library-version))
                                 (version library-version))))
    (let ((repository-directory (merge-pathnames
                                 (pathname (format nil "~A/" repository-name))
                                 *libraries-directory*)))
      (verbose-msg "Repository directory: ~A~%" repository-directory)
      (if (probe-file repository-directory)
          (verbose-msg "Repository for ~A already exists in ~A~%"
                       library-version
                       repository-directory)
                                        ;else
          (progn
            (verbose-msg "Repository does not exist for ~A. Caching...~%" library-version)
            (let ((done nil))
              (loop for repository in (repositories library-version)
                 while (not done)
                 do (progn
                      (verbose-msg "Trying with ~A...~%" repository)
                      (setf done (cache-repository repository repository-directory))
                      (if (not done)
                          (verbose-msg "Failed.~%")
                          (verbose-msg "Success.~%"))))
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
           (verbose-msg "Symlinking ~A to ~A~%"
                        (repository-directory repository-address)
                        linkname)
           (symlink (repository-directory repository-address)
                    linkname)))
        (:copy
         (verbose-msg "Copying directory ~A to ~A~%"
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
      (verbose-msg "Downloading ~A...~%" (url repository-address))
      (run-or-fail (format nil "wget -O ~A ~A" temporal-file (url repository-address)))
      (verbose-msg "Extracting...~%")
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
    (verbose-msg "Cloning repository: ~A...~%" (url repository-address))
    (run-or-fail (format nil "git clone ~A ~A"
                         (url repository-address)
                         (princ-to-string target-directory)))
    (when (branch repository-address)
      (verbose-msg "Checking out ~A branch.~%" (branch repository-address))
      (run-or-fail (format nil "cd ~A; git checkout ~A"
			   target-directory
			   (branch repository-address))))
    (when (commit repository-address)
      (verbose-msg "Checking out commit ~A~%" (commit repository-address))
      (let ((command  (format nil "cd ~A; git checkout ~A"
                              (princ-to-string target-directory)
                              (commit repository-address))))
        (verbose-msg "~A~%" command)
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
    (verbose-msg "Checking if ~A exists~%" cld-file)
    (probe-file cld-file)))

(defmethod find-cld ((cld-repository http-cld-repository) library-name)
  (let* ((cld-url-address (format nil "~A/~A.cld"
                                  (repository-url cld-repository)
                                  library-name))
         (temporal-directory #p"/tmp/")
         (temporal-file (merge-pathnames (pathname (format nil "~A.cld" library-name))
                                         temporal-directory)))
    (verbose-msg "Trying to fetch ~A~%" cld-url-address)
    (let ((command (format nil "wget -O ~A ~A"
                           temporal-file
                           cld-url-address)))
      (verbose-msg "~A~%" command)
      (multiple-value-bind (result error status)
          (trivial-shell:shell-command command)
        (declare (ignore result error))
        (if (equalp status 0)
            (progn
              (verbose-msg "~A downloaded.~%" cld-url-address)
              temporal-file)
                                        ; else
            (verbose-msg "Failed.~%"))))))

(defmethod find-cld :around ((cld-repository cached-http-cld-repository) library-name)
  (ensure-directories-exist (cache-directory cld-repository))
  (let ((cached-file (merge-pathnames
		      (pathname (format nil "~A.cld" library-name))
		      (cache-directory cld-repository))))
    (if (probe-file cached-file)
	cached-file
	;; else
	(let ((downloaded-file (call-next-method)))
	  (when downloaded-file
	    (let ((command (format nil "cp ~A ~A"
				   downloaded-file
				   cached-file)))
	      (verbose-msg "~A~%" command)
	      (multiple-value-bind (output error status)
		  (trivial-shell:shell-command command)
		(declare (ignore output error))
		(if (zerop status)
		    ;; success
		    cached-file
		    ;; else
		    (progn
		      (verbose-msg "Could not cache cdl file ~A~%" downloaded-file)
		      downloaded-file))))))))) 

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
             :documentation "The pathname to the cld file")
   (branch :initarg :branch
	   :initform "HEAD"
	   :accessor git-branch
	   :documentation "The git branch"))
  (:documentation "A cld in a git repository"))

(defmethod print-object ((cld-address git-cld-address) stream)
  (print-unreadable-object (cld-address stream :type t :identity t)
    (format stream "~A:~A [~A]"
            (git-url cld-address)
            (cld-pathname cld-address)
	    (git-branch cld-address))))

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
      (:git (destructuring-bind (url pathname &key branch) (rest cld-address)
	      (make-instance 'git-cld-address
			     :git-url url
			     :pathname (pathname pathname)
			     :branch (or branch "HEAD")))))))

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
      (let ((command (format nil "cd /tmp; git archive --remote=~A ~A~@[:~A~] ~A | tar -x"
                             (cl-ppcre:regex-replace-all "~" (git-url cld-address) "~~")
			     (or (git-branch cld-address) "HEAD")
                             file-directory
                             file-namestring)))
        (verbose-msg command)
	(multiple-value-bind (result error status)
	    (trivial-shell:shell-command command)
	  (declare (ignore result error))
	  (when (zerop status)
	    (pathname (format nil "/tmp/~A" file-namestring)))))))
  (:method ((cld-address http-cld-address))
    (cl-ppcre:register-groups-bind (url-path filename)
        ("^(http://.*)/(.*)$" (cld-url cld-address))
      (declare (ignore url-path))
      (let* ((temporal-file (pathname (format nil "/tmp/~A" filename)))
             (command (format nil "wget -O ~A ~A" temporal-file (cld-url cld-address))))
        (verbose-msg command)
        (trivial-shell:shell-command command)
        temporal-file)))
  (:method (cld-address)
    (parse-cld-address cld-address)))

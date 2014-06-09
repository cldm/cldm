(in-package :cldm)

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

(defclass ssh-cld-repository (cld-repository)
  ((address :initarg :address
	    :initform (error "Provide the ssh address")
	    :accessor repository-address
	    :documentation "The cld repository ssh address"))
  (:documentation "A cld repository accessible via ssh"))

(defclass cached-cld-repository (cld-repository)
  ((cache-directory :initarg :cache-directory
                    :initform (error "Provide the cache directory")
                    :accessor cache-directory
                    :documentation "The cache directory"))
  (:documentation "A cld repository in which a cache is maintained in a local directory"))

(defclass cached-http-cld-repository (http-cld-repository cached-cld-repository)
  ())

(defclass cached-ssh-cld-repository (ssh-cld-repository cached-cld-repository)
  ())

(defun find-cld-repository (name &optional (repositories *cld-repositories*))
  (find name (mapcar #'eval *cld-repositories*)
	:key #'name
	:test #'equalp))

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

(defmethod find-cld ((cld-repository ssh-cld-repository) library-name)
  (let* ((cld-address (format nil "~A/~A.cld"
			      (repository-address cld-repository)
			      library-name))
         (temporal-directory #p"/tmp/")
         (temporal-file (merge-pathnames (pathname (format nil "~A.cld" library-name))
                                         temporal-directory)))
    (verbose-msg "Trying to fetch ~A~%" cld-address)
    (let ((command (format nil "scp ~A ~A"
			   cld-address
			   temporal-file)))
      (verbose-msg "~A~%" command)
      (multiple-value-bind (result error status)
          (trivial-shell:shell-command command)
        (declare (ignore result error))
        (if (equalp status 0)
            (progn
              (verbose-msg "~A downloaded.~%" cld-address)
              temporal-file)
                                        ; else
            (verbose-msg "Failed.~%"))))))

(defmethod find-cld :around ((cld-repository cached-cld-repository) library-name)
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

(defun cache-library-version (library-version &optional (libraries-directory *libraries-directory*))
  (info-msg "Installing ~A...~%" (library-version-unique-name library-version))
  (ensure-directories-exist libraries-directory)
  (let* ((repository-name (format nil "~A-~A"
				  (library-name (library library-version))
				  (print-version-to-string (version library-version))))
	 (repository-directory (merge-pathnames
				(pathname (format nil "~A/" repository-name))
				libraries-directory))
	 (return-repository nil))
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
		    (setf return-repository repository)
		    (setf done (cache-repository repository repository-directory))
		    (if (not done)
			(verbose-msg "Failed.~%")
			(verbose-msg "Success.~%"))))
	    (when (not done)
	      (error "Couldn't cache repository from ~{~A~}~%"
		     (repositories library-version))))))
    (values repository-directory return-repository)))

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

(defclass ssh-repository-address (repository-address)
  ((address :initarg :address
	    :initform (error "Provide the repository address")
	    :accessor address
	    :documentation "The repository ssh address"))
  (:documentation "A remote ssh repository address"))

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
      (info-msg "Downloading ~A...~%" (url repository-address))
      (run-or-fail (format nil "wget -O ~A ~A" temporal-file (url repository-address)))
      (info-msg "Extracting...~%")
      (run-or-fail (format nil "mkdir ~A" (princ-to-string target-directory)))
      (run-or-fail (format nil "tar zxf ~A --strip=1 -C ~A"
                           temporal-file
                           (princ-to-string target-directory))))
    t))

(defmethod cache-repository-from-address ((repository-address ssh-repository-address)
                                          repository target-directory)
  (flet ((run-or-fail (&rest args)
           (multiple-value-bind (result code status)
               (apply #'trivial-shell:shell-command args)
             (when (not (zerop status))
               (return-from cache-repository-from-address nil)))))
    (let ((temporal-file
           (merge-pathnames
            (file-namestring (address repository-address))
            #p"/tmp/")))
      (info-msg "Downloading ~A...~%" (address repository-address))
      (run-or-fail (format nil "scp ~A ~A" (address repository-address) temporal-file))
      (info-msg "Extracting...~%")
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
    (info-msg "Cloning repository: ~A...~%" (url repository-address))
    (run-or-fail (format nil "git clone ~A ~A"
                         (url repository-address)
                         (princ-to-string target-directory)))
    (when (branch repository-address)
      (info-msg "Checking out ~A branch.~%" (branch repository-address))
      (run-or-fail (format nil "cd ~A; git checkout ~A"
			   target-directory
			   (branch repository-address))))
    (when (commit repository-address)
      (info-msg "Checking out commit ~A~%" (commit repository-address))
      (let ((command  (format nil "cd ~A; git checkout ~A"
                              (princ-to-string target-directory)
                              (commit repository-address))))
        (verbose-msg "~A~%" command)
        (run-or-fail command)))
    t))

(defgeneric repository-address-sexp (repository-address)
  (:method ((repository-address directory-repository-address))
    (list :directory (repository-directory repository-address)))
  (:method ((repository-address url-repository-address))
    (list :url (repository-url repository-address)))
  (:method ((repository-address git-repository-address))
    `(:git ,(url repository-address)
	   ,@(when (commit repository-address)
		   (list :commit (commit repository-address)))
	   ,@(when (branch repository-address)
		   (list :branch (branch repository-address)))))
  (:method ((repository-address ssh-repository-address))
    (list :ssh (address repository-address))))

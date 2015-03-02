(in-package :cldm)

(defvar *download-session-enabled-p* t)
(defvar *download-session* nil)
(defvar *download-session-failed-uris* nil)
(defvar *if-already-installed-library-version* :supersede)

(defun call-with-download-session (function)
  (let ((*download-session* t)
        (*download-session-failed-uris* nil))
    (let ((repos (list-cld-repositories)))
      (loop for repo in repos
         do (start-download-session repo))
      (unwind-protect
           (funcall function)
        (loop for repo in repos
           do (stop-download-session repo))))))

(defmacro with-download-session ((&optional (enabled-p '*download-session-enabled-p*)) &body body)
  `(when ,enabled-p
     (call-with-download-session (lambda () ,@body))))

(defclass cld-repository ()
  ((name :initarg :name
         :accessor name
         :initform nil
         :documentation "The cld repository name"))
  (:documentation "A .cld files repository"))

(defgeneric start-download-session (cld-repository)
  (:method ((cld-repository cld-repository))))

(defgeneric stop-download-session (cld-repository)
  (:method ((cld-repository cld-repository))))

(defgeneric publish-cld (cld-repository cld-pathname)
  (:method ((cld-repository cld-repository) cld-pathname)
    (error "~A repository doesn't support publishing" cld-repository)))

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

(defmethod print-object ((cld-repository ssh-cld-repository) stream)
  (print-unreadable-object (cld-repository stream :type t :identity t)
    (format stream "~A : ~A"
            (name cld-repository)
            (repository-address cld-repository))))

(defclass cached-cld-repository (cld-repository)
  ((cache-directory :initarg :cache-directory
                    :initform (error "Provide the cache directory")
                    :accessor cache-directory
                    :documentation "The cache directory"))
  (:documentation "A cld repository in which a cache is maintained in a local directory"))

(defclass cached-http-cld-repository (http-cld-repository cached-cld-repository)
  ())

(defmethod print-object ((cld-repository cached-http-cld-repository) stream)
  (print-unreadable-object (cld-repository stream :type t :identity t)
    (format stream "~A : ~A (cache: ~A)"
            (name cld-repository)
            (repository-url cld-repository)
            (cache-directory cld-repository))))

(defclass cached-ssh-cld-repository (ssh-cld-repository cached-cld-repository)
  ())

(defmethod print-object ((cld-repository cached-ssh-cld-repository) stream)
  (print-unreadable-object (cld-repository stream :type t :identity t)
    (format stream "~A : ~A (cache: ~A)"
            (name cld-repository)
            (repository-address cld-repository)
            (cache-directory cld-repository))))

(defmethod find-cld ((cld-repository directory-cld-repository) library-name)
  (let ((cld-file (merge-pathnames (pathname (format nil "~A.cld" library-name))
                                   (pathname (repository-directory cld-repository)))))
    (verbose-msg "Checking if ~A exists~%" cld-file)
    (probe-file cld-file)))

(defmethod publish-cld ((cld-repository directory-cld-repository) cld-pathname)
  (copy-file (or (probe-file cld-pathname)
                 (error "cld file does not exists ~A" cld-pathname))
             (merge-pathnames (file-namestring cld-pathname)
                              (pathname (repository-directory cld-repository)))))

(defmethod cld-url-address ((cld-repository http-cld-repository) library-name)
  (format nil "~A/~A.cld"
          (repository-url cld-repository)
          library-name))

(defmethod find-cld ((cld-repository http-cld-repository) library-name)
  (let* ((cld-url-address (cld-url-address cld-repository library-name))
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
            (progn
              (verbose-msg "Failed.~%")
              nil ;; it is important to return nil if not found
              ))))))

(defmethod find-cld :around ((cld-repository http-cld-repository) library-name)
  (if *download-session*
      ;; If we are in a download session, check that cld-url-address
      ;; has not previously failed
      (let ((cld-url-address (cld-url-address cld-repository library-name)))
        (if (member cld-url-address *download-session-failed-uris* :test #'equalp)
            ;; The uri has previously failed, do nothing
            (return-from find-cld nil)
            ;; else, the uri has not previously failed, try loading the cld from there
            (let ((cld (call-next-method)))
              (when (not cld)
                ;; the uri failed, add the uri to failed ones
                (push cld-url-address *download-session-failed-uris*))
              cld)))))

(defmethod cld-url-address ((cld-repository ssh-cld-repository) library-name)
  (format nil "~A/~A.cld"
          (repository-address cld-repository)
          library-name))

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
            (progn
              (verbose-msg "Failed.~%")
              nil ;; it is important to return nil if couldn't fetch
              ))))))

(defmethod publish-cld ((cld-repository ssh-cld-repository) cld-pathname)
  (let*  ((cld-address (format nil "~A/~A"
                               (repository-address cld-repository)
                               cld-pathname))
          (command (format nil "scp ~A ~A"
                           (or (probe-file cld-pathname)
                               (error "cld file does not exists ~A" cld-pathname))
                           cld-address)))
    (multiple-value-bind (result error status)
        (trivial-shell:shell-command command)
      (declare (ignore result error))
      (if (equalp status 0)
          (progn
            (verbose-msg "~A published.~%" cld-pathname)
            cld-pathname)
          (error "Error publishing ~A to ~A" cld-pathname cld-repository)))))

(defmethod find-cld :around ((cld-repository ssh-cld-repository) library-name)
  (if *download-session*
      ;; If we are in a download session, check that cld-url-address
      ;; has not previously failed
      (let ((cld-url-address (cld-url-address cld-repository library-name)))
        (if (member cld-url-address *download-session-failed-uris* :test #'equalp)
            ;; The uri has previously failed, do nothing
            (return-from find-cld nil)
            ;; else, the uri has not previously failed, try loading the cld from there
            (let ((cld (call-next-method)))
              (when (not cld)
                ;; the uri failed, add the uri to failed ones
                (push cld-url-address *download-session-failed-uris*))
              cld)))))

(defmethod find-cld :around ((cld-repository cached-cld-repository) library-name)
  (ensure-directories-exist (pathname (cache-directory cld-repository)))
  (let ((cached-file (merge-pathnames
                      (pathname (format nil "~A.cld" library-name))
                      (pathname (cache-directory cld-repository)))))
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

(defun remove-directory (directory)
  (multiple-value-bind (output error status)
      (trivial-shell:shell-command (format nil "rm -r ~A" directory))
    (declare (ignore output))
    (when (not (zerop status))
      (error error))))

(defmethod install-library-version ((library-version library-version)
				    &optional
				      (libraries-directory *libraries-directory*)
				      (if-installed *if-already-installed-library-version*))
  "Installs LIBRARY-VERSION to LIBRARIES-DIRECTORY.
   LIBRARIES-DIRECTORY is the root directory where the library version is to be installed.
   IF-INSTALLED controls what is done if the library is already installed. One of :supersede, :reinstall, :ignore, :error.
   Return values: if the library was installed, returns a INSTALLED-LIBRARY-VERSION object. Else, nil"

  (ensure-directories-exist libraries-directory)
  (let* ((install-directory-name (format nil "~A-~A"
                                         (library-name (library library-version))
                                         (print-version-to-string (version library-version))))
         (install-directory (merge-pathnames
                             (pathname (format nil "~A/" install-directory-name))
                             libraries-directory))
         (installed-repository nil))
    (flet ((%install-library-version ()
             (info-msg "Installing ~A...~%"
                       (library-version-unique-name library-version))
             (let ((done nil))
               (loop for repository in (repositories library-version)
                  while (not done)
                  do (progn
                       (verbose-msg "Trying with ~A...~%" repository)
                       (setf installed-repository repository)
                       (setf done (install-repository repository install-directory))
                       (if (not done)
                           (verbose-msg "Failed.~%")
                           (verbose-msg "Success.~%"))))
               (when (not done)
                 (error "Couldn't install repository from ~{~A~}~%"
                        (repositories library-version))))
             ;; Build the installed library version object to return
             (make-instance 'installed-library-version
                            :name (library-name library-version)
                            :version (version library-version)
                            :install-directory install-directory
                            :repository installed-repository))
           (%remove-installed-library-version ()
             (remove-directory install-directory)))
      (verbose-msg "Repository directory: ~A~%" install-directory)
      (if (probe-file install-directory)
          ;; If the install directory exists, we assume the library version
          ;; is already installed.
          ;; Act according to IF-INSTALLED variable
          ;; TODO: this assumption can be incorrect. How to fix?
          (progn
            (verbose-msg "Repository for ~A already exists in ~A~%"
                         library-version
                         install-directory)
            (ecase if-installed
              (:supersede
               (verbose-msg "Reinstalling ~A~%" library-version)
               (%remove-installed-library-version)
               (%install-library-version))
              (:install
               (verbose-msg "Reinstalling ~A~%" library-version)
               (%remove-installed-library-version)
               (%install-library-version))
              (:error (error "~A is already installed." library-version))
              (:ignore
               (values t install-directory))))
          ;; else, the library is not installed. Install.
          (%install-library-version)))))

(defmethod install-library-version ((ilv installed-library-version)
				    &optional
				      (libraries-directory *libraries-directory*)
				      (if-installed *if-already-installed-library-version*))
  "Installs LIBRARY-VERSION specified in lock file to LIBRARIES-DIRECTORY.
   LIBRARIES-DIRECTORY is the root directory where the library version is to be installed.
   IF-INSTALLED controls what is done if the library is already installed. One of :supersede, :reinstall, :ignore, :error.
   Return values: if the library was installed, returns a INSTALLED-LIBRARY-VERSION object. Else, nil"

  (ensure-directories-exist libraries-directory)
  (let* ((install-directory-name (format nil "~A-~A"
                                         (name ilv)
                                         (print-version-to-string (version ilv))))
         (install-directory (merge-pathnames
                             (pathname (format nil "~A/" install-directory-name))
                             libraries-directory)))
    (flet ((%install-library-version ()
	     (info-msg "Installing ~A-~A...~%"
		       (name ilv)
		       (print-version-to-string (version ilv)))
	     (when (not (install-repository (repository ilv) install-directory))
	       (error "Couldn't install from ~A~%" (repository ilv)))
	     ilv)
	   (%remove-installed-library-version ()
             (remove-directory install-directory)))
      (if (probe-file install-directory)
	  ;; If the install directory exists, we assume the library version
	  ;; is already installed.
	  ;; Act according to IF-INSTALLED variable
	  ;; TODO: this assumption can be incorrect. How to fix?
	  (progn
	    (verbose-msg "Repository for ~A already exists in ~A~%"
			 ilv
			 install-directory)
	    (ecase if-installed
	      (:supersede
	       (verbose-msg "Reinstalling ~A~%" ilv)
	       (%remove-installed-library-version)
	       (%install-library-version))
	      (:install
	       (verbose-msg "Reinstalling ~A~%" ilv)
	       (%remove-installed-library-version)
	       (%install-library-version))
	      (:error (error "~A is already installed." ilv))
	      (:ignore
	       (values t install-directory))))
	  ;; else, the library is not installed. Install.
	  (%install-library-version)))))

(defun update-library-version (library-version project)
  "Update a library version"

  (let ((installed-library-version
         (find-installed-library-version
          project
          (library-name library-version))))
    (if installed-library-version
        ;; There's a library version installed already
        (when (or (equalp library-version :max-version)
                  (version/= (version library-version)
                             (version installed-library-version)))
          ;; The update conditions are satisfied, try to update the repository
          (update-repository installed-library-version library-version))
        ;; else, the library is not installed: install the library version
        (install-library-version library-version (libraries-directory project)))))

(defun update-repository (installed-library-version library-version)
  (update-repository-from-address
   (repository-address (repository installed-library-version))
   (repository installed-library-version)
   (install-directory installed-library-version)
   library-version))

(defmethod update-repository-from-address (repository-address
                                           repository
                                           install-directory
                                           library-version)
  ;; repository-addresses are not updateable by default
  nil)

(defmethod install-repository (repository directory)
  (install-repository-from-address (repository-address repository)
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

(defmethod print-object ((repository-address directory-repository-address) stream)
  (print-unreadable-object (repository-address stream :type t :identity t)
    (format stream "~A" (repository-directory repository-address))))

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
           :documentation "Commit to fetch")
   (tag :initarg :tag
        :initform nil
        :accessor tag
        :documentation "Tag to fetch"))
  (:documentation "A git repository"))

(defmethod print-object ((repository-address git-repository-address) stream)
  (print-unreadable-object (repository-address stream :type t :identity t)
    (format stream "~A ~@[ commit: ~A~] ~@[ tag: ~A~] ~@[ branch: ~A~]"
            (url repository-address)
            (commit repository-address)
            (tag repository-address)
            (branch repository-address))))

(defclass url-repository-address (repository-address)
  ((url :initarg :url
        :initform (error "Provide the repository package url")
        :accessor url
        :documentation "The repository url"))
  (:documentation "A remote repository package"))

(defmethod print-object ((repository-address url-repository-address) stream)
  (print-unreadable-object (repository-address stream :type t :identity t)
    (format stream "~A" (url repository-address))))

(defclass ssh-repository-address (repository-address)
  ((address :initarg :address
            :initform (error "Provide the repository address")
            :accessor address
            :documentation "The repository ssh address"))
  (:documentation "A remote ssh repository address"))

(defmethod print-object ((repository-address ssh-repository-address) stream)
  (print-unreadable-object (repository-address stream :type t :identity t)
    (format stream "~A" (address repository-address))))

(defgeneric install-repository-from-address (repository-address repository target-directory)
  (:documentation "Cache the given repository from repository-address to target-directory.
                   Methods of this generic function should return T on success, or NIL on failure."))

(defmethod install-repository-from-address ((repository-address directory-repository-address)
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

(defmethod install-repository-from-address ((repository-address url-repository-address)
                                            repository target-directory)
  (flet ((run-or-fail (&rest args)
           (multiple-value-bind (result code status)
               (apply #'trivial-shell:shell-command args)
             (when (not (zerop status))
               (return-from install-repository-from-address nil)))))
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

(defmethod install-repository-from-address ((repository-address ssh-repository-address)
                                            repository target-directory)
  (flet ((run-or-fail (&rest args)
           (multiple-value-bind (result code status)
               (apply #'trivial-shell:shell-command args)
             (when (not (zerop status))
               (return-from install-repository-from-address nil)))))
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

(defmethod install-repository-from-address ((repository-address git-repository-address)
                                            repository target-directory)
  (flet ((run-or-fail (command)
           (multiple-value-bind (result code status)
               (trivial-shell:shell-command command)
             (declare (ignore result code))
             (when (not (zerop status))
               (return-from install-repository-from-address nil)))))
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
    (when (tag repository-address)
      (info-msg "Checking out tag ~A~%" (tag repository-address))
      (let ((command  (format nil "cd ~A; git checkout tags/~A"
                              (princ-to-string target-directory)
                              (tag repository-address))))
        (verbose-msg "~A~%" command)
        (run-or-fail command)))
    t))

(defmethod update-repository-from-address ((repository-address git-repository-address)
                                           repository
                                           install-directory
                                           library-version)
  (flet ((run-or-fail (&rest args)
           (multiple-value-bind (result code status)
               (apply #'trivial-shell:shell-command args)
             (declare (ignorable result code))
             (when (not (zerop status))
               (return-from update-repository-from-address (values nil nil nil))))))
    (let ((library-version-repository (find-library-version-repository
                                       library-version
                                       (name repository))))
      (when (and library-version-repository
                 (typep (repository-address library-version-repository)
                        'git-repository-address))
        (let ((library-version-repository-address (repository-address library-version-repository)))
          ;; Ok, try the update
          (info-msg "Updating ~A...~%" (library-version-unique-name library-version))
          (let ((command (format nil "cd ~A; git pull" install-directory)))
            (verbose-msg (format nil "~A~%" command))
            (run-or-fail command))
          (awhen (branch library-version-repository-address)
            (let ((command (format nil "cd ~A; git checkout ~A" install-directory it)))
              (verbose-msg (format nil "~A~%" command))
              (run-or-fail command)))
          (awhen (commit library-version-repository-address)
            (let ((command (format nil "cd ~A; git checkout ~A" install-directory it)))
              (verbose-msg (format nil "~A~%" command))
              (run-or-fail command)))
          (awhen (tag library-version-repository-address)
            (let ((command (format nil "cd ~A; git checkout tags/~A" install-directory it)))
              (verbose-msg (format nil "~A~%" command))
              (run-or-fail command)))
          ;; Return the install directory and repository
          (values t                 ;; The update was successful
                  install-directory ;; In which directory the update was made
                  library-version-repository ;; The repository from which the update was made
                  ))))))

(defun find-cld-repository (name &optional (error-p t))
  (let ((cld-repository (find name (list-cld-repositories)
                              :key #'name
                              :test #'equalp)))
    (when (and error-p (not cld-repository))
      (error "cld repository ~A not found" name))
    cld-repository))

(defgeneric repository-address-sexp (repository-address)
  (:method ((repository-address directory-repository-address))
    (list :directory (repository-directory repository-address)))
  (:method ((repository-address url-repository-address))
    (list :url (url repository-address)))
  (:method ((repository-address git-repository-address))
    `(:git ,(url repository-address)
           ,@(when (commit repository-address)
                   (list :commit (commit repository-address)))
           ,@(when (tag repository-address)
                   (list :tag (tag repository-address)))
           ,@(when (branch repository-address)
                   (list :branch (branch repository-address)))))
  (:method ((repository-address ssh-repository-address))
    (list :ssh (address repository-address))))

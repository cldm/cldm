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
  ((address :initarg :address
            :initform (error "Provide the cld repository url address")
            :accessor repository-address
            :documentation "The cld repository url address"))
  (:documentation "A cld repository on http"))

(defmethod print-object ((cld-repository http-cld-repository) stream)
  (print-unreadable-object (cld-repository stream :type t :identity t)
    (format stream "~A : ~A"
            (name cld-repository)
            (repository-address cld-repository))))

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
            (repository-address cld-repository)
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
          (repository-address cld-repository)
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
              cld)))
      ;; else, no download session
      (call-next-method)))

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
              cld)))
      ;; else, no download session
      (call-next-method)))

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
                      (verbose-msg "Could not cache cld file ~A~%" downloaded-file)
                      downloaded-file)))))))))

(defgeneric clear-cache (cld-repository)
  (:method ((cld-repository cached-cld-repository))
    (verbose-msg "Clearing ~A cache...~%" cld-repository)
    (remove-directory (pathname (cache-directory cld-repository))))
  (:method ((cld-repository cld-repository))
    (error "Cannot clear the cache of this kind of repository")))

(defun remove-directory (directory)
  (when (fad:directory-exists-p directory)
    (multiple-value-bind (output error status)
        (trivial-shell:shell-command (format nil "rm -r ~A" directory))
      (declare (ignore output))
      (when (not (zerop status))
        (error error)))))

(defmethod remove-library-version ((library-version library-version) libraries-directory)
  (let* ((install-directory-name (format nil "~A-~A"
                                         (library-name (library library-version))
                                         (print-version-to-string (version library-version))))
         (install-directory (merge-pathnames
                             (pathname (format nil "~A/" install-directory-name))
                             libraries-directory)))
    (remove-directory install-directory)))

(defun library-version-install-directory (library-version &optional
							    (libraries-directory *libraries-directory*))
    (let* ((install-directory-name (format nil "~A-~A"
                                         (library-name (library library-version))
                                         (print-version-to-string (version library-version)))))
         (merge-pathnames
	  (pathname (format nil "~A/" install-directory-name))
	  libraries-directory)))

(defun library-version-installed-p (library-version &optional
						      (libraries-directory *libraries-directory*))
  "Returns whether a library version is installed and if it is, where"
  (if (listp libraries-directory)
      (loop for dir in libraries-directory
	 do (multiple-value-bind (installed-p install-directory)
		(library-version-installed-p library-version dir)
	      (when installed-p
		(return-from library-version-installed-p
		  (values t install-directory)))))
					; else
      (let ((install-directory (library-version-install-directory library-version libraries-directory)))
	(if (probe-file install-directory)
	    (values t install-directory)))))

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
             (remove-directory install-directory)
             ))
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

(defmethod remove-library-version ((ilv installed-library-version) libraries-directory)
  (let* ((install-directory-name (format nil "~A-~A"
                                         (name ilv)
                                         (print-version-to-string (version ilv))))
         (install-directory (merge-pathnames
                             (pathname (format nil "~A/" install-directory-name))
                             libraries-directory)))
    (remove-directory install-directory)))

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
             (remove-directory install-directory)
             ))
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
        (if (or (equalp library-version :max-version)
                (version/= (version library-version)
                           (version installed-library-version)))
            ;; The update conditions are satisfied, try to update the repository
            (update-repository installed-library-version library-version)
            ;; else, the library does not need update
            installed-library-version)
        ;; else, the library is not installed: install the library version
        (install-library-version library-version (libraries-directory project)))))

(defun update-repository (installed-library-version library-version)
  (let ((updated-p
         (update-repository-from-address
          (repository-address (repository installed-library-version))
          (repository installed-library-version)
          (install-directory installed-library-version)
          library-version)))
    (when (not updated-p)
      (error "Error updating ~A to ~A"
             installed-library-version
             library-version))
    (make-instance 'installed-library-version
                   :name (library-name library-version)
                   :version (version library-version)
                   :install-directory (install-directory installed-library-version)
                   :repository (repository installed-library-version))))

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

(defgeneric update-repository-from-address (repository-address
                                            repository
                                            install-directory
                                            library-version)
  (:method ((repository-address repository-address)
            repository
            install-directory
            library-version)
    (remove-directory install-directory)
    (install-repository-from-address repository-address
                                     repository
                                     install-directory)))

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

(defgeneric install-repository-from-address (repository-address repository install-directory)
  (:documentation "Cache the given repository from repository-address to install-directory.
                   Methods of this generic function should return T on success, or NIL on failure."))

(defmethod install-repository-from-address ((repository-address directory-repository-address)
                                            repository install-directory)
  (multiple-value-bind (result code status)
      (ecase *address-cache-operation*
        (:symlink
         (let ((linkname (subseq (princ-to-string install-directory)
                                 0
                                 (1- (length (princ-to-string install-directory))))))
           (verbose-msg "Symlinking ~A to ~A~%"
                        (repository-directory repository-address)
                        linkname)
           (symlink (repository-directory repository-address)
                    linkname)))
        (:copy
         (verbose-msg "Copying directory ~A to ~A~%"
                      (repository-directory repository-address)
                      install-directory)
         (copy-directory (repository-directory repository-address)
                         install-directory)))
    (declare (ignore result))
    (zerop status)))

(defmethod install-repository-from-address ((repository-address url-repository-address)
                                            repository install-directory)
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
      (info-msg "Extracting to ~A...~%" (princ-to-string install-directory))
      (run-or-fail (format nil "mkdir ~A" (princ-to-string install-directory)))
      (run-or-fail (format nil "tar zxf ~A --strip=1 -C ~A"
                           temporal-file
                           (princ-to-string install-directory))))
    t))

(defmethod install-repository-from-address ((repository-address ssh-repository-address)
                                            repository install-directory)
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
      (info-msg "Extracting to ~A...~%" (princ-to-string install-directory))
      (run-or-fail (format nil "mkdir ~A" (princ-to-string install-directory)))
      (run-or-fail (format nil "tar zxf ~A --strip=1 -C ~A"
                           temporal-file
                           (princ-to-string install-directory))))
    t))

(defmethod install-repository-from-address ((repository-address git-repository-address)
                                            repository install-directory)
  (flet ((run-or-fail (command)
           (multiple-value-bind (result code status)
               (trivial-shell:shell-command command)
             (declare (ignore result code))
             (when (not (zerop status))
               (return-from install-repository-from-address nil)))))
    (info-msg "Cloning repository ~A to ~A...~%" 
	      (url repository-address)
	      (princ-to-string install-directory))
    (run-or-fail (format nil "git clone ~A ~A"
                         (url repository-address)
                         (princ-to-string install-directory)))
    (when (branch repository-address)
      (info-msg "Checking out ~A branch.~%" (branch repository-address))
      (run-or-fail (format nil "cd ~A; git checkout ~A"
                           install-directory
                           (branch repository-address))))
    (when (commit repository-address)
      (info-msg "Checking out commit ~A~%" (commit repository-address))
      (let ((command  (format nil "cd ~A; git checkout ~A"
                              (princ-to-string install-directory)
                              (commit repository-address))))
        (verbose-msg "~A~%" command)
        (run-or-fail command)))
    (when (tag repository-address)
      (info-msg "Checking out tag ~A~%" (tag repository-address))
      (let ((command  (format nil "cd ~A; git checkout tags/~A"
                              (princ-to-string install-directory)
                              (tag repository-address))))
        (verbose-msg "~A~%" command)
        (run-or-fail command)))
    t))

#+nil(defmethod update-repository-from-address ((repository-address git-repository-address)
                                                repository
                                                install-directory
                                                library-version)
       (flet ((run-or-fail (&rest args)
                (multiple-value-bind (result code status)
                    (apply #'trivial-shell:shell-command args)
                  (declare (ignorable result code))
                  (when (not (zerop status))
                    (verbose-msg "Command ~A failed~A" args)
                    (return-from update-repository-from-address nil)))))
         ;; Ok, try the update
         (info-msg "Updating ~A...~%" (library-version-unique-name library-version))
         (let ((command (format nil "cd ~A; git pull" install-directory)))
           (verbose-msg (format nil "~A~%" command))
           (run-or-fail command))
         (awhen (branch repository-address)
           (let ((command (format nil "cd ~A; git checkout ~A" install-directory it)))
             (verbose-msg (format nil "~A~%" command))
             (run-or-fail command)))
         (awhen (commit repository-address)
           (let ((command (format nil "cd ~A; git checkout ~A" install-directory it)))
             (verbose-msg (format nil "~A~%" command))
             (run-or-fail command)))
         (awhen (tag repository-address)
           (let ((command (format nil "cd ~A; git checkout tags/~A" install-directory it)))
             (verbose-msg (format nil "~A~%" command))
             (run-or-fail command)))
         (make-instance 'installed-library-version
                        :name (library-name library-version)
                        :version (version library-version)
                        :repository repository
                        :install-directory install-directory)))

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

;; Indexed repositories
(defvar +index-file-name+ "libraries.idx")

(defclass indexed-cld-repository (cld-repository)
  ((index-file :initarg :index-file
               :initform nil
               :accessor index-file)
   (index :initform nil
          :accessor index)
   (search-index-path :initform nil
		      :accessor search-index-path)
   (search-index :initform nil
                 :accessor search-index)))

(defmethod initialize-instance :after ((cld-repository indexed-cld-repository) &rest initargs)
  (declare (ignore initargs))
  (when (not (index-file cld-repository))
    ;; Set default index file
    (setf (index-file cld-repository)
          (format nil "~A/~A" 
		  (repository-address cld-repository)
                  +index-file-name+)))
  (when (not (search-index-path cld-repository))
    (setf (search-index-path cld-repository)
	  (merge-pathnames "index" 
			   (cache-directory cld-repository))))

  (if (not (probe-file (cached-index-file cld-repository)))
    ;; Download the index
    (update-cld-repository cld-repository)
    ;; else, load the index
    (progn
      (setf (index cld-repository) (read-index-file (cached-index-file cld-repository)))
      (initialize-search-index cld-repository))))

(defclass indexed-ssh-cld-repository (indexed-cld-repository cached-ssh-cld-repository)
  ())

(defclass indexed-http-cld-repository (indexed-cld-repository cached-http-cld-repository)
  ())

(defclass indexed-directory-cld-repository (indexed-cld-repository directory-cld-repository)
  ())

(defun initialize-search-index (cld-repository)
  (verbose-msg "Initializing search index...~%")
  (setf (search-index cld-repository)
	(make-instance 'montezuma:index
		       :path (search-index-path cld-repository))))

(defun remove-search-index (cld-repository)
  (verbose-msg "Removing search index...~%")
  (remove-directory (search-index-path cld-repository)))

(defun build-search-document (library-info)
  (list (cons "name" (getf library-info :name))
        (cons "author" (prin1-to-string (getf library-info :author)))
	(cons "description" (getf library-info :description))
	(cons "licence" (getf library-info :licence))
	(cons "keywords" (getf library-info :keywords))))

(defun build-search-index (cld-repository)
  (verbose-msg "Building search index...~%")
  (loop for library-info in (index cld-repository)
       do
       (montezuma:add-document-to-index (search-index cld-repository)
					(build-search-document library-info))))

(defun cached-index-file (cld-repository)
  (merge-pathnames (pathname +index-file-name+) 
		   (cache-directory cld-repository)))

(defun download-index-file (cld-repository)
  (verbose-msg "Downloading index file ~A~%" (index-file cld-repository))
  (ensure-directories-exist (cache-directory cld-repository))
  (let ((command (format nil "wget -O ~A ~A"
			 (cached-index-file cld-repository)
			 (index-file cld-repository))))
    (multiple-value-bind (output error status)
	(trivial-shell:shell-command command)
      (declare (ignore output error))
      (when (not (zerop status))
	(error "Error downloading repository index: ~A"
	       (index-file cld-repository))))))

(defun read-index-file (pathname)
  (verbose-msg "Reading index ~A...~%" pathname) 
  (read-from-string (file-to-string pathname)))

(defun library-index (library)
  "Create an index from the library description"
  (list :name (library-name library)
        :author (library-author library)
        :description (library-description library)
        :licence (library-licence library)
        :cld (cld-address (library-cld library))
        :keywords (library-keywords library)))

(defun build-index-file (pathname)
  (let ((*print-pretty* nil))
    (with-open-file (f pathname :direction :output
		       :if-exists :supersede
		       :if-does-not-exist :create)
      (format f "(")
      (loop for library in (list-all-libraries)
	 do
	   (format f "~S~%" (library-index library)))
      (format f ")"))))

(defun build-index-file-from-directory (directory index-file-pathname)
  ;; Load the library definitions first
  (setf *libraries* (make-hash-table :test #'equalp))
  (loop for file in (fad:list-directory directory :follow-symlinks nil)
     when (equalp (pathname-type file) "cld")
     do (load file))
  ;; build the index file
  (build-index-file index-file-pathname))

(defmethod find-cld :around ((cld-repository indexed-cld-repository) library-name)
  "The cld file search succeeds on indexed repositories iff it is found in the index"
  ;; TODO: should we use a hash table instead of list as index??
  (let ((library-info (find library-name (index cld-repository) 
			    :key (lambda (x) (getf x :name))
			    :test #'equalp)))
    (when library-info
      (call-next-method))))

(defgeneric update-cld-repository (cld-repository)
  (:method ((cld-repository cld-repository))
    ;; Do nothing
    ))

(defmethod update-cld-repository ((cld-repository indexed-cld-repository))
  (verbose-msg "Updating ~A...~%" cld-repository)
  (download-index-file cld-repository)
  (setf (index cld-repository) 
	(read-index-file (cached-index-file cld-repository)))
  (remove-search-index cld-repository)
  (initialize-search-index cld-repository)
  (build-search-index cld-repository))

(defgeneric search-cld-repository (cld-repository term)
  (:method ((cld-repository cld-repository) term)
    ;; Do nothing
    ))

(defmethod search-cld-repository ((cld-repository indexed-cld-repository) term)
  (verbose-msg "Searching for ~A in ~A...~%" term cld-repository)
  (montezuma:search (search-index cld-repository) term))

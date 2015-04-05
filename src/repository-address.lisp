#|

\chapter{Repository addresses}

\ignore{
|#


(in-package :cldm)

#|
}

\section{Overview}

A repository address is the address from which a library can be downloaded.

There are different types of repository addresses that can be handled by \textbf{CLDM}.

|#

(defclass repository-address ()
  ()
  (:documentation "Repository address"))

#|

\section{Directory repository address}

A directory repository address points to a filesystem directory. CLDM can copy the library from the directory specified.

|#

(defclass directory-repository-address (repository-address)
  ((directory :initarg :directory
              :initform (error "Provide the filesystem directory")
              :accessor repository-directory
              :documentation "The repository filesystem directory"))
  (:documentation "A repository in a file system directory"))

(defmethod print-object ((repository-address directory-repository-address) stream)
  (print-unreadable-object (repository-address stream :type t :identity t)
    (format stream "~A" (repository-directory repository-address))))

#|

\section{Git repository address}

A Git repository address points to a git repository. Url, branch, commit and tag can be specified.

|#

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

#|

\section{URL repository address}

An URL repository address points to some HTTP url where \verb'tar.gz' of the library can be found.

|#

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

(defclass darcs-repository-address (repository-address)
  ((url :initarg :url
        :initform (error "Provide the darcs url")
        :accessor url
        :documentation "The darcs url"))
  (:documentation "A darcs repository"))

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

(defmethod install-repository-from-address ((repository-address darcs-repository-address)
                                            repository install-directory)
  (flet ((run-or-fail (command)
           (multiple-value-bind (result code status)
               (trivial-shell:shell-command command)
					;(declare (ignore result code))
             (when (not (zerop status))
	       (print result)
	       (print code)
	       (print status)
               (return-from install-repository-from-address nil)))))
    (info-msg "Checking out repository ~A to ~A...~%" 
	      (url repository-address)
	      (princ-to-string install-directory))
    (let ((command (format nil "darcs get ~A ~A"
			   (url repository-address)
			   (princ-to-string install-directory))))
      (verbose-msg "~A~%" command)
      (run-or-fail command))
    t))

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

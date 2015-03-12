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
         :initform (error "Provide the repository name")
         :documentation "The cld repository name"))
  (:documentation "A .cld files repository"))

(defgeneric start-download-session (cld-repository)
  (:method ((cld-repository cld-repository))))

(defgeneric stop-download-session (cld-repository)
  (:method ((cld-repository cld-repository))))

(defgeneric publish-cld (cld-repository cld-pathname)
  (:method ((cld-repository cld-repository) cld-pathname)
    (error "~A repository doesn't support publishing" cld-repository)))


(defun remove-directory (directory)
  (when (fad:directory-exists-p directory)
    (multiple-value-bind (output error status)
        (trivial-shell:shell-command (format nil "rm -r ~A" directory))
      (declare (ignore output))
      (when (not (zerop status))
        (error error)))))

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

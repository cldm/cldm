#|

\chapter{CLD repositories}

\ignore{

|#

(in-package :cldm)

(defvar *if-already-installed-library-version* :supersede)

#|
}

\section{Overview}

CLD repositories are places where CLD files can be found and downloaded.

|#

(defclass cld-repository ()
  ((name :initarg :name
         :accessor name
         :initform (error "Provide the repository name")
         :documentation "The cld repository name"))
  (:documentation "A .cld files repository"))

#|

\section{Publishing}

CLDs can be published to some kind of CLD repositories

|#

(defgeneric publish-cld (cld-repository cld-pathname)
  (:method ((cld-repository cld-repository) cld-pathname)
    (error "~A repository doesn't support publishing" cld-repository)))

#|

\section{Updating}

|#

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

(defgeneric search-cld-repository (cld-repository term)
  (:method ((cld-repository cld-repository) term)
    ;; No results
    nil
    ))

;; Util

#|

\ignore {

|#

(defun remove-directory (directory)
  (when (fad:directory-exists-p directory)
    (multiple-value-bind (output error status)
        (trivial-shell:shell-command (format nil "rm -r ~A" directory))
      (declare (ignore output))
      (when (not (zerop status))
        (error error)))))


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

#|
}
|#

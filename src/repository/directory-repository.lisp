(in-package :cldm)

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

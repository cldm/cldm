#|
\chapter{Projects}
|#

#|

\ignore{

|#

(in-package :cldm)

#|
}
|#

#|

\section{Overview}

CLDM projects are filesystem directories with a CLD library definition file.

In the context of a CLDM project, dependencies are installed and loaded from a folder relative to the project directory, usually from the \verb'lib' folder. This way, projects dependencies can be installed, updated and loaded locally, without interference with libraries installed elsewhere.

|# 

(defclass project ()
  ((name :initarg :name
         :accessor project-name
         :initform (error "Provide the project name")
         :documentation "The project name")
   (library :initarg :library
            :accessor library
            :initform (error "Provide the project library")
            :documentation "The project library")
   (version :initarg :version
            :accessor project-version
            :initform nil
            :documentation "The project version")
   (project-directory :initarg :directory
                      :accessor project-directory
                      :initform (error "Provide the project directory")
                      :documentation "The project directory")
   (installed-library-versions :accessor installed-library-versions
                               :initform nil
                               :documentation "The project's installed libraries metadata")
   (libraries-directory :accessor libraries-directory
                        :initform nil
                        :documentation "Where dependency libraries are/are to be installed"))
  (:documentation "A CLDM project"))

(defmethod print-object ((project project) stream)
  (print-unreadable-object (project stream :type t :identity t)
    (format stream "~A ~@[~A~] in ~A"
            (project-name project)
            (project-version project)
            (project-directory project))))

(defun initialize-project (project)
  ;; Read project configuration
  (let ((project-config-file (merge-pathnames (pathname ".cldm")
                                              (project-directory project))))
    (when (probe-file project-config-file)
      (let ((config (read-config-file project-config-file)))
        (awhen (getf config :libraries-directory)
          (setf (libraries-directory project) it))
        (awhen (getf config :project-version)
          (setf (project-version project) it))))

    ;; Configure unconfigured variables
    (when (not (libraries-directory project))
      (setf (libraries-directory project)
	    (merge-pathnames #p"lib/"
			     (project-directory project))))))

(defmethod installed-library-versions ((project project))
  (aif (slot-value project 'installed-library-versions)
       it
       ;; Read the list of installed libraries
       (let ((lock-file (merge-pathnames (pathname "cldm.lock")
                                         (project-directory project))))
         (when (probe-file lock-file)
           (setf (installed-library-versions project)
                 (read-lock-file lock-file)))
         (slot-value project 'installed-library-versions))))

(defun find-project-cld-file (directory)
  (first
   (directory
    (make-pathname :directory (pathname-directory directory)
                   :name :wild
                   :type "cld"))))

(defun load-project-from-directory (directory &optional (error-p t))
  (let ((project-cld-file (find-project-cld-file directory)))
    (if (not project-cld-file)
        (when error-p
          (error "There's no CLDM project in ~A" directory))
        ;; else
        (progn
          (load-cld project-cld-file)
          (let ((library *latest-registered-library*))
            (make-instance 'project
                           :name (library-name library)
                           :library library
                           :directory directory))))))

(defmethod initialize-instance :after ((project project) &rest initargs)
  (declare (ignore initargs))
  (initialize-project project))

(defun find-installed-library-version (project library-name)
  (find library-name (installed-library-versions project)
        :key #'name
        :test #'equalp))

(defun create-lock-file (project installed-library-versions)
  (let ((lock-file-pathname (merge-pathnames "cldm.lock"
                                             (project-directory project))))
    (verbose-msg "Writing lock file ~A~%" lock-file-pathname)
    (with-open-file (f lock-file-pathname
                       :direction :output
                       :if-exists :supersede
                       :if-does-not-exist :create)
      (format f "~S"
              (loop for installed-library-version in installed-library-versions
                 collect
                   (serialize-installed-library-version installed-library-version))))))

(defun serialize-installed-library-version (installed-library-version)
  (list
   :name (name installed-library-version)
   :version (print-version-to-string (version installed-library-version))
   :install-directory (install-directory installed-library-version)
   :repository (let ((repository (repository installed-library-version)))
                 (list (name repository)
                       (repository-address-sexp (repository-address repository))))
   :checksum (directory-checksum (install-directory installed-library-version))))

(defun unserialize-installed-library-version (data)
  (make-instance 'installed-library-version
                 :name (getf data :name)
                 :version (read-version-from-string (getf data :version))
                 :install-directory (getf data :install-directory)
                 :repository (destructuring-bind (name rep-address-sexp)
                                 (getf data :repository)
                               (make-instance 'library-version-repository
                                              :name name
                                              :library-version nil
                                              :address (parse-repository-address rep-address-sexp)))
                 :checksum (getf data :checksum)))

#|

\section{Lock files}

The currently installed library versions are saved to a .lock file.

|#

(defun read-lock-file (file)
  (verbose-msg "Reading lock file ~A~%" file)
  (let ((installed-libraries-info (read-from-string (file-to-string file))))
    (loop for data in installed-libraries-info
       collect (unserialize-installed-library-version data))))

(in-package :cldm)

(defclass project ()
  ((name :initarg :name
	 :accessor project-name
	 :initform nil
	 :documentation "The project name")
   (version :initarg :version
	    :accessor project-version
	    :initform nil
	    :documentation "The project version")
   (project-directory :initarg :directory
		      :accessor project-directory
		      :initform nil
		      :documentation "The project directory")
   (installed-libraries :accessor installed-libraries
			:initform nil
			:documentation "The project's installed libraries and metadata")
   (libraries-directory :accessor libraries-directory
			:initform *local-libraries-directory*
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
  (let ((config (read-config-file (merge-pathnames (pathname ".cldm")
						   (project-directory project)))))
    (awhen (getf config :libraries-directory)
      (setf (libraries-directory project) it))
    (awhen (getf config :project-version)
      (setf (project-version project) it)))

  ;; Read the list of installed libraries
  
  )

(defun find-project-cld-file (directory)
  (first
   (directory
    (make-pathname :directory (pathname-directory directory)
                   :name :wild
                   :type "cld"))))

(defun load-project-from-directory (directory)
      (let ((libraries-directory (or (clon:getopt :long-name "libraries-directory")
                                   cldm:*local-libraries-directory*))


(defmethod initialize-instance :after ((project project) &rest initargs)
  (initialize-project project))

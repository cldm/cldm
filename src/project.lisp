(in-package :cldm)

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
   (installed-libraries :accessor installed-libraries
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
	  (setf (project-version project) it)))

      ;; Configure unconfigured variables
      (when (not (libraries-directory project))
	(setf (libraries-directory project)
	      (merge-pathnames #p"lib/"
			       (project-directory project)))))))

(defmethod installed-libraries ((project project))
  (aif (slot-value project 'installed-libraries)
    it
    ;; Read the list of installed libraries
    (let ((lock-file (merge-pathnames (pathname "cldm.lock")
					(project-directory project))))
      (when (probe-file lock-file)
	(setf (installed-libraries project)
	      (read-lock-file lock-file)))
      (slot-value project 'installed-libraries))))

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
  (initialize-project project))

(defun find-installed-library-info (project library-name)
  (find library-name (installed-libraries project)
	:key (compose #'library-name #'first)
	:test #'equalp))

(defun create-lock-file (installed-libraries)
  (let ((lock-file-pathname (merge-pathnames "cldm.lock"
                                             (osicat:current-directory))))
    (with-open-file (f lock-file-pathname
                       :direction :output
                       :if-exists :supersede
                       :if-does-not-exist :create)
      (format f "~S"
              (loop for installed-library in installed-libraries
                 collect
                   (destructuring-bind (library-version library-directory repository) installed-library
                     (list
                      (library-name (library library-version))
		      (print-version-to-string (version library-version))
                      library-directory
		      (list (name repository)
                            (repository-address-sexp (repository-address repository)))
		      (directory-checksum library-directory))))))))

(defun read-lock-file (file)
  (let ((installed-libraries-info (read-from-string (file-to-string file))))
    (loop for installed-library-info in installed-libraries-info
	 collect
	 (destructuring-bind (library version library-directory repository-spec md5)
	     installed-library-info
	     (let* ((library (find-library library))
		    (library-version (find-library-version library
							   (or (and version
								    (read-version-from-string version))"latest"))))
		    
	     (list library-version
		   library-directory
		   (destructuring-bind (name rep-address-sexp) repository-spec
		       (make-instance 'library-version-repository
				      :name name
				      :library-version library-version
				      :address (parse-version-repository-address rep-address-sexp)))
		   md5))))))

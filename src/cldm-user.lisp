(defpackage :cldm-user
  (:use :cl :duologue)
  (:shadow :require :search)
  (:export :install
           :require
           :search
           :install-project
           :load-project
           :load-current-project
           :publish
           :show
           :init
	   :cd
           :config-print
           :config-get
           :config-set
           :repo-add
           :repo-remove
           :repo-append
           :repo-unappend
           :repo-list
           :repo-update
           :repo-search
           :repo-clear
           :repo-cache
           :repo-publish))

(in-package :cldm-user)

(defun install (library &rest args
                &key version
                  cld
                  (verbose cldm::*verbose-mode*)
                  (solving-mode cldm::*solving-mode*)
                  (libraries-directory cldm::*libraries-directory*)
                  (clear-registered-libraries t)
                  (interactive t))
  (declare (ignorable version cld verbose solving-mode libraries-directory
                      clear-registered-libraries interactive))
  (apply #'cldm:install-library (string-downcase (string library))
         args))

(defun require (library &rest args
                &key
                  version
                  (clean-asdf-environment cldm::*clean-asdf-environment*)
                  (libraries-directory cldm::*libraries-directory*))
  (declare (ignorable version libraries-directory clean-asdf-environment))
  (apply #'cldm::load-library (string-downcase (string library)) args))

(defun install-project (project &rest args
                        &key
                          version
                          libraries-directory
                          (verbose cldm::*verbose-mode*)
                          (solving-mode cldm::*solving-mode*)
                          (clear-registered-libraries t)
                          (interactive t))
  (declare (ignorable version libraries-directory
                      verbose solving-mode
                      clear-registered-libraries
                      interactive))
  (apply #'cldm::install-project project args))

(defun load-project (project &rest args
                     &key
                       libraries-directory
                       (clean-asdf-environment cldm::*clean-asdf-environment*))
  (declare (ignorable libraries-directory
                      clean-asdf-environment))
  (apply #'cldm::load-project project args))

(defun load-current-project (&rest args &key libraries-directory
                                          (clean-asdf-environment cldm::*clean-asdf-environment*))
  (declare (ignorable libraries-directory clean-asdf-environment))
  (apply #'load-project (osicat:current-directory) args))

(defun repo-list ()
  (cldm::list-cld-repositories))

(defun search (library)
  (cldm.cmd::search-library library))

(defun cd (dir)
  (setf (osicat:current-directory) dir))

(defun find-asdf-files (&optional (directory (osicat:current-directory)))
  (directory (merge-pathnames "*.asd" directory)))

(defun find-cld-files (&optional (directory (osicat:current-directory)))
  (directory (merge-pathnames "*.cld" directory)))

(defun init (&key name force (directory (osicat:current-directory)))
  (let ((cld-files (find-cld-files directory)))
    (when cld-files
      (say "A system seems to have been initialized here: ~A" cld-files)
      (when (not (ask "Continue?" :default t))
	(return-from init))))
  (let* ((asdf-files (find-asdf-files directory))
	 (asdf-file nil))
    (when asdf-files
      (when (ask "There are ASDF systems available. Do you want to take some info from there?" :default t)
	(setf asdf-file
	      (if (> (length asdf-files) 1)
		  (choose "Choose the asdf file: " asdf-files)
		  (first asdf-files)))
	(say "Using ~A" asdf-file)))
    (let ((cld-filename (or (and name (pathname (format nil "~A.cld" name)))
			    (format nil "~A.cld" (car (last (pathname-directory (osicat:current-directory))))))))
      (flet ((create-cld-file ()
	       (let ((cld-template
		      (cldm.cmd::create-cld-template-interactive)))
		 (say "~A" cld-template)
		 (when (ask "Create?" :default t)
		   (with-open-file (f cld-filename :direction :output
				      :if-exists :supersede
				      :if-does-not-exist :create)
		     (format f "~A" cld-template))))))

	;; If the cld file exists, error unless a force option was given
	(let ((cld-file (merge-pathnames cld-filename
					 (osicat:current-directory))))
	  (if (probe-file cld-file)
	      (if (not force)
		  (progn
		    (format t "The cld file already exist. Use the :force option to overwrite.~%"))
		  (create-cld-file))
	      (create-cld-file)))))))

(defparameter +config-variables+
  (list (cons :libraries-directory
              (lambda (value scope)
                (cldm::config-set-libraries-directory (pathname value) scope)))
        (cons :local-libraries-directory
              (lambda (value scope)
                (cldm::config-set-local-libraries-directory (pathname value) scope)))
        (cons :verbose
              (lambda (value scope)
                (cldm::config-set-verbose
                 (not (member value (list "no" "false")
                              :test #'equalp))
                 scope)))
        (cons :minisat+-binary
              #'cldm::config-set-minisat+-binary)
        (cons :solving-mode
              (lambda (value scope)
                (cldm::config-set-solving-mode (intern (string-upcase value) :keyword)
                                               scope)))))


(defun config-print (&optional (scope :local))
  (loop for config-var in (mapcar #'car +config-variables+)
     do
       (format t "~A: ~A~%"
               config-var
               (cldm::get-config-var config-var scope))))

(defun config-get (variable &optional (scope :local))
  (if (not (assoc variable +config-variables+))
      (format t "~A is not a valid config variable. Config variables: ~{~A~^, ~}~%"
              variable
              (mapcar #'car +config-variables+))
      (format t "~A~%"
              (cldm::get-config-var variable scope))))

(defun config-set (variable value &optional (scope :local))
  (let ((variable-setter (cdr (assoc variable +config-variables+))))
    (if (not variable-setter)
        (format t "~A is not a valid config variable. Config variables: ~{~A~^, ~}~%"
                variable
                (mapcar #'car +config-variables+))
        (funcall variable-setter value scope))))

(defun publish (repository-name &optional cld)
  (let ((repository (cldm::find-cld-repository repository-name))
        (cld (or cld
                 (cldm::find-project-cld-file (osicat:current-directory)))))
    (cldm:publish-cld repository cld)))

(defun repo-add (repo-spec &optional (scope :local))
  (cldm::config-add-repository repo-spec scope))

(defun repo-remove (repo-name &optional (scope :local))
  (cldm::config-remove-repository repo-name scope))

(defun repo-append (repo-spec &optional (scope :local))
  (cldm::config-append-repository repo-spec scope))

(defun repo-unappend (repo-name &optional (scope :local))
  (cldm::config-unappend-repository repo-name scope))

(defun repo-publish (repo-name cld)
  (let ((repo (cldm::find-cld-repository repo-name)))
    (cldm::publish-cld repo cld)))

(defun repo-update (repo-name)
  (let ((repo (cldm::find-cld-repository repo-name)))
    (cldm::update-cld-repository repo)))

(defun repo-cache (repo-name)
  (let ((repo (cldm::find-cld-repository repo-name)))
    (cldm::cache-cld-repository repo)))

(defun repo-clear (repo-name)
  (let ((repo (cldm::find-cld-repository repo-name)))
    (cldm::clear-cache repo)))

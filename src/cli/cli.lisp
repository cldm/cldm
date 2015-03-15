(in-package :cldm.cli)

(defparameter *debug-mode* nil)
(defparameter *verbose-mode* nil)

(defun verbose-msg (msg &rest args)
  (when *verbose-mode*
    (apply #'format t msg args)))

(defparameter +CLDM-version+ "0.0.1")

(defgeneric process-command (command))

(defmethod process-command :around (command)
  (handler-case
      (call-next-method)
    (error (e)
      (format t "An error ocurred: ~A~%" e)
      (when *debug-mode*
	(trivial-backtrace:print-backtrace e))
      (clon:exit 1))))

(defparameter +commands+
  (list
   ;; init command
   (cons "init"
         (clon:defsynopsis (:make-default nil :postfix "[OPTIONS]")
           (text :contents "Initialize a basic cld file in the current directory.")
           (flag :short-name "h" :long-name "help"
                 :description "Print this help and exit.")
           (switch :short-name "f" :long-name "force"
                   :description "Force cld file creation")
           (stropt :long-name "name"
                   :argument-name "NAME"
		   :description "The library name")
           (stropt :long-name "cld"
                   :argument-name "CLD"
                   :default-value ""
                   :argument-type :optional
                   :description "The cld address")
           (stropt :long-name "description"
                   :argument-name "DESCRIPTION"
                   :default-value ""
                   :argument-type :optional
                   :description "The library description")
           (stropt :long-name "author"
                   :argument-name "AUTHOR"
                   :default-value ""
                   :argument-type :optional
                   :description "The library author")))
   ;; search command
   (cons "search"
	 (clon:defsynopsis (:make-default nil :postfix "LIBRARY")
	   (text :contents "Search for a library")
	   (flag :short-name "h" :long-name "help"
                 :description "Print this help and exit.")))
   ;; install command
   (cons "install"
         (clon:defsynopsis (:make-default nil :postfix "[LIBRARY] [VERSION]")
           (text :contents "Install the CLDM project dependencies or a particular library.")
           (flag :short-name "h" :long-name "help"
                 :description "Print this help and exit.")
           (flag :short-name "d" :long-name "dry-run"
                 :description "Fake the install operation. List the libraries that would be installed")
           (switch :short-name "v" :long-name "verbose"
                   :description "Run in verbose mode")
	   (switch :short-name "i" :long-name "interactive"
		   :description "Interactive"
		   :default-value t)
           (switch :long-name "lenient"
                   :default-value nil
                   :description "Allow some of the dependencies not to be installed.")
           (path :long-name "libraries-directory"
                 :type :directory)))
   ;; update command
   (cons "update"
         (clon:defsynopsis (:make-default nil)
           (text :contents "Update the CLDM project dependencies to available latest versions.")
           (flag :short-name "h" :long-name "help"
                 :description "Print this help and exit.")
           (flag :short-name "d" :long-name "dry-run"
                 :description "Fake the update operation. List which libraries would be updated")
	   (switch :short-name "i" :long-name "interactive"
		   :description "Interactive"
		   :default-value t)
           (switch :short-name "v" :long-name "verbose"
                   :description "Run in verbose mode")
           (switch :long-name "lenient"
                   :default-value nil
                   :description "Allow some of the dependencies not to be updated.")))
   ;; config command
   (cons "config"
         (clon:defsynopsis (:make-default nil
                                          :postfix (format nil "CONFIG-CMD [OPTIONS]"))
           (text :contents (format nil "Manage CLDM configuration.~%
Config commands: ~{~A~^, ~}~%" (mapcar #'car +config-commands+)))
           (flag :short-name "h" :long-name "help"
                 :description "Print this help and exit.")
           (enum :long-name "scope"
                 :enum (list :system :user :local)
                 :default-value :local)))
   ;; repositories command
   (cons "repo"
         (clon:defsynopsis (:make-default nil
                                          :postfix (format nil "REPO-CMD [OPTIONS]"))
           (text :contents (format nil "Manage CLDM CLD repositories.~%
Repositories commands: ~{~A~^, ~}~%" (mapcar #'car +repo-commands+)))
           (flag :short-name "h" :long-name "help"
                 :description "Print this help and exit.")
           (enum :long-name "scope"
                 :enum (list :system :user :local)
                 :default-value :local)))
   ;; show command
   (cons "show"
	 (clon:defsynopsis (:make-default nil :postfix "LIBRARY")
	   (text :contents "Display a library information")
	   (flag :short-name "h" :long-name "help"
                 :description "Print this help and exit.")))))

(defun print-command-list ()
  (format nil "~{~A~^, ~}" (mapcar #'car +commands+)))

(defun find-command (name)
  (cdr (assoc name +commands+ :test #'string=)))

(clon:defsynopsis (:postfix "command [OPTIONS]")
  (text :contents (format nil "CLDM is a Common Lisp Dependency Manager.

Available commands: ~A

Use 'cldm <command> --help' to get command-specific help.
" (print-command-list)))
  (flag :short-name "h" :long-name "help"
        :description "Print this help and exit.")
  (flag :long-name "version"
        :description "Print the CLDM version")
  (switch :short-name "v" :long-name "verbose"
          :description "Run in verbose mode"
	  :env-var "VERBOSE")
  (switch :short-name "d" :long-name "debug"
          :description "Turn debugging on or off."
          :argument-style :on/off
          :env-var "DEBUG"))

(defun initialize-cldm ()
  "Load cldm config"
  (verbose-msg "Initializing cldm~%")
  (setf cldm::*system-config-file* #p"/etc/cldm/config")
  (setf cldm::*user-config-file* #p"~/.cldm/config")
  (setf cldm:: *local-config-file* (merge-pathnames (pathname ".cldm")
                                                    (osicat:current-directory)))
  (setf cldm::*local-libraries-directory* (merge-pathnames (pathname "lib/")
                                                           (osicat:current-directory)))
  (cldm::load-cldm-config))

(defun main ()
  "Entry point for the standalone application."
  
  (initialize-cldm)

  ;; Prepare to process command line
  (clon:make-context)
  (cond ((or (clon:getopt :short-name "h")
             (not (clon:cmdline-p)))
         (clon:help))
        ((clon:getopt :long-name "version")
         (format t "CLDM Common Lisp Dependency Manager version ~A~%" +CLDM-version+))
        (t
         (unless (clon:remainder)
           (format t "Missing command.~%")
           (clon:exit 1))
	 ;; Process switches
	 ;; Careful: there's a bug in CLON. When clon:getopt on the same
	 ;; option twice, gives wrong value the second time.
	 (let ((*verbose-mode* (clon:getopt :long-name "verbose"))
	       ;; Bug: why does this not work here?
	       ;(cldm::*verbose-mode* *verbose-mode*)
	       (*debug-mode* (clon:getopt :long-name "debug")))
	   (when *debug-mode*
	     (format t "Debug mode on~%"))
	   (when *verbose-mode*
	     (format t "Verbose mode on~%")
	     ;; Bug: binding above doesn't work
	     (setf cldm::*verbose-mode* *verbose-mode*))
	   (clon:make-context
	    :synopsis (let ((command-name (car (clon:remainder))))
			(let ((command (find-command command-name)))
			  (if command
			      command
			      (progn
				(format t "Unknown command.~%")
				(clon:exit 1)))))
	    :cmdline (clon:remainder))
	   (cond ((clon:getopt :short-name "h")
		  (clon:help))
		 (t ;; Process the command
		  (verbose-msg "Running ~A command~%" (clon:progname))
		  (process-command (intern (string-upcase (clon:progname)) :keyword)))))))
  (clon:exit))

(defun install-project-command ()
  (let ((project (cldm::load-project-from-directory (osicat:current-directory))))
    (when (not project)
      (format t "Couldn't find a CLDM project in the current directory. Run `cldm init` to start.~%")
      (clon:exit 1))
    (let ((solving-mode (or (and (clon:getopt :long-name "lenient")
                                 :lenient)
                            :strict))
          (verbose-mode (or (clon:getopt :long-name "verbose")
                            *verbose-mode*)))
      (format t "Installing ~A dependencies to ~A~%"
              (cldm::project-name project)
              (cldm::libraries-directory project))
      (cldm:install-project project
			    :solving-mode solving-mode
			    :verbose verbose-mode
			    :interactive (clon:getopt :long-name "interactive"))
      (format t "Done.~%")
      (clon:exit 0))))

(defun install-library-command ()
  (let ((libraries-directory (or (clon:getopt :long-name "libraries-directory")
                                 cldm:*libraries-directory*))
        (solving-mode (or (and (clon:getopt :long-name "lenient")
                               :lenient)
                          :strict))
        (verbose-mode (or (clon:getopt :long-name "verbose")
			  *verbose-mode*))
        (library-name (car (clon:remainder)))
        (library-version-string (cadr (clon:remainder))))
    (cldm:install-library library-name
			  :version library-version-string
			  :libraries-directory libraries-directory
			  :solving-mode solving-mode
			  :verbose verbose-mode
			  :interactive (clon:getopt :long-name "interactive"))
    (format t "Done.~%")
    (clon:exit 0)))

(defmethod process-command ((command (eql :install)))
  (if (clon:remainder)
      (install-library-command)
      (install-project-command)))

(defun update-project-command ()
  (let ((project (cldm::load-project-from-directory (osicat:current-directory))))
    (when (not project)
      (format t "Couldn't find a CLDM project in the current directory. Run `cldm init` to start.~%")
      (clon:exit 1))
    (let ((solving-mode (or (and (clon:getopt :long-name "lenient")
                                 :lenient)
                            :strict))
          (verbose-mode (or (clon:getopt :long-name "verbose")
                            *verbose-mode*)))
      (format t "Updating ~A dependencies~%"
	      (cldm::project-name project))
      (cldm:update-project project
			   :solving-mode solving-mode
			   :verbose verbose-mode
			   :interactive (clon:getopt :long-name "interactive"))
      (format t "Done.~%")
      (clon:exit 0))))

(defmethod process-command ((command (eql :update)))
  (update-project-command))

(defmethod process-command ((command (eql :show)))
  (let ((library-name (car (clon:remainder))))
    ;; Check that the library name was given
    (when (null library-name)
      (format t "Library name is missing.~%")
      (clon:exit 1))
    (let ((cld-pathname (cldm::find-library-cld library-name)))
      (if (not cld-pathname)
	  (progn
	    (format t "Library information not found~%")
	    (clon:exit 1))
	  (format t "~%~A~%" (cldm::file-to-string cld-pathname))))))

(defmethod process-command ((comand (eql :search)))
  (let ((library-name (car (clon:remainder))))
    ;; Check that the library name was given
    (when (null library-name)
      (format t "Library name is missing.~%")
      (clon:exit 1))
    (loop for repo in (cldm::list-cld-repositories)
       do
	 (format t "~A:~%" (cldm::name repo))
	 (let ((search-result 
		(ignore-errors (cldm::search-cld-repository repo (format nil "name:\"~A\"" library-name)))))
	   (loop for elem in search-result
	      do 
		(format t "~A ~A~%" 
			(cdr (assoc :name elem))
			(cdr (assoc :score elem))))))))

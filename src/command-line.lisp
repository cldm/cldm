(in-package :cl-user)

(require :cldm)

(ql:quickload :com.dvlsoft.clon)

(setq *load-verbose* nil)

(eval-when (:execute :load-toplevel :compile-toplevel)
  (com.dvlsoft.clon:nickname-package))

(defparameter +commands+
  (list
   ;; Push command
   (cons "push"
	 (clon:defsynopsis (:make-default nil)
	   (text :contents "Push local changes to the remote server.")
	   (flag :short-name "h" :long-name "help"
		 :description "Print this help and exit.")
	   (flag :short-name "d" :long-name "dry-run"
		 :description "Fake the push operation.")
	   (stropt :long-name "remote"
		   :argument-name "SERVER"
		   :description "Use SERVER instead of default remote.")))
   ;; Pop command
   (cons "pop"
	 (clon:defsynopsis (:make-default nil)
	   (text :contents "Pull remote changes to the local server.")
	   (flag :short-name "h" :long-name "help"
		 :description "Print this help and exit.")
	   (flag :short-name "d" :long-name "dry-run"
		 :description "Fake the push operation.")
	   (switch :long-name "update"
		   :default-value t
		   :description "Also update the working directory.")))))

(defun print-command-list ()
  (format nil "~{~A~^, ~}" (mapcar #'car +commands+)))

(defun find-command (name)
  (cdr (assoc name +commands+ :test #'string=)))

(clon:defsynopsis (:postfix "command [OPTIONS]")
  (text :contents (format nil "  ___ _    ___  __  __ 
 / __| |  |   \\|  \\/  |
| (__| |__| |) | |\\/| |
 \\___|____|___/|_|  |_|
                        

CLDM is a Common Lisp Dependency Manager.

Available commands: ~A

Use 'cldm <command> --help' to get command-specific help.
" (print-command-list)))	
  (flag :short-name "h" :long-name "help"
	:description "Print this help and exit.")
  (switch :short-name "d" :long-name "debug"
	  :description "Turn debugging on or off."
	  :argument-style :on/off
	  :env-var "DEBUG"))

(defun main ()
  "Entry point for the standalone application."
  (clon:make-context)
  (cond ((or (clon:getopt :short-name "h")
	     (not (clon:cmdline-p)))
	 (clon:help))
	(t
	 (unless (clon:remainder)
	   (format t "Missing command.~%")
	   (clon:exit 1))
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
	       (t
		(format t "Command name: ~A~%~%" (clon:progname))
		(format t "Options:")
		(clon:do-cmdline-options (option name value source)
		  (print (list option name value source)))
		(terpri)
		(format t "Remainder: ~A~%" (clon:remainder))))))
  (clon:exit))

(clon:dump "cldm" main)

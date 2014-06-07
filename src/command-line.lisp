(in-package :cl-user)

(require :cldm)

(ql:quickload :com.dvlsoft.clon)

(setq *load-verbose* nil)

(eval-when (:execute :load-toplevel :compile-toplevel)
  (com.dvlsoft.clon:nickname-package))

(clon:defsynopsis (:postfix "command [OPTIONS]")
  (text :contents "  ___ _    ___  __  __ 
 / __| |  |   \\|  \\/  |
| (__| |__| |) | |\\/| |
 \\___|____|___/|_|  |_|
                        

CLDM is a Common Lisp Dependency Manager.

Available commands: push pull.

Use 'cldm <command> --help' to get command-specific help.
")
  (flag :short-name "h" :long-name "help"
	:description "Print this help and exit.")
  (switch :short-name "d" :long-name "debug"
	  :description "Turn debugging on or off."
	  :argument-style :on/off
	  :env-var "DEBUG"))

(defconstant +push-synopsis+
    (clon:defsynopsis (:make-default nil)
      (text :contents "Push local changes to the remote server.")
      (flag :short-name "h" :long-name "help"
	    :description "Print this help and exit.")
      (flag :short-name "d" :long-name "dry-run"
	    :description "Fake the push operation.")
      (stropt :long-name "remote"
	      :argument-name "SERVER"
	      :description "Use SERVER instead of default remote."))
  "The synopsis for the PUSH operation.")

(defconstant +pull-synopsis+
    (clon:defsynopsis (:make-default nil)
      (text :contents "Pull remote changes to the local server.")
      (flag :short-name "h" :long-name "help"
	    :description "Print this help and exit.")
      (flag :short-name "d" :long-name "dry-run"
	    :description "Fake the push operation.")
      (switch :long-name "update"
	      :default-value t
	      :description "Also update the working directory."))
  "The synopsis for the PULL operation.")


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
	  :synopsis (cond ((string= (first (clon:remainder)) "push")
			   +push-synopsis+)
			  ((string= (first (clon:remainder)) "pull")
			   +pull-synopsis+)
			  (t
			   (format t "Unknown command.~%")
			   (clon:exit 1)))
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

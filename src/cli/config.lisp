(in-package :cldm.cli)

(defparameter +config-commands+
  (list
   (cons "print"
	 (clon:defsynopsis (:make-default nil)
	   (text :contents "Prints configuration values")
           (flag :short-name "h" :long-name "help"
                 :description "Print this help and exit.")))
   (cons "set"
         (clon:defsynopsis (:make-default nil :postfix "VARIABLE VALUE")
           (text :contents "Sets a CLDM configuration variable value.")
           (flag :short-name "h" :long-name "help"
                 :description "Print this help and exit.")))
   (cons "get"
         (clon:defsynopsis (:make-default nil :postfix "VARIABLE VALUE")
           (text :contents "Gets a CLDM configuration variable value.")
           (flag :short-name "h" :long-name "help"
                 :description "Print this help and exit.")))))

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

(defun find-config-command (name)
  (cdr (assoc name +config-commands+ :test #'string=)))

(defgeneric process-config-command (command scope))

(defmethod process-command ((command (eql :config)))
  (let ((scope (or (clon:getopt :long-name "scope")
                   :local)))
    (clon:make-context
     :synopsis (let ((command-name (car (clon:remainder))))
                 (let ((command (find-config-command command-name)))
                   (if command
                       command
                       (progn
                         (format t "Unknown config command.~%")
                         (clon:exit 1)))))
     :cmdline (clon:remainder))
    (cond ((clon:getopt :short-name "h")
           (clon:help))
          (t ;; Process the config command
           (process-config-command (intern (string-upcase (clon:progname)) :keyword)
                                   scope)))))

(defmethod process-config-command ((command (eql :set)) scope)
  (let ((variable-name (car (clon:remainder)))
        (variable-value (cadr (clon:remainder))))
    (let* ((variable-keyword (intern (string-upcase variable-name) :keyword))
           (variable-setter (cdr (assoc variable-keyword +config-variables+))))
      (when (not variable-setter)
        (format t "~A is not a valid config variable. Config variables: ~{~A~^, ~}~%"
                variable-name
                (mapcar #'car +config-variables+))
        (clon:exit 1))
      (funcall variable-setter variable-value scope))))

(defmethod process-config-command ((command (eql :get)) scope)
  (let* ((variable-name (car (clon:remainder)))
         (variable-keyword (intern (string-upcase variable-name) :keyword)))
    (when (not (assoc variable-keyword +config-variables+))
      (format t "~A is not a valid config variable. Config variables: ~{~A~^, ~}~%"
              variable-name
              (mapcar #'car +config-variables+))
      (clon:exit 1))
    (format t "~A~%"
            (cldm::get-config-var variable-keyword scope))))

(defmethod process-config-command ((command (eql :print)) scope)
  (loop for config-var in (mapcar #'car +config-variables+)
       do
       (format t "~A: ~A~%"
	       config-var
	       (cldm::get-config-var config-var scope))))

(in-package :cldm.cli)

(defparameter +repo-commands+
  (list
   (cons "add"
         (clon:defsynopsis (:make-default nil :postfix "NAME TYPE ARGS")
           (text :contents "Adds a CLD repository to the configuration")
           (flag :short-name "h" :long-name "help"
                 :description "Print this help and exit.")))
   (cons "remove"
         (clon:defsynopsis (:make-default nil :postfix "NAME")
           (text :contents "Removes the CLD repository named NAME from the configuration")
           (flag :short-name "h" :long-name "help"
                 :description "Print this help and exit.")))
   (cons "append"
         (clon:defsynopsis (:make-default nil :postfix "NAME TYPE ARGS")
           (text :contents "Appends a CLD repository to the configuration")
           (flag :short-name "h" :long-name "help"
                 :description "Print this help and exit.")))
   (cons "unappend"
         (clon:defsynopsis (:make-default nil :postfix "NAME")
           (text :contents "Removes a CLD repository from the list of appended repositories")
           (flag :short-name "h" :long-name "help"
                 :description "Print this help and exit.")))
   (cons "list"
         (clon:defsynopsis (:make-default nil)
           (text :contents "List CLD repositories")
           (flag :short-name "h" :long-name "help"
                 :description "Print this help and exit.")))
   (cons "publish"
	 (clon:defsynopsis (:make-default nil :postfix "CLD REPO")
	   (text :contents "Publish a cld file to a repository")
	   (flag :short-name "h" :long-name "help"
                 :description "Print this help and exit.")))
   (cons "update"
	 (clon:defsynopsis (:make-default nil :postfix "REPO")
	   (text :contents "Update a repository")
	   (flag :short-name "h" :long-name "help"
                 :description "Print this help and exit.")))
   (cons "search"
	 (clon:defsynopsis (:make-default nil :postfix "REPO LIBRARY")
	   (text :contents "Search for a library in repository")
	   (flag :short-name "h" :long-name "help"
                 :description "Print this help and exit.")))
   (cons "cache"
	 (clon:defsynopsis (:make-default nil :postfix "REPO")
	   (text :contents "Download repository cld files")
	   (flag :short-name "h" :long-name "help"
                 :description "Print this help and exit.")))
   (cons "clear"
         (clon:defsynopsis (:make-default nil :postfix "REPOSITORY-NAME")
           (text :contents "Clears the cache of a repository")
           (flag :short-name "h" :long-name "help"
                 :description "Print this help and exit.")))))

(defun find-repo-command (name)
  (cdr (assoc name +repo-commands+ :test #'string=)))

(defgeneric process-repo-command (command scope))

(defmethod process-command ((command (eql :repo)))
  (let ((scope (or (clon:getopt :long-name "scope")
                   :local)))
    (clon:make-context
     :synopsis (let ((command-name (car (clon:remainder))))
                 (let ((command (find-repo-command command-name)))
                   (if command
                       command
                       (progn
                         (format t "Unknown repositories command.~%")
                         (clon:exit 1)))))
     :cmdline (clon:remainder))
    (cond ((clon:getopt :short-name "h")
           (clon:help))
          (t ;; Process the config command
           (process-repo-command
            (intern (string-upcase (clon:progname)) :keyword)
            scope)))))

(defmethod process-repo-command ((command (eql :add)) scope)
  (destructuring-bind (repository-name repository-type &rest args) (clon:remainder)
    (setf repository-type (intern (string-upcase (cadr (clon:remainder))) :cldm))
    (setf args (loop for prop in args by #'cddr
                  for val in (cdr args) by #'cddr
                  collect (intern (string-upcase prop) :keyword)
                  collect val))
    (cldm::config-add-repository `(,repository-type :name ,repository-name ,@args)
                                 scope)))

(defmethod process-repo-command ((command (eql :remove)) scope)
  (let ((repository-name (car (clon:remainder))))
    (let ((repository
           (cldm::find-cld-repository repository-name)))
      (when (not repository)
        (format t "Repository not found: ~A~%" repository-name)
        (clon:exit 1))
      (cldm::config-remove-repository repository-name scope))))

(defmethod process-repo-command ((command (eql :append)) scope)
  (destructuring-bind (repository-name repository-type &rest args) (clon:remainder)
    (setf repository-type (intern (string-upcase (cadr (clon:remainder))) :cldm))
    (setf args (loop for prop in args by #'cddr
                  for val in (cdr args) by #'cddr
                  collect (intern (string-upcase prop) :keyword)
                  collect val))
    (cldm::config-append-repository `(,repository-type :name ,repository-name ,@args)
                                    scope)))

(defmethod process-repo-command ((command (eql :unappend)) scope)
  (let ((repository-name (car (clon:remainder))))
    (let ((repository
           (cldm::find-cld-repository repository-name)))
      (when (not repository)
        (format t "Repository not found: ~A~%" repository-name)
        (clon:exit 1))
      (cldm::config-unappend-repository repository-name scope))))

(defmethod process-repo-command ((command (eql :list)) scope)
  (loop for repository in (cldm:list-cld-repositories)
     do (format t "~A~%" repository)))

(defmethod process-repo-command ((command (eql :publish)) scope)
  (destructuring-bind (cld-filepath repository-name) (clon:remainder)
    (let ((cld-pathname (pathname cld-filepath))
	  (cld-repository (cldm:find-cld-repository repository-name)))
      (cldm:publish-cld cld-repository cld-pathname))))

(defmethod process-repo-command ((command (eql :update)) scope)
  (let ((repository-name (car (clon:remainder))))
    (let ((repository
           (cldm::find-cld-repository repository-name)))
      (cldm::update-cld-repository repository))))      
  
(defmethod process-repo-command ((command (eql :search)) scope)
  (destructuring-bind (repo-name library)
      (clon:remainder)
    (let* ((repo
	    (cldm::find-cld-repository repo-name))
	   (search-result 
	    (cldm::search-cld-repository repo (format nil "name:\"~A\"" library))))
      (loop for elem in search-result
	   do 
	   (format t "~A ~A~%" 
		   (cdr (assoc :name elem))
		   (cdr (assoc :score elem)))))))

(defmethod process-repo-command ((command (eql :clear)) scope)
  (let ((repo-name (first (clon:remainder))))
    (cldm::clear-cache (cldm:find-cld-repository repo-name))))

(defmethod process-repo-command ((command (eql :cache)) scope)
  (let ((repo-name (first (clon:remainder))))
    (cldm::cache-cld-repository (cldm:find-cld-repository repo-name)
				:show-progress t)))

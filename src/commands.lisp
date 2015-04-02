(defpackage :cldm.cmd
  (:use :cl :duologue))

(in-package :cldm.cmd)

(defun stringp* (str)
  (and (stringp str)
       (not (zerop (length str)))
       str))

(defun search-library (library-name &key prefix)
  (loop for repo in (cldm::list-cld-repositories)
       do
	 (format t "~A:~%" (cldm::name repo))
	 (let ((search-result 
		(ignore-errors (cldm::search-cld-repository 
				repo 
				(if prefix
				    (format nil "name:~A*" library-name)
				    (format nil "name:*~A*" library-name))))))
	   (loop for elem in search-result
	      do 
		(format t "~A~@[ - ~A~]~%" 
			(cdr (assoc :name elem))
			(stringp* (cdr (assoc :description elem))))))))

(defun create-cld-template (name &key version cld description author maintainer 
				   licence dependencies repositories keywords &allow-other-keys)
  (let ((library
	 (make-instance 'cldm:library
			:name name
			:cld cld
			:description description
			:author author
			:maintainer maintainer
			:licence licence
			:keywords keywords)))
    (let ((library-version (make-instance 'cldm::library-version 
					  :library library
					  :version version
					  :dependencies dependencies
					  :repositories repositories)))
      (setf (cldm::library-versions library)
	    (list library-version)))
    library))

(defun create-cld-template-interactive (&rest args &key name cld 
						     description author 
						     maintainer licence
						     dependencies
						     repositories 
						     keywords 
						     &allow-other-keys)
  (format t "Are you going to publish the library in a registry?")
  (if (yes-or-no-p)
      (apply #'create-registry-cld-template-interactive args)
      (apply #'create-full-cld-template-interactive args)))

(defun create-registry-cld-template-interactive (&key name cld description 
						   author maintainer licence
						   dependencies repositories
						   keywords &allow-other-keys)
  (error "Implement"))

(defun parse-version-constraints (string)
  (let ((version-or-constraints
	 (esrap:parse '(or semver::version
			cldm::version-constraint-list)
		      string)))
    (if (semver:versionp version-or-constraints)
	(list (list :== version-or-constraints))
	version-or-constraints)))

(defun read-dependency (&key complete)
  (let ((library (prompt "Library: " 
			 :required-p nil
			 :completer (and complete
					 #'library-completer))))
    (say "Library versions can be either a specific semantic version (i.e. \"1.0.0\"), or a comma separated list of versions constraints (i.e. \"> 1.0.0, < 2.0.0 \")." :color :green)
    (when (not library)
      (return-from read-dependency nil))
    (let ((version-constraints 
	   (prompt "Version constraints: " 
		   :parser #'parse-version-constraints
		   :default (list :any)))
	  (cld (progn
		 (say "Explicit CLD addresses in dependencies are usually not needed. Dependencies CLD's are usually grabbed from repositories")
		 (prompt "CLD: " 
			 :parser #'cldm::parse-cld-address
			 :default nil
			 :required-p nil)))
	  (repository (progn
			(say "Specify a dependency custom repository only if the dependency cannot be grabbed otherwise")
			(prompt "Custom repository: " 
				:parser #'cldm::parse-repository-address
				:default nil
				:required-p nil))))
      (make-instance 'cldm::requirement
		     :library-name library
		     :version-constraints version-constraints
		     :cld cld
		     :repository repository))))

(defun confirm-dependency (library-name &key complete)
  (let ((library (ask "Is ~S a dependency? " library-name :default t)))
    (when library
      (let ((version-constraints 
	     (prompt "Version constraints: " 
		     :parser #'parse-version-constraints
		     :default (list :any)))
	    (cld (progn
		   (say "Explicit CLD addresses in dependencies are usually not needed. Dependencies CLD's are usually grabbed from repositories")
		   (prompt "CLD: " 
			   :parser #'cldm::parse-cld-address
			   :default nil
			   :required-p nil)))
	    (repository (progn
			  (say "Specify a dependency custom repository only if the dependency cannot be grabbed otherwise")
			  (prompt "Custom repository: " 
				  :parser #'cldm::parse-repository-address
				  :default nil
				  :required-p nil))))
	(make-instance 'cldm::requirement
		       :library-name library
		       :version-constraints version-constraints
		       :cld cld
		       :repository repository)))))

(defun read-library-version-repository (&key complete)
  (let ((name (prompt "Repository name: "))
	(type (choose "Repository type: " 
		      (list :url :directory 
			    :git :ssh :darcs)
		      :complete complete)))
    (let ((repository-address
	   (ecase type
	     (:url (make-instance 'cldm::url-repository-address
				  :url (prompt-url "Url: " :probe t)))
	     (:directory (make-instance 'cldm::directory-repository-address
					:directory (prompt-pathname "Directory: "
								    :probe t
								    :complete complete)))
	     (:ssh (make-instance 'cldm::ssh-repository-address
				  :address (prompt-url "Address: ")))
	     (:git (make-instance 'cldm::git-repository-address
				  :url (prompt-url "Url: ")
				  :branch (prompt "Branch: " :default "master")
				  :commit (prompt "Commit: " :default nil)
				  :tag (prompt "Tag: " :default nil)))
	     (:darcs (make-instance 'cldm::darcs-repository-address
				    :url (prompt "Url: "))))))
      (make-instance 'cldm::library-version-repository 
		     :name name
		     :address repository-address))))  
	
(defun create-library-version-interactive (library
					   &key 
					     version
					     dependencies
					     complete &allow-other-keys)
  (flet ((read-dependencies ()
	   (say "Enter dependencies.")
	   (loop 
	      :for dependency := (read-dependency)
	      :while dependency
	      :collect dependency))
	 (confirm-dependencies (dependencies)
	   (remove-if #'null
		      (loop 
			 :for dependency :in dependencies
			 :collect (confirm-dependency dependency))))
	 (read-repositories ()
	   (let ((repositories (list (read-library-version-repository :complete complete))))
	     (while "More repositories? " (:default nil)
	       (push (read-library-version-repository :complete complete)
		     repositories))
	     repositories)))
    (let ((version (prompt "Version: " 
			   :parser #'semver:read-version-from-string
			   :default (semver:make-semantic-version 0 0 1)
			   :required-p t))
	  (repositories (read-repositories))
	  (dependencies (append (confirm-dependencies dependencies)
				(read-dependencies))))
      (make-instance 'cldm::library-version
		     :library library
		     :repositories repositories
		     :dependencies dependencies))))			     

(defun create-full-cld-template-interactive (&key name  
					       cld description author 
					       maintainer licence 
					       keywords 
					       complete &allow-other-keys)
  (let ((default-name (or name
			  (car (last 
				(pathname-directory 
				 (osicat:current-directory)))))))
    (let ((name (prompt "Name: " :default default-name))
	  (description (prompt "Description: " :default description))
	  (cld (prompt "CLD: " :required-p nil :default cld))
	  (author (prompt "Author: " :default author)))
      (make-instance 'cldm::library
		     :name name
		     :cld cld
		     :description description
		     :author author
		     :versions (

(defun library-completer (text start end)
  (declare (ignorable start end))
  (flet ((common-prefix (items)
	   (subseq
	    (car items) 0
	    (position
	     nil
	     (mapcar
	      (lambda (i)
		(every (lambda (x)
			 (char= (char (car items) i)
				(char x           i)))
		       (cdr items)))
	      (alexandria:iota (reduce #'min (mapcar #'length items)))))))
	 (search-library (library-name)
	   (remove-duplicates
	    (loop for repo in (cldm::list-cld-repositories)
	       appending
		 (let ((search-result 
			(ignore-errors (cldm::search-cld-repository 
					repo 
					(format nil "name:~A*" library-name)))))
		   (loop for elem in search-result
		      collect (cdr (assoc :name elem)))))
	    :test #'equalp)))
    (let ((els (search-library text)))
      (if (cdr els)
	  (cons (common-prefix els) els)
	  els))))

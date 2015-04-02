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

(defun create-cld-template (name &key cld description author maintainer 
				   licence dependencies repositories keywords &allow-other-keys)
  (make-instance 'cldm:library
		 :name name
		 :cld cld
		 :description description
		 :author author
		 :maintainer maintainer
		 :licence licence
		 :dependencies dependencies
		 :repositories repositories
		 :keywords keywords))

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

(defun create-full-cld-template-interactive (&key name cld description author 
					       maintainer licence 
					       dependencies repositories
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
      (flet ((read-dependencies ()
	       (say "Enter dependencies.")
	       (let ((dependencies nil)
		     (continue t))
		 (loop while continue
		    do
		      (progn
			(let ((library (prompt "Library: " 
					       :required-p nil
					       :completer (and complete
							       #'library-completer))))
			  (if (not (equalp library ""))
			      (progn
				(say "Library versions can be either a specific semantic version (i.e. \"1.0.0\"), or a comma separated list of versions constraints (i.e. \"> 1.0.0, < 2.0.0 \")." :color :green)
				(let ((version-constraints 
				       (prompt "Version constraints: " 
					       :parser #'parse-version-constraints
					       :default "latest"))
				      (cld (progn
					     (say "Explicit CLD addresses in dependencies is usually not needed. Dependencies CLD's are usually grabbed from repositories")
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
				  (let ((dependency (make-instance 'cldm::requirement
								   :library-name library
								   :version-constraints version-constraints
								   :cld cld
								   :repository repository)))
				    (push dependency dependencies))))
			      ;; else
			      (return)))))
		 dependencies)))
	(create-cld-template name
			     :cld cld
			     :description description
			     :author author
			     :dependencies (read-dependencies))))))

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

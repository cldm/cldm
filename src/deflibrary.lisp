(in-package :cldm)

(defmacro deflibrary (name &body options)
  (destructuring-bind (&key author maintainer description
                            licence cld versions tags)
      options
    `(make-instance 'library
                    :name ',(if (symbolp name)
                                (string-downcase (symbol-name name))
                                name)
                    :author ',author
                    :maintainer ',maintainer
                    :description ,description
                    :licence ,licence
                    :cld (parse-cld-address ',cld)
                    :versions (parse-library-versions ',versions)
                    :tags ',tags)))

(defun parse-library-versions (versions)
  (let ((parsed-versions))
    (flet ((find-library-version-to-extend (version)
	     (or (find version parsed-versions
		       :key #'version
		       :test #'version=)
		 (error "Version ~A not found when trying to extend"
			version)))) 
      (loop for version in (reverse versions)
	 do
	   (destructuring-bind (version-keyword version
						&key stability description repositories depends-on
						extends
						add-repositories
						remove-repositories
						add-dependencies
						remove-dependencies)
	       version
	     (assert (equalp version-keyword :version) nil "Invalid token ~A" version-keyword)
	     (let ((parsed-version (read-version-from-string version)))
		   
	       (if extends
		   ;; The library version extends another
		   (let* ((parsed-extends-version (read-version-from-string extends))
			  (version-to-extend (find-library-version-to-extend parsed-extends-version))
			  (library-version-copy (copy-library-version version-to-extend)))
		     (setf (version library-version-copy) parsed-version)
		     (setf (stability library-version-copy) stability)
		     (setf (description library-version-copy) description)
		     (if repositories
			 ;; Set the repositories of the version copy
			   (setf (repositories library-version-copy)
				 (loop for repository in (parse-version-repositories repositories)
				      do (setf (library-version repository) library-version-copy)
				      collect repository))			   
			 ; else
			 (progn
			   (loop for repository in (parse-version-repositories add-repositories)
			      do (add-repository library-version-copy repository))
			   (loop for repository-name in remove-repositories
			      do (remove-repository library-version-copy repository-name))))
		     (if depends-on
			 (setf (dependencies library-version-copy)
			       (parse-version-dependencies depends-on))
			 ; else
			 (progn
			   (loop for dependency in (parse-version-dependencies add-dependencies)
			      do (add-dependency library-version-copy dependency))
			   (loop for library-name in remove-dependencies
			      do (remove-dependency library-version-copy library-name))))
		     (push library-version-copy parsed-versions))
		   ;; else, just create the library version
		   (push
		    (make-instance 'library-version
				   :version parsed-version
				   :stability stability
				   :description description
				   :repositories (parse-version-repositories repositories)
				   :dependencies (parse-version-dependencies depends-on))
		    parsed-versions)))))
      parsed-versions)))

(defun parse-version-repositories (repositories)
  (loop for repository in repositories
     collect
       (parse-library-version-repository repository)))

(defun parse-library-version-repository (repository)
  (destructuring-bind (name address) repository
    (make-instance 'library-version-repository
		   :name name
		   :address (parse-version-repository-address address))))

(defun parse-version-dependencies (dependencies)
  (loop for dependency in dependencies
     collect
       (cond
	 ((listp dependency)
	  (destructuring-bind (library-name &key version-constraints cld) dependency
	    (apply #'make-instance 'requirement
		   `(:library-name ,(if (symbolp library-name)
					(string-downcase (symbol-name library-name))
					library-name)
				   ,@(when version-constraints
					   (list :version-constraints
						 (loop for version-constraint in version-constraints
						    collect (list (first version-constraint)
								  (read-version-from-string
								   (second version-constraint))))))
				   ,@(when cld
					   (list :cld (parse-cld-address cld)))))))
	 ((symbolp dependency)
	  (make-instance 'requirement
			 :library-name (string-downcase (symbol-name dependency))))
	 ((stringp dependency)
	  (read-requirement-from-string dependency))
	 (t (error "~A is not a valid dependency spec" dependency)))))

(defun parse-version-repository-address (address)
  (cond
    ((pathnamep address)
     (make-instance 'directory-repository-address
		    :directory address))
    ((and (listp address)
          (equalp (first address) :directory))
     (make-instance 'directory-repository-address
		    :directory (second address)))
    ((and (listp address)
          (equalp (first address) :git))
     (destructuring-bind (url &rest args)
         (rest address)
       (apply #'make-instance 'git-repository-address
	      `(:url ,url
		     ,@args))))
    ((and (listp address)
          (equalp (first address)
                  :url))
     (make-instance 'url-repository-address
		    :url (second address)))
    (t (error "Invalid repository ~A" address))))


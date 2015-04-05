#|

\chapter{Libraries definition}

\ignore{
|#

(in-package :cldm)

#|
}

\section{deflibrary macro}

Libraries are defined used the \emph{deflibrary} macro.

There are two different semantics for library definition.

\begin{itemize}

\item \textbf{Standard or full:}  The library definition contains all of the library versions descriptions listed after the \verb':versions' keyword.

\item \textbf{To publish in a registry:} The library definition contains the current library version description only. Descriptions of older library versions can be found in older versions of the CLD file (in VCS history). This kind of definition is meant to be published in a CLD registry.

\end{itemize}
|#

(defmacro deflibrary (name &body options)
  (destructuring-bind (&key author maintainer description
                            licence cld versions keywords
			    homepage bug-reports documentation source-repository &allow-other-keys)
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
		    :homepage ,homepage
		    :bug-reports ,bug-reports
		    :documentation ,documentation
		    :source-repository ,source-repository
                    :versions (parse-library-versions ',versions)
                    :keywords ',keywords)))

(defun parse-library (librarydef)
  (destructuring-bind (deflibrary name &body options) librarydef
    (assert (or (equalp deflibrary 'cldm:deflibrary)
		(equalp deflibrary 'cl-user::deflibrary)))
    (destructuring-bind (&key author maintainer description
			      licence cld versions keywords 
			      homepage bug-reports documentation source-repository
			      &allow-other-keys)
	options
      (check-type author (or null string))
      (check-type maintainer (or string null))
      (check-type description (or null string))
      (check-type licence (or null string))
      (check-type keywords (or cons null))
      (make-instance 'library
		     :name (if (symbolp name)
			       (string-downcase (symbol-name name))
			       name)
		     :author author
		     :maintainer maintainer
		     :description description
		     :licence licence
		     :cld (parse-cld-address cld)
		     :homepage homepage
		     :bug-reports bug-reports
		     :documentation documentation
		     :source-repository source-repository
		     :keywords keywords
		     :versions (parse-library-versions versions)))))

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
		   :address (parse-repository-address address))))

(defun parse-version-dependency (dependency)
  (cond
    ((listp dependency)
     (destructuring-bind (library-name &key version version-constraints cld repository) dependency
       (when version
	 (push (list :== (read-version-from-string version)) version-constraints))
       (apply #'make-instance 'requirement
	      `(:library-name ,(if (symbolp library-name)
				   (string-downcase (symbol-name library-name))
				   library-name)
			      ,@(when version-constraints
				      (list :version-constraints
					    (loop for version-constraint in version-constraints
					       collect (list (first version-constraint)
							     (cond
							       ((stringp (second version-constraint))
								(read-version-from-string (second version-constraint)))
							       ((versionp (second version-constraint))
								(second version-constraint))
							       (t (error "Invalid version ~A" (second version-constraint))))))))
			      ,@(when cld
				      (list :cld (parse-cld-address cld)))
			      ,@(when repository
				      (list :repository (parse-repository-address repository)))))))
    ((symbolp dependency)
     (make-instance 'requirement
		    :library-name (string-downcase (symbol-name dependency))))
    ((stringp dependency)
     (read-requirement-from-string dependency))
    (t (error "~A is not a valid dependency spec" dependency))))

(defun parse-version-dependencies (dependencies)
  (loop for dependency in dependencies
     collect
       (parse-version-dependency dependency)))

(defun parse-repository-address (address)
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
	  (equalp (first address) :darcs))
     (make-instance 'darcs-repository-address 
		    :url (second address)))
    ((and (listp address)
          (equalp (first address)
                  :url))
     (make-instance 'url-repository-address
		    :url (second address)))
    ((and (listp address)
	  (equalp (first address)
		  :ssh))
     (make-instance 'ssh-repository-address
		    :address (second address)))
    (t (error "Invalid repository ~A" address))))

(defmethod unparse-cld-address ((cld-address git-cld-address))
  (list :git (git-url cld-address) (git-branch cld-address)))

(defmethod unparse-cld-address ((cld-address http-cld-address))
  (cld-url cld-address))

(defun unparse-library-version-repository (lvrepo)
  (list (name lvrepo) 
	(unparse-repository-address (repository-address lvrepo))))

(defmethod unparse-repository-address ((repository-address git-repository-address))
  (list :git (url repository-address)))

(defmethod unparse-repository-address ((repository-address ssh-repository-address))
  (list :ssh (address repository-address)))

(defmethod unparse-repository-address ((repository-address url-repository-address))
  (list :url (url repository-address)))

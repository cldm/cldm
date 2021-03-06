#|

\chapter{Libraries}

\ignore{
|#

(in-package :cldm)

#|
}
|#

#|

\section{Overview}

When a library is created, it is registered on \emph{*libraries*} hash-table, unless \emph{*register-libraries*} is bound to \emph{NIL}
|#

(defparameter *libraries* (make-hash-table :test #'equalp) "Registered libraries table")

(defparameter *if-already-registered-library* :append "What to do if a library is already registered. One of :append, :replace, :error, :ignore")
(defparameter *latest-registered-library* nil "The latest registered library")
(defparameter *register-libraries* t "Libraries are registered after creation iff it is T")

#|
A library is a mutable entity that holds the name, author, maintainer, description, etc. And each library has a list of versions.

Note that libraries are mutable entities. In other systems, libraries are just a collection of library versions. They are immutable. This could be a better design, but it is not implemented like that at the moment.
|#

(defclass library ()
  ((name :initarg :name
         :initform (error "Provide the library name")
         :accessor library-name
         :documentation "The library name")
   (author :initarg :author
           :initform nil
           :accessor library-author
           :documentation "The library author")
   (maintainer :initarg :maintainer
               :initform nil
               :accessor library-maintainer
               :documentation "The library maintainer")
   (description :initarg :description
                :initform nil
                :accessor library-description
                :documentation "The library description")
   (licence :initarg :licence
            :initform nil
            :accessor library-licence
            :documentation "The library licence")
   (homepage :initarg :homepage
	     :initform nil
	     :accessor library-homepage
	     :documentation "The library home page url")
   (documentation :initarg :documentation
		  :initform nil
		  :accessor library-documentation
		  :documentation "The url where the library documentation is")
   (bug-reports :initarg :bug-reports
		:initform nil
		:accessor library-bug-reports
		:documentation "The url where library bug reports are made")
   (source-repository :initarg :source-repository
		      :initform nil
		      :accessor library-source-repository
		      :documentation "The source repository url. For documentation purposes only.")
   (cld :initarg :cld
        :initform (error "Provide the cld")
        :accessor library-cld
        :documentation "The library meta description address. Can be a pathname or an url")
   (versions :initarg :versions
             :initform (error "Provide a library version at least")
             :accessor library-versions
             :documentation "The library versions")
   (keywords :initarg :keywords
	     :initform nil
	     :accessor library-keywords
         :documentation "Library keywords"))
  (:documentation "A library meta description"))

#|

\section{Some library functions}

|#

(defun find-library (name &optional (error-p t))
  "Find a library with name"
  (or (gethash name *libraries*)
      (when error-p
        (error "Library ~A not found" name))))

(defun find-library-versions (library requirement)
  "Find library versions that satisfy the requirement"
  (loop for library-version in (library-versions library)
       when (equalp (library-version-matches library-version requirement) :match)
       collect library-version))

(defun list-all-libraries ()
  "List all registered libraries"
  (loop for library being the hash-values of *libraries*
     collect library))

(defun register-library (library &key (if-already-registered *if-already-registered-library*))
  "Registers a library"
  (when *register-libraries*
    (check-type if-already-registered (member :append :replace :error :ignore))
    (aif (find-library (library-name library) nil)
	 (ecase if-already-registered
	   (:error (error "The library ~A has already been registered" (library-name library)))
	   (:replace (setf (gethash (library-name library) *libraries*) library))
	   (:ignore nil)
	   (:append (append-to-library library it)))
					;else
	 (setf (gethash (library-name library) *libraries*) library))
    (setf *latest-registered-library* library)))

(defun clear-registered-libraries ()
  "Clear registered libraries"
  (setf *libraries* (make-hash-table :test #'equalp)))

(defun append-to-library (library target-library)
  "Appends library versions found in LIBRARY to TARGET-LIBRARY"
  (setf (library-versions target-library)
	(append (library-versions target-library)
		(library-versions library))))

(defun find-library-version (library version &optional (error-p t))
  "Find a library specific version"
  (loop for library-version in (library-versions library)
     when (version= (version library-version) version)
     do (return-from find-library-version library-version))
  (when (equalp version :max-version)
    (let ((library-version (first (library-versions library))))
      (when library-version
	(return-from find-library-version library-version))))
  (when error-p
    (error "~A version ~A not found" library version)))

(defmethod library-versions ((library library))
  "Returns the library versions, sorted by version"
  (sort (slot-value library 'versions)
	#'version>= 
	:key #'version))

(defmethod latest-library-version ((library library))
  (first (library-versions library)))

(defmethod print-object ((library library) stream)
  (print-unreadable-object (library stream :type t :identity t)
    (format stream "~A (~A)"
            (library-name library)
            (library-cld library))))

(defmethod initialize-instance :after ((library library) &rest initargs)
  (declare (ignore initargs))

  ;; Assign the library to the versions
  (loop for version in (library-versions library)
     do (setf (library version) library))

  ;; Register the library
  (register-library library))

#|
\section{Library versions}

Library versions hold information about a particular version of a library, like its stability, repositories (places where the library version can be downloaded from), dependencies and conflicts with other libraries, etc.

|#

(defclass library-version ()
  ((library :initarg :library
            :initform nil
            :accessor library
            :documentation "The library")
   (version :initarg :version
            :initform (error "Provide the version")
            :accessor version
            :documentation "The library version")
   (description :initarg :description
                :initform nil
                :accessor description
                :documentation "Library version description")
   (stability :initarg :stability
              :initform nil
              :accessor stability
              :documentation "Library version stability. One of :stable, :beta, :alpha")
   (repositories :initarg :repositories
                 :initform (error "Provide a repository at least")
                 :accessor repositories
                 :documentation "Library version repositories")
   (custom-repositories :initarg :custom-repositories
			:initform nil
			:accessor custom-repositories
			:documentation "Custom package repositories to use.

By default CLDM just uses libraries repositories. By specifying custom repositories you can get packages from elsewhere.

Repositories are not resolved recursively. Repository declarations of dependencies are ignored.")
   (dependencies :initarg :dependencies
                 :initform nil
                 :accessor dependencies
                 :documentation "The library version dependencies (list of requirement objects)")
   (provides :initarg :provides
	     :initform nil
	     :accessor provides
	     :documentation "List of requirements the library provides")
   (conflicts :initarg :conflicts
	      :initform nil
	      :accessor conflicts
	      :documentation "List of requirements the library is in conflict with")
   (replaces :initarg :replaces
	     :initform nil
	     :accessor replaces
	     :documentation "List of requirements the library replaces")
   (suggests :initarg :suggests
	     :initform nil
	     :accessor suggests
	     :documentation "List of requirements the library suggests"))
  (:documentation "A library version description"))

(defmethod initialize-instance :after ((library-version library-version) &rest initargs)
  (declare (ignore initargs))

  ;; Validate the version has a repository at least
  (assert (plusp (length (repositories library-version)))
          nil
          "~A version needs to define a repository at least" (version library-version))

  ;; Assign the version to the repositories
  (loop for repository in (repositories library-version)
     do (setf (library-version repository) library-version)))

(defmethod library-name ((library-version library-version))
  (library-name (library library-version)))

#|

\section{Parsing}

|#

(defrule requirement-type (or "depends" "provides" "suggests" "conflicts" "replaces")
  (:function (lambda (match)
	       (make-keyword (string-upcase match)))))

(defrule library-requirements
    (and requirement-type
	 spaces
	 #\(
	 distribution-constraint
	 (* (and #\, spaces distribution-constraint))
	 #\))
  (:function (lambda (match)
	       (destructuring-bind (requirement-type
				    spaces
				    open-paren
				    constraint
				    constraints
				    close-paren) match
		 (list requirement-type (cons constraint (mapcar #'third constraints)))))))

(defrule library (and library-unique-name (* (and #\; spaces library-requirements)))
  (:function (lambda (match)
	       (destructuring-bind (name requirements) match
		   (list name (mapcar #'third requirements))))))

(defun read-library-version-from-string (string)
  (destructuring-bind (unique-name requirements)
      (parse 'library string)
    (make-instance 'library-version
		   :name (first unique-name)
		   :version (second unique-name)
		   :dependencies
		   (let ((depends (cadar
				   (remove-if-not (lambda (reqs)
						    (equalp (first reqs) :depends))
						  requirements))))
		     (loop for constraint in depends
			collect (progn
					;(break "~A" constraint)
				  (make-requirement (first constraint) (second constraint)))))
		   :provides
		   (let ((provides (cadar
				    (remove-if-not (lambda (reqs)
						     (equalp (first reqs) :provides))
						   requirements))))
		     (loop for constraint in provides
			collect (make-requirement (first constraint) (second constraint))))
		   :conflicts
		   (let ((conflicts (cadar
				     (remove-if-not (lambda (reqs)
						      (equalp (first reqs) :conflicts))
						    requirements))))
		     (loop for constraint in conflicts
			collect (make-requirement (first constraint) (second constraint))))
		   :suggests
		   (let ((suggests (cadar
				    (remove-if-not (lambda (reqs)
						     (equalp (first reqs) :suggests))
						   requirements))))
		     (loop for constraint in suggests
			collect (make-requirement (first constraint) (second constraint))))
		   :replaces
		   (let ((replaces (cadar
				    (remove-if-not (lambda (reqs)
						     (equalp (first reqs) :replaces))
						   requirements))))
		     (loop for constraint in replaces
			collect (make-requirement (first constraint) (second constraint)))))))

#|

\section{Printing}

|#

(defmethod library-version-unique-name ((library-version library-version))
  (format nil "~A~@[-~A~]"
	  (library-name library-version)
	  (when (version library-version)
	    (print-version-to-string (version library-version)))))

(defun valid-library-name-p (string)
  (not (null
	(ignore-errors
	  (parse 'library-unique-name string)))))

(defmethod describe-object ((library-version library-version) stream)
  (format stream "~A library~%~%" (library-version-unique-name library-version))
  (format stream "Dependencies: ~{~a~^, ~}~%"
	  (or
	   (mapcar #'print-requirement-to-string
		   (dependencies library-version))
	   (list "None")))
  (format stream "Provides: ~{~a~^, ~}~%"
	  (or
	   (mapcar #'print-requirement-to-string
		   (provides library-version))
	   (list "None")))
  (format stream "Conflicts: ~{~a~^, ~}~%"
	  (or
	   (mapcar #'print-requirement-to-string
		   (conflicts library-version))
	   (list "None")))
  (format stream "Replaces: ~{~a~^, ~}~%"
	  (or
	   (mapcar #'print-requirement-to-string
		   (replaces library-version))
	   (list "None")))
  (format stream "Suggests: ~{~a~^, ~}~%"
	  (or
	   (mapcar #'print-requirement-to-string
		   (suggests library-version))
	   (list "None"))))

(defun print-library-version (library-version stream)
  (format stream "~A" (library-version-unique-name library-version))
  (when (dependencies library-version)
    (format stream "; depends (~{~a~^, ~})"
	    (mapcar #'print-requirement-to-string
		    (dependencies library-version))))
  (when (provides library-version)
    (format stream "; provides (~{~a~^, ~})"
	    (mapcar #'print-requirement-to-string
		    (provides library-version))))
  (when (replaces library-version)
    (format stream "; replaces (~{~a~^, ~})"
	    (mapcar #'print-requirement-to-string
		    (replaces library-version)))))

(defun print-library-version-to-string (library-version)
  (with-output-to-string (s)
    (print-library-version library-version s)))

(defmethod print-object ((library-version library-version) stream)
  (print-unreadable-object (library-version stream :type t :identity t)
    (print-library-version library-version stream)))

(defun print-library-definition (library &optional stream)
  (let ((*print-case* :downcase))
    (format stream "~S" (library-definition library))))

(defun library-definition (library)
  `(cldm:deflibrary ,(intern (string-upcase (library-name library)))
     ,@(when (library-cld library)
	     (list :cld (cldm::unparse-cld-address (library-cld library))))
     ,@(when (library-description library)
	     (list :description (library-description library)))
     ,@(when (library-author library)
	     (list :author (library-author library)))
     ,@(when (library-licence library)
	     (list :licence (library-licence library)))
     :versions ,(mapcar #'library-version-definition 
			(library-versions library))))

(defun library-version-definition (library-version)
  `(:version ,(semver:print-version-to-string (version library-version))
	     :repositories ,(mapcar #'cldm::unparse-library-version-repository
				    (repositories library-version))
	     :depends-on ,(mapcar #'cldm::print-requirement-to-string 
				  (dependencies library-version))))


(defmethod library-version-matches ((library-version library-version) (requirement requirement))
  "Checks whether the candidate library-version matches the requirement, either directly or through provides.

  Returns: - :match-name if only the name matches
           - :match if both name and version match
           - :match-provide if the match is through the library-version provides
           - nil if there's no match"
  (let ((library-version-requirement (read-requirement-from-library-version-string
				      (library-version-unique-name library-version))))
    (if (equalp (library-name requirement)
		(library-name library-version))
      	(if (or (requirement-universal-p requirement)
		(requirement-matches library-version-requirement requirement))
	    (values :match nil)
					;else
	    (values :match-name (library-name requirement)))
					;else
	(progn
	  ;; look for providers
	  (loop for provide in (provides library-version)
	     when (requirement-matches requirement provide)
	     do (return-from library-version-matches
		  (values :match-provide provide)))

	  ;; look for replaces
	  (loop for replace in (replaces library-version)
	     when (requirement-matches requirement replace)
	     do (return-from library-version-matches
		  (values :match-replace replace)))))))

(defun library-version= (lib1 lib2)
  (and (equalp (library-version-unique-name lib1)
	       (library-version-unique-name lib2))
       (set-equal (dependencies lib1)
		  (dependencies lib2)
		  :test #'requirement=)
       (set-equal (provides lib1)
		  (provides lib2)
		  :test #'requirement=)
       (set-equal (suggests lib1)
		  (suggests lib2)
		  :test #'requirement=)
       (set-equal (replaces lib1)
		  (replaces lib2)
		  :test #'requirement=)
       (set-equal (conflicts lib1)
		  (conflicts lib2)
		  :test #'requirement=)))

(defun copy-library-version (library-version)
  (make-instance 'library-version
		 :library (library library-version)
		 :version (version library-version)
		 :description (description library-version)
		 :stability (stability library-version)
		 :repositories (copy-list (repositories library-version))
		 :dependencies (copy-list (dependencies library-version))
		 :provides (copy-list (provides library-version))
		 :conflicts (copy-list (conflicts library-version))
		 :replaces (copy-list (replaces library-version))
		 :suggests (copy-list (suggests library-version))))

(defun find-library-version-repository (library-version repository-name)
  (find repository-name (repositories library-version)
	:key #'name))	



(defmethod remove-repository ((library-version library-version)
			      repository-name)
  "Removes the repository with name REPOSITORY-NAME from LIBRARY-VERSION.
   A repository with that name has to exists in the library version repositories."
  
  (assert (find repository-name (repositories library-version)
		:key #'name :test #'equalp) nil "Repository ~A not found in ~A"
		repository-name library-version)
  (setf (repositories library-version)
	(remove repository-name
		(repositories library-version)
		:key #'name :test #'equalp)))

(defmethod add-dependency ((library-version library-version)
			   (dependency requirement))
  "Add a dependency to a library version.
   If a dependency on the same library exists, it is replaced."

  (if (find (library-name dependency)
	    (dependencies library-version)
	    :key #'library-name
	    :test #'equalp)
      ;; There's a dependency on the same library, replace it
      (setf (dependencies library-version)
	    (cons dependency
		  (remove (library-name dependency)
			  (dependencies library-version)
			  :key #'library-name :test #'equalp)))
      ;; else, just add the dependency
      (setf (dependencies library-version)
	    (cons dependency (dependencies library-version)))))

(defmethod remove-dependency ((library-version library-version)
			      library-name)
  "Removes a dependency to LIBRARY-NAME from LIBRARY-VERSION.
   If the dependency does not exists, an error is signaled"

  (assert (find library-name (dependencies library-version)
		:key #'library-name :test #'equalp) nil "Dependency to ~A not found in ~A"
		library-name
		library-version)
  
  (setf (dependencies library-version)
	(remove library-name
		(dependencies library-version)
		:key #'library-name :test #'equalp)))

(defclass installed-library-version ()
  ((name :initarg :name
	 :initform (error "Provide the library name")
	 :accessor name)
   (version :initarg :version
	    :initform (error "Provide the library version")
	    :accessor version)
   (install-directory :initarg :install-directory
		      :initform (error "Provide the install directory")
		      :accessor install-directory)
   (repository :initarg :repository
	       :initform (error "Provide the repository")
	       :accessor repository)
   (checksum  :initarg :checksum
	      :accessor checksum
	      :initform nil)))

(defmethod print-object ((ilv installed-library-version) stream)
  (print-unreadable-object (ilv stream :type t :identity t)
    (format stream "~A-~A"
	    (name ilv)
	    (print-version-to-string (version ilv)))))

(defmethod library-name ((library-version installed-library-version))
  (name library-version))

(defmethod library-version-unique-name ((library-version installed-library-version))
  (format nil "~A-~A" 
	  (name library-version)
	  (semver:print-version-to-string (version library-version))))
#|

\section{Library versions repositories}

Library versions repositories are those specified with \emph{:repositories} in \emph{deflibrary} and indicate the list of location from where the library version can be downloaded.

They have a \emph{name} and a \emph{repository address}.

|#

(defclass library-version-repository ()
  ((library-version :initarg :library-version
                    :initform nil
                    :accessor library-version
                    :documentation "The library version of the repository")
   (name :initarg :name
         :initform (error "Provide the repository name")
         :accessor name
         :documentation "The repository name")
   (address :initarg :address
            :initform (error "Provide the repository address")
            :accessor repository-address
            :documentation "The repository address. Can be a pathname, an url or a git reference"))
  (:documentation "A library version repository (a location from which the library version can be downloaded)"))

(defmethod print-object ((version-repository library-version-repository) stream)
  (print-unreadable-object (version-repository stream :type t :identity t)
    (print-library-version (library-version version-repository)
                           stream)
    (format stream " ~A ~A"
            (name version-repository)
            (repository-address version-repository))))

(defmethod add-repository ((library-version library-version)
			   (repository library-version-repository))
  "Adds REPOSITORY to LIBRARY-VERSION.

   Args: - LIBRARY-VERSION (library-version): The library version.
         - REPOSITORY (repository): The repository.

   If the library version contains a repository with the given repository name, replaces the repository"
  
  (if (find (name repository)
	    (repositories library-version)
	    :key #'name :test #'equalp)
      ;; There's a repository with the same name, replace it
      (setf (repositories library-version)
	    (cons repository
		  (remove (name repository)
			  (repositories library-version)
			  :key #'name :test #'equalp)))
      ;; else, just add the repository
      (push repository (repositories library-version)))
  (setf (library-version repository) library-version))

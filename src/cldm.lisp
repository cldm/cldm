(in-package #:cldm)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (pushnew :cldm *features*))

(defun info-msg (msg &rest args)
  (apply #'format t (cons msg args)))

(defun debug-msg (msg &rest args)
  (when *debug-mode*
    (apply #'format t (cons msg args))))

(defun verbose-msg (msg &rest args)
  (when (or *verbose-mode* *debug-mode*)
    (apply #'format t (cons msg args))))

(defun find-library-cld (library-name &optional (cld-repositories (list-cld-repositories)))
  (loop
     for cld-repository in (list-cld-repositories)
     for cld = (find-cld cld-repository
                         library-name)
     when cld
     return (values cld cld-repository)))

(defun library-versions-to-install (library-version)
  "Calculates the library versions to install for a given LIBRARY-VERSION"

  ;; Load libraries metadata
  (load-library-version library-version)

  ;; Calculate list of library-versions involved
  (let ((library-versions-involved
         (calculate-library-versions-involved library-version)))

    (pbo-solve-library-versions library-version
                                library-versions-involved)))

(defun setup (library-name &optional version (libraries-directory *libraries-directory*))
  "Setup an already loaded library and its dependencies"
  (let ((library (find-library library-name)))
    (verbose-msg "Loading ~A.~%" library)
    (let ((library-version (if version
                               (find-library-version library version)
                               (first (library-versions library)))))
      ;; Load libraries metadata
      (load-library-version library-version)

      ;; Calculate list of library-versions involved
      (let ((library-versions-involved
             (calculate-library-versions-involved library-version)))

        (let ((library-versions (pbo-solve-library-versions library-version
                                                            library-versions-involved)))

          (info-msg "Libraries to load: ~{~A~^, ~}~%"
                    (mapcar #'library-version-unique-name library-versions))

          ;; Check the version existance and download if not
          (loop for version in library-versions
             do
               (install-library-version version libraries-directory))))))
  (verbose-msg "Done.~%"))

(defun load-library (library-name
                     &key
                       version
                       cld
                       (verbose *verbose-mode*)
                       (solving-mode *solving-mode*)
                       (clean-asdf-environment *clean-asdf-environment*)
                       (libraries-directory *libraries-directory*)
		       (clear-registered-libraries t))
  "Installs a library if not present, and loads it in the current lisp image"
  (install-library library-name
                   :version version
                   :cld cld
                   :verbose verbose
                   :solving-mode solving-mode
                   :libraries-directory libraries-directory
		   :clear-registered-libraries clear-registered-libraries)
  (when clean-asdf-environment
    (setf asdf:*central-registry* nil)
    (asdf:clear-source-registry)
    (asdf:clear-configuration)
    (setf asdf:*system-definition-search-functions* (list 'ASDF/FIND-SYSTEM:SYSDEF-CENTRAL-REGISTRY-SEARCH)))
  (asdf:load-system library-name :force-not (asdf:registered-systems)))

(defun install-library (library-name
                        &key
                          version
                          cld
                          (verbose *verbose-mode*)
                          (solving-mode *solving-mode*)
                          (libraries-directory *libraries-directory*)
			  (clear-registered-libraries t))
  "Tries to find a cld for the library and load it.
   Then setup the library and its dependencies"
  (let ((*verbose-mode* verbose)
        (*solving-mode* solving-mode))
    (when clear-registered-libraries
      (clear-registered-libraries))
    (with-download-session ()
      (let ((cld (and cld (load-cld (parse-cld-address cld)))))
	(if cld
	    (setup library-name version libraries-directory)
	    ;; else
	    (if (find-library library-name nil)
		(setup library-name version libraries-directory)
		;; else
		(progn
		  (loop
		     for cld-repository in (list-cld-repositories)
		     while (not cld)
		     do
		       (let ((repository-cld (find-cld cld-repository
						       library-name)))
			 (setf cld (and repository-cld
					(load-cld repository-cld)))
			 (when cld
			   (verbose-msg "~A cld found in ~A~%"
					library-name
					cld-repository))))
		  (if cld
		      (progn
			(setup library-name version)
			t)
		      (error "Couldn't find a cld for ~S library~%" library-name)))))))))

(defmethod load-project ((directory pathname)
                         &key
                           version
                           libraries-directory
                           (verbose *verbose-mode*)
                           (solving-mode *solving-mode*)
                           (clean-asdf-environment *clean-asdf-environment*)
			   (clear-registered-libraries t))
  (load-project (load-project-from-directory directory)
                :version version
                :libraries-directory libraries-directory
                :verbose verbose
                :solving-mode solving-mode
                :clean-asdf-environment clean-asdf-environment
		:clear-registered-libraries clear-registered-libraries))

(defmethod load-project ((project project)
                         &key
                           version
                           libraries-directory
                           (verbose *verbose-mode*)
                           (solving-mode *solving-mode*)
                           (clean-asdf-environment *clean-asdf-environment*)
			   (clear-registered-libraries t))
  "Install a project dependencies and load the project in the current lisp image"

  (install-project project
                   :version version
                   :verbose verbose
                   :solving-mode solving-mode
                   :libraries-directory libraries-directory
		   :clear-registered-libraries clear-registered-libraries)
  (when clean-asdf-environment
    (setf asdf:*central-registry* nil)
    (asdf:clear-source-registry)
    (asdf:clear-configuration)
    (setf asdf:*system-definition-search-functions* (list 'ASDF/FIND-SYSTEM:SYSDEF-CENTRAL-REGISTRY-SEARCH)))
  (asdf:load-system (library-name (library project))
                    :force-not (asdf:registered-systems)))

(defun install-project-from-ilv (project libraries-directory)
  "Install project form library versions in the lock file"
  (loop for ilv in (installed-library-versions project)
     do (install-library-version ilv libraries-directory)))       

(defmethod install-project ((project project)
                            &key
                              version
                              libraries-directory
                              (verbose *verbose-mode*)
                              (solving-mode *solving-mode*)
			      (clear-registered-libraries t))
  "Installs a CLDM project dependencies"

  (let ((*verbose-mode* verbose)
        (*solving-mode* solving-mode)
        (version (or version
                     (project-version project)))
        (libraries-directory (or libraries-directory
                                 (libraries-directory project)
                                 *local-libraries-directory*)))
    (verbose-msg "Loading ~A.~%" project)
    (when clear-registered-libraries
      (clear-registered-libraries))
    (verbose-msg "Removing installed libraries...~%")
    (remove-directory libraries-directory)
    (ensure-directories-exist libraries-directory)
    (verbose-msg "Installing project libraries...~%")
    (if (installed-library-versions project)
	;; If there's a lock file, install versions specified there
	(install-project-from-ilv project libraries-directory)
	;; else, calculate the dependencies
	(with-download-session ()
	  (let ((library-version (if version
				     (find-library-version (library project) version)
				     (first (library-versions (library project))))))
	    ;; Load libraries metadata
	    (load-library-version library-version)

	    ;; Calculate list of library-versions involved
	    (let ((library-versions-involved
		   (calculate-library-versions-involved library-version)))

	      (let ((library-versions (pbo-solve-library-versions library-version
								  library-versions-involved)))
		;; Remove the project library from the library versions list
		(setf library-versions (remove (library-name (library project)) library-versions
					       :key #'library-name
					       :test #'equalp))

		(info-msg "Libraries to install: ~{~A~^, ~}~%" (mapcar #'library-version-unique-name library-versions))

		;; Check the version existance and download if not
		(let ((installed-library-versions ()))
		  (loop for version in library-versions
		     do
		       (let ((installed-library-version
			      (install-library-version version libraries-directory)))
			 (push installed-library-version installed-library-versions)))
		  (create-lock-file project installed-library-versions)))))
	  (verbose-msg "Done.~%")
	  t))))

(defmethod update-project ((project project)
                           &key
                             version
                             libraries-directory
                             (verbose *verbose-mode*)
                             (solving-mode *solving-mode*)
			     (clear-registered-libraries t))
  "Updates a CLDM project dependencies"

  (let ((*verbose-mode* verbose)
        (*solving-mode* solving-mode)
        (version (or version
                     (project-version project)))
	(libraries-directory (or libraries-directory
                                 (libraries-directory project)
                                 *local-libraries-directory*)))
    (verbose-msg "Loading ~A.~%" project)
    (when clear-registered-libraries
      (clear-registered-libraries))
    (verbose-msg "Updating project dependencies...~%")
    (let ((project-library-versions (installed-library-versions project)))
      (with-download-session ()
	(let ((library-version (if version
				   (find-library-version (library project) version)
				   (first (library-versions (library project))))))
	  ;; Load libraries metadata
	  (load-library-version library-version)

	;; Calculate list of library-versions involved
	(let ((library-versions-involved
	       (calculate-library-versions-involved library-version)))
	  
	  (let ((library-versions (pbo-solve-library-versions library-version
							      library-versions-involved)))
	    ;; Remove the project library from the library versions list
	    (setf library-versions (remove (library-name (library project)) library-versions
					   :key #'library-name
					   :test #'equalp))

	    (info-msg "Libraries to install/update: ~{~A~^, ~}~%" (mapcar #'library-version-unique-name library-versions))

	    ;; Remove the unused project dependencies
	    (loop for project-library-version in project-library-versions
		 do
		 (when (not (find (library-version-unique-name project-library-version)
				  library-versions
				  :key #'library-version-unique-name
				  :test #'equalp))
		   (verbose-msg "Removing ~A...~%" 
				(library-version-unique-name project-library-version))
		   (remove-library-version project-library-version libraries-directory)))	  

	    ;; Check the version existance and download if not
	    (let ((installed-library-versions ()))
	      (loop for version in library-versions
		 do
		   (let ((updated-library-version
			  (update-library-version version project)))
		     (if updated-library-version
			 (push updated-library-version installed-library-versions)
			 ;; else
			 (let ((installed-library-version
				(find-installed-library-version
				 project
				 (library-name version))))
			   (push installed-library-version installed-library-versions)))))
	      ;; Create the lock file
	      (create-lock-file project installed-library-versions))))))
      (verbose-msg "Done.~%")
      t)))

(defun load-library-version (library-version &key (if-already-loaded *if-already-loaded-cld*))
  "Load a library version dependencies clds"
  (verbose-msg "Loading ~A.~%" library-version)
  (labels ((load-dependency (dependency)
             "Load a dependency cld, and the cld of dependencies of the dependency"
             (let* ((library (find-library (library-name dependency)))
                    (library-versions (find-library-versions library dependency)))
               (loop for library-version in library-versions
                  do (load-library-version library-version :if-already-loaded if-already-loaded))))
           (load-dependency-cld (dependency)
	     ; To load a dependency cld, we try looking in repositories first, and, if we couldn't find
             ; a cld there, we try to load the cld specified in the dependency. This is so that we can give
             ; the user an opportunity to have some control of which cld files he wants to take priority over others
             ; by adding a cld repository to *cld-repositories*
	     (let (cld)
	       (loop
		  for cld-repository in (list-cld-repositories)
		  while (not cld)
		  do
		    (let ((repository-cld (find-cld cld-repository
						    (library-name dependency))))
		      (setf cld (and repository-cld (load-cld repository-cld
							      :if-already-loaded
							      if-already-loaded)))
		      (when cld
			(verbose-msg "~A cld found in ~A~%"
				     (library-name dependency)
				     cld-repository))))
	       (if cld
		   ;; A cld was found in repositories, load it
		   (load-dependency dependency)
		   ;; Otherwise, a cld could not be found in repositories, try with the dependency cld, if it exists
		   (progn
		     (setf cld (and (cld dependency)
				    (load-cld (cld dependency)
					      :if-already-loaded if-already-loaded)))
		     (if cld
			 ;; the cld specified in the dependency was found, load the dependency
			 (load-dependency dependency)
			 
		       ;; else, In this case a cld was not found either in repositories or by looking at the cld
		       ;; specified in the dependency
		       ;; What to do in this case??
		       ;; we can signal an error, or ignore this (signal a warning), as
		       ;; the library version may be loadable from the user system repository
		       ;; anyway (.i.e. Quicklisp)
		       (ecase *solving-mode*
			 (:lenient (warn "Couldn't find a cld for ~A" dependency))
			 (:strict (error "Couldn't find a cld for ~A" dependency)))))))))
    ;; Load the dependencies for the library version
    (loop for dependency in (dependencies library-version)
       do (progn
            (verbose-msg "Handling ~A.~%" dependency)
            ;; For each dependency, try to load its cld
            (load-dependency-cld dependency)))))

(defun calculate-library-versions-involved (library-version &optional visited)
  (remove-duplicates
   (cons library-version
         (loop for dependency in (dependencies library-version)
            appending
              (if (find (library-name dependency) visited
                        :key #'library-name
                        :test #'equalp)
                  (error "Cyclic dependency on ~A" dependency)
                                        ;else
                  (let ((library (find-library (library-name dependency) nil)))
                    (if library
                        (let ((library-versions (find-library-versions library dependency)))
                          (append library-versions
                                  (loop for dependency-library-version in library-versions
                                     appending
                                       (calculate-library-versions-involved
                                        dependency-library-version
                                        (cons dependency visited)))))
                                        ;else
                        (ecase *solving-mode*
                          (:lenient (warn "No ASDF system is being loaded by CLDM for ~A~%"
                                          dependency))
                          (:strict (error "Coudn't load ~A" dependency))))))))
   :test #'library-version=))

(defun pick-library-version (library-versions)
  "Picks a version from a library list of versions"
  (let ((library-version (first library-versions)))
    (loop for lib-version in (rest library-versions)
       do (setf library-version (best-library-version library-version lib-version)))
    library-version))

(defun best-library-version (v1 v2)
  (cond
    ((not (version v2))
     v1)
    ((not (version v1))
     v2)
    (t
     (assert (equalp (version v1) (version v2)) nil "This should not have happened")
     v1)))

(defun pick-library-versions (versions-list)
  (flet ((pick-latest-version (library-version)
           ;; If the library version is not specified, pick the latest version available
           (if (not (version library-version))
               (let ((latest-library-version
                      (first (library-versions (library library-version)))))
                 (verbose-msg "No specific library version specified for ~A. Picking latest library version: ~A~%"
                              library-version
                              latest-library-version)
                 latest-library-version)
               library-version)))
    (mapcar (compose #'pick-latest-version #'pick-library-version)
            (group-by versions-list
                      :key (compose #'library-name #'library)
                      :test #'equalp))))

;; ASDF plugging

;; A very bad and dumb ASDF system searcher for now
;; Try to improve using repositories metadata, among other things
(defun asdf-system-directory-search (name directory)
  (loop for dir in (cl-fad:list-directory directory)
     for asd-file = (merge-pathnames (pathname (format nil "~A.asd" name)) dir)
     when (and (cl-fad:directory-pathname-p dir)
               (probe-file asd-file))
     return asd-file))

(defun asdf-system-search (name)
  (let ((system-name (or (and (symbolp name)
                              (string-downcase (symbol-name name)))
                         name)))
    (let ((libraries-directories (list *local-libraries-directory* *libraries-directory*)))
      (loop for libraries-directory in libraries-directories
         for system-filename = (asdf-system-directory-search system-name libraries-directory)
         when system-filename
         return system-filename))))

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

(defun calculate-library-dependencies (library
                                       &key version
                                         (libraries-directory *libraries-directory*))
  (let ((library (or (and (stringp library)
                          (find-library library))
                     library)))
    (verbose-msg "Calculating dependencies for ~A...~%" library)
    (let ((library-version (if version
                               (find-library-version library version)
                               (first (library-versions library)))))
      ;; Load libraries metadata
      (load-library-version-metadata library-version)

      ;; Calculate list of library-versions involved
      (let ((library-versions-involved
             (calculate-library-versions-involved library-version)))

        (pbo-solve-library-versions library-version
                                    library-versions-involved)))))

(defun clean-asdf-environment ()
  (setf asdf:*central-registry* nil)
  (asdf:clear-source-registry)
  (asdf:clear-configuration)
  (setf asdf:*system-definition-search-functions* (list 'ASDF/FIND-SYSTEM:SYSDEF-CENTRAL-REGISTRY-SEARCH)))

(defun load-library (library-name
                     &key
		       version
                       (clean-asdf-environment *clean-asdf-environment*)
                       (libraries-directory *libraries-directory*))
  (when clean-asdf-environment
    (clean-asdf-environment))
  (load-cld-for-library library-name)
  (let ((library (find-library library-name)))
    (let ((library-versions (calculate-library-dependencies library
							   :version version
							   :libraries-directory libraries-directory))
	  (library-version (if version 
			       (find-library-version library version)
			       (first (library-versions library)))))
      (loop for library-version in (cons library-version library-versions)
	   do
	   (multiple-value-bind (installed-p install-directory)
	       (library-version-installed-p library-version)
	     (if installed-p
		 (push install-directory asdf:*central-registry*)
		 (error "~A is not installed" library-version))))
      (asdf:load-system library-name :force-not (asdf:registered-systems)))))

(defun install-library-dependencies (library &key version
                                               (libraries-directory *libraries-directory*)
                                               (interactive t))
  (let ((library (or (and (stringp library)
                          (find-library library))
                     library)))
    (let ((library-versions (calculate-library-dependencies library
							    :version version
							    :libraries-directory libraries-directory)))
      (info-msg "Libraries to install: ~{~A~^, ~}~%"
                (mapcar #'library-version-unique-name library-versions))
      (let ((install-p t))
        (when interactive
          (info-msg "Install?~%")
          (setf install-p (yes-or-no-p)))
        (when install-p
          ;; Check the version existance and download if not
          (loop for version in library-versions
             do
               (install-library-version version libraries-directory)))))))

(defun load-cld-for-library (library-name &key (error-p t))
  (aif (find-library library-name nil)
       it
       ;; else
       (let (cld)
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
	 (when (and (not cld) error-p)
	   (error "Couldn't find a cld for ~S library~%" library-name))
	 cld)))

(defun install-library (library-name
                        &key
                          version
                          cld
                          (verbose *verbose-mode*)
                          (solving-mode *solving-mode*)
                          (libraries-directory *libraries-directory*)
                          (clear-registered-libraries t)
                          (interactive t))
  "Tries to find a cld for the library and load it.
   Then setup the library and its dependencies"
  (let ((*verbose-mode* verbose)
        (*solving-mode* solving-mode))
    (when clear-registered-libraries
      (clear-registered-libraries))
    (info-msg "Installing ~A...~%" library-name)
    (with-download-session ()
      (let ((version (when version
                       (read-version-from-string version))))
	(if cld 
	    (load-cld (parse-cld-address cld))
	    (load-cld-for-library library-name))
	(install-library-dependencies library-name
				      :version version
				      :libraries-directory libraries-directory
				      :interactive interactive)))))
        
(defmethod load-project ((directory pathname)
			 &rest args
                         &key
			   libraries-directory
			   (clean-asdf-environment *clean-asdf-environment*))
  (declare (ignorable libraries-directory
		      clean-asdf-environment))
  (apply #'load-project (load-project-from-directory directory)
	 args))                

(defmethod load-project ((project project)
                         &key
			   (libraries-directory (libraries-directory project))
			   (clean-asdf-environment *clean-asdf-environment*))
  "Load a project and its dependencies"
  (info-msg "Loading ~A...~%" (project-name project))
  (when clean-asdf-environment
    (clean-asdf-environment))
  (push (project-directory project) asdf:*central-registry*)
  (push libraries-directory asdf:*central-registry*)
  (asdf:load-system (library-name (library project))
                    :force-not (asdf:registered-systems)))

(defun install-project-from-ilv (project libraries-directory &key (interactive t))
  "Install project form library versions in the lock file"
  (info-msg "Libraries to install: ~{~A~^, ~}~%"
            (mapcar #'library-version-unique-name
                    (installed-library-versions project)))
  (let ((install-p t))
    (when interactive
      (info-msg "Install?~%")
      (setf install-p (yes-or-no-p)))
    (when install-p
      (loop for ilv in (installed-library-versions project)
         do (install-library-version ilv libraries-directory)))))

(defmethod install-project ((directory pathname)
			    &key
			      version
			      libraries-directory
			      (verbose *verbose-mode*)
			      (solving-mode *solving-mode*)
			      (clean-asdf-environment *clean-asdf-environment*)
			      (clear-registered-libraries t)
			      (interactive t))
  (install-project (load-project-from-directory directory)
		   :version version
		   :libraries-directory libraries-directory
		   :verbose verbose
		   :solving-mode solving-mode
		   :clean-asdf-environment clean-asdf-environment
		   :clear-registered-libraries clear-registered-libraries
		   :interactive interactive))

(defmethod install-project ((project project)
                            &key
                              version
                              libraries-directory
                              (verbose *verbose-mode*)
                              (solving-mode *solving-mode*)
                              (clear-registered-libraries t)
                              (interactive t))
  "Installs a CLDM project dependencies"

  (let ((*verbose-mode* verbose)
        (*solving-mode* solving-mode)
        (version (or version
                     (project-version project)))
        (libraries-directory (or libraries-directory
                                 (libraries-directory project)
                                 *local-libraries-directory*)))
    (info-msg "Loading ~A...~%" project)
    (when clear-registered-libraries
      (clear-registered-libraries))
    (verbose-msg "Removing installed libraries...~%")
    (remove-directory libraries-directory)
    (ensure-directories-exist libraries-directory)
    (verbose-msg "Installing project libraries...~%")
    (if (installed-library-versions project)
        ;; If there's a lock file, install versions specified there
        (install-project-from-ilv project libraries-directory
                                  :interactive interactive)
        ;; else, calculate the dependencies
        (with-download-session ()
          (let ((library-version (if version
                                     (find-library-version (library project) version)
                                     (first (library-versions (library project))))))
            ;; Load libraries metadata
            (load-library-version-metadata library-version)

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
                (let ((install-p t))
                  (when interactive
                    (info-msg "Install?~%")
                    (setf install-p (yes-or-no-p)))

                  (when install-p
                    ;; Check the version existance and download if not
                    (let ((installed-library-versions ()))
                      (loop for version in library-versions
                         do
                           (let ((installed-library-version
                                  (install-library-version version libraries-directory)))
                             (push installed-library-version installed-library-versions)))
                      (create-lock-file project installed-library-versions))
                    (verbose-msg "Done.~%"))))))
          t))))

(defmethod update-project ((project project)
                           &key
                             version
                             libraries-directory
                             (verbose *verbose-mode*)
                             (solving-mode *solving-mode*)
                             (clear-registered-libraries t)
                             (interactive t))
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
    (info-msg "Updating project dependencies...~%")
    (let ((project-library-versions (installed-library-versions project)))
      (with-download-session ()
        (let ((library-version (if version
                                   (find-library-version (library project) version)
                                   (first (library-versions (library project))))))
          ;; Load libraries metadata
          (load-library-version-metadata library-version)

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

              (let ((update-p t))
                (when interactive
                  (info-msg "Update?~%")
                  (setf update-p (yes-or-no-p)))
                (when update-p
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
                    (create-lock-file project installed-library-versions))
                  (verbose-msg "Done.~%")))))
          t)))))

(defun load-library-version-metadata (library-version &key (if-already-loaded *if-already-loaded-cld*))
  "Load a library version dependencies clds"
  (verbose-msg "Loading ~A.~%" library-version)
  (labels ((load-dependency (dependency)
             "Load a dependency cld, and the cld of dependencies of the dependency"
	     (let* ((library (find-library (library-name dependency)))
                    (library-versions (find-library-versions library dependency)))
               (loop for library-version in library-versions
                  do (load-library-version-metadata library-version :if-already-loaded if-already-loaded))))
           (load-dependency-cld (dependency dependant)
                                        ; To load a dependency cld, we try looking in repositories first, and, if we couldn't find
                                        ; a cld there, we try to load the cld specified in the dependency. This is so that we can give
                                        ; the user an opportunity to have some control of which cld files he wants to take priority over others
                                        ; by adding a cld repository to *cld-repositories*
	     ;; If the dependency specifies a repository, then no cld is loaded
	     ;; The library is just fetched form there
	     (when (requirement-repository dependency)
	       (return-from load-dependency-cld))
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
                           (:lenient (warn "Couldn't find a cld for ~A required by ~A" dependency dependant))
                           (:strict (error "Couldn't find a cld for ~A required by ~A" dependency dependant)))))))))
    ;; Load the dependencies for the library version
    (loop for dependency in (dependencies library-version)
       do (progn
            (verbose-msg "Handling ~A.~%" dependency)
            ;; For each dependency, try to load its cld
            (load-dependency-cld dependency library-version)))))

(defun calculate-library-versions-involved (library-version &optional visited)
  (remove-duplicates
   (cons library-version
         (loop for dependency in (dependencies library-version)
            appending
              (cond 
		((find (library-name dependency) visited
		       :key #'library-name
		       :test #'equalp)
		 ;; Error, there a cyclic dependency
		 (error "Cyclic dependency on ~A" dependency))
		((requirement-repository dependency)
		 ;; If the dependency specifies a repository,
		 ;; then don't load the cld and calculate recursively, 
		 ;; just add the library version
		 (list (requirement-library-version dependency)))
		(t
		 ;; Calculate library versions involved recursively
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
			 (:strict (error "Coudn't load ~A" dependency)))))))))
   :test #'library-version=))

(defmethod remove-library-version ((library-version library-version) libraries-directory)
  (let* ((install-directory-name (format nil "~A-~A"
                                         (library-name (library library-version))
                                         (print-version-to-string (version library-version))))
         (install-directory (merge-pathnames
                             (pathname (format nil "~A/" install-directory-name))
                             libraries-directory)))
    (remove-directory install-directory)))

(defun library-version-install-directory (library-version &optional
							    (libraries-directory *libraries-directory*))
    (let* ((install-directory-name (format nil "~A-~A"
                                         (library-name (library library-version))
                                         (print-version-to-string (version library-version)))))
         (merge-pathnames
	  (pathname (format nil "~A/" install-directory-name))
	  libraries-directory)))

(defun library-version-installed-p (library-version &optional
						      (libraries-directory *libraries-directory*))
  "Returns whether a library version is installed and if it is, where"
  (if (listp libraries-directory)
      (loop for dir in libraries-directory
	 do (multiple-value-bind (installed-p install-directory)
		(library-version-installed-p library-version dir)
	      (when installed-p
		(return-from library-version-installed-p
		  (values t install-directory)))))
					; else
      (let ((install-directory (library-version-install-directory library-version libraries-directory)))
	(if (probe-file install-directory)
	    (values t install-directory)))))

(defmethod install-library-version ((library-version library-version)
                                    &optional
                                      (libraries-directory *libraries-directory*)
                                      (if-installed *if-already-installed-library-version*))
  "Installs LIBRARY-VERSION to LIBRARIES-DIRECTORY.
   LIBRARIES-DIRECTORY is the root directory where the library version is to be installed.
   IF-INSTALLED controls what is done if the library is already installed. One of :supersede, :reinstall, :ignore, :error.
   Return values: if the library was installed, returns a INSTALLED-LIBRARY-VERSION object. Else, nil"

  (ensure-directories-exist libraries-directory)
  (let* ((install-directory-name (format nil "~A-~A"
                                         (library-name (library library-version))
                                         (print-version-to-string (version library-version))))
         (install-directory (merge-pathnames
                             (pathname (format nil "~A/" install-directory-name))
                             libraries-directory))
         (installed-repository nil))
    (flet ((%install-library-version ()
             (info-msg "Installing ~A...~%"
                       (library-version-unique-name library-version))
             (let ((done nil))
               (loop for repository in (repositories library-version)
                  while (not done)
                  do (progn
                       (verbose-msg "Trying with ~A...~%" repository)
                       (setf installed-repository repository)
                       (setf done (install-repository repository install-directory))
                       (if (not done)
                           (verbose-msg "Failed.~%")
                           (verbose-msg "Success.~%"))))
               (when (not done)
                 (error "Couldn't install repository from ~{~A~}~%"
                        (repositories library-version))))
             ;; Build the installed library version object to return
             (make-instance 'installed-library-version
                            :name (library-name library-version)
                            :version (version library-version)
                            :install-directory install-directory
                            :repository installed-repository))
           (%remove-installed-library-version ()
             (remove-directory install-directory)
             ))
      (verbose-msg "Repository directory: ~A~%" install-directory)
      (if (probe-file install-directory)
          ;; If the install directory exists, we assume the library version
          ;; is already installed.
          ;; Act according to IF-INSTALLED variable
          ;; TODO: this assumption can be incorrect. How to fix?
          (progn
	    (verbose-msg "Repository for ~A already exists in ~A~%"
                         library-version
                         install-directory)
            (ecase if-installed
              (:supersede
               (verbose-msg "Reinstalling ~A~%" library-version)
               (%remove-installed-library-version)
               (%install-library-version))
              (:install
               (verbose-msg "Reinstalling ~A~%" library-version)
               (%remove-installed-library-version)
               (%install-library-version))
              (:error (error "~A is already installed." library-version))
              (:ignore
               (values t install-directory))))
          ;; else, the library is not installed. Install.
          (%install-library-version)))))

(defmethod remove-library-version ((ilv installed-library-version) libraries-directory)
  (let* ((install-directory-name (format nil "~A-~A"
                                         (name ilv)
                                         (print-version-to-string (version ilv))))
         (install-directory (merge-pathnames
                             (pathname (format nil "~A/" install-directory-name))
                             libraries-directory)))
    (remove-directory install-directory)))

(defmethod install-library-version ((ilv installed-library-version)
                                    &optional
                                      (libraries-directory *libraries-directory*)
                                      (if-installed *if-already-installed-library-version*))
  "Installs LIBRARY-VERSION specified in lock file to LIBRARIES-DIRECTORY.
   LIBRARIES-DIRECTORY is the root directory where the library version is to be installed.
   IF-INSTALLED controls what is done if the library is already installed. One of :supersede, :reinstall, :ignore, :error.
   Return values: if the library was installed, returns a INSTALLED-LIBRARY-VERSION object. Else, nil"

  (ensure-directories-exist libraries-directory)
  (let* ((install-directory-name (format nil "~A-~A"
                                         (name ilv)
                                         (print-version-to-string (version ilv))))
         (install-directory (merge-pathnames
                             (pathname (format nil "~A/" install-directory-name))
                             libraries-directory)))
    (flet ((%install-library-version ()
             (info-msg "Installing ~A-~A...~%"
                       (name ilv)
                       (print-version-to-string (version ilv)))
             (when (not (install-repository (repository ilv) install-directory))
               (error "Couldn't install from ~A~%" (repository ilv)))
             ilv)
           (%remove-installed-library-version ()
             (remove-directory install-directory)
             ))
      (if (probe-file install-directory)
          ;; If the install directory exists, we assume the library version
          ;; is already installed.
          ;; Act according to IF-INSTALLED variable
          ;; TODO: this assumption can be incorrect. How to fix?
          (progn
            (verbose-msg "Repository for ~A already exists in ~A~%"
                         ilv
                         install-directory)
            (ecase if-installed
              (:supersede
               (verbose-msg "Reinstalling ~A~%" ilv)
               (%remove-installed-library-version)
               (%install-library-version))
              (:install
               (verbose-msg "Reinstalling ~A~%" ilv)
               (%remove-installed-library-version)
               (%install-library-version))
              (:error (error "~A is already installed." ilv))
              (:ignore
               (values t install-directory))))
          ;; else, the library is not installed. Install.
          (%install-library-version)))))

(defun update-library-version (library-version project)
  "Update a library version"

  (let ((installed-library-version
         (find-installed-library-version
          project
          (library-name library-version))))
    (if installed-library-version
        ;; There's a library version installed already
        (if (or (equalp library-version :max-version)
                (version/= (version library-version)
                           (version installed-library-version)))
            ;; The update conditions are satisfied, try to update the repository
            (update-repository installed-library-version library-version)
            ;; else, the library does not need update
            installed-library-version)
        ;; else, the library is not installed: install the library version
        (install-library-version library-version (libraries-directory project)))))

;; TODO: this is wrong
;; put this initialization operation in the places it should go (toplevel operations?)

(load-cldm-config)

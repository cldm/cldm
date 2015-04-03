.. code-block:: common-lisp

          (in-package #:cldm)
     
     


CLDM developer manual
=====================

Introduction
============

**CLDM** is a distributed dependency manager for Common Lisp. 

Its design is similar to that of `Smalltalk Metacello <https://code.google.com/p/metacello>`_. But unlike Metacello, it allows version constraints (like <, <=, >=, >) and solves them using Pseudo Boolean Optimization (PBO) as described in `this paper <http://www.mancoosi.org/papers/ase10.pdf>`_. Library dependencies are encoded to PBO and a PBO solver is run afterwards optimizing to get the newest versions of libraries. `minisat+ <https://github.com/niklasso/minisatp>`_ is the PBO solver being used at the moment, but support for others like `sat4j <http://www.sat4j.org>`_ is also planned.

Common Lisp libraries and its versions are described in ``.cld`` files, that should be made accessible to **CLDM** somehow (url, filesystem, git)

Then **CLDM** download the exact versions of dependencies for a given library and version, and puts them in a filesystem directory. After that, pushes their ``.asd`` definitions to ``asdf:*central-registry*`` and from that point on asdf is in charge.

.. code-block:: common-lisp

          
     
     (eval-when (:compile-toplevel :load-toplevel :execute)
       (pushnew :cldm *features*))
     
     
 

Debugging
=========

**info-msg** function is used for printing INFO messages: 

.. code-block:: common-lisp

          
     
     (defun info-msg (msg &rest args)
       "Output info messages"
       (apply #'format t (cons msg args)))
     
     


**debug-msg** function outputs messages only when *debug-mode* is on:

.. code-block:: common-lisp

          
     
     (defun debug-msg (msg &rest args)
       (when *debug-mode*
         (apply #'format t (cons msg args))))
     
     
     


**verbose-msg** function outputs messages when either *verbose-mode* or *debug-mode* are on:

.. code-block:: common-lisp

          
     
     (defun verbose-msg (msg &rest args)
       (when (or *verbose-mode* *debug-mode*)
         (apply #'format t (cons msg args))))
     
     


CLDs
====

CLDs are library description files.

This is an example:

.. code-block:: common-lisp

     (cldm:deflibrary cldm
       :cld (:git "https://github.com/cldm/cldm.git" "cldm.cld")
       :description "Common Lisp Dependency Manager"
       :author "Mariano Montone <marianomontone@gmail.com>"
       :maintainer "Mariano Montone <marianomontone@gmail.com>"
       :homepage "http://cldm.github.io/cldm"
       :bug-reports "https://github.com/cldm/cldm/issues"
       :source-repository "https://github.com/cldm/cldm"
       :documentation "http://cldm.github.io/cldm/doc/manual/_build/html/index.html"
       :licence "MIT"
       :keywords ("dependency")
       :categories ("Dependency manager")
       :versions
       ((:version "0.0.1"
		  :repositories
		  ((:github (:git "https://github.com/cldm/cldm.git")))
		  :depends-on
		  (:alexandria :ironclad :md5 :cl-ppcre :cl-syntax :esrap
			    :trivial-shell :puri :anaphora :split-sequence
			    :cl-fad :osicat))))
.. code-block:: common-lisp

          
     
     (defun find-library-cld (library-name &optional (cld-repositories (list-cld-repositories)))
       "Given a library name and an optional list of cld-repositories, finds the library CLD."
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
           ;; Use library version's custom repositories, if any
           ;; This is done only once, not recursively. Top level operation.
           (let ((*cld-repositories* (append (custom-repositories library-version)
     					*cld-repositories*)))
     	;; Load libraries metadata
     	(load-library-version-metadata library-version)
     
     	;; Calculate list of library-versions involved
     	(let ((library-versions-involved
     	       (calculate-library-versions-involved library-version)))
     
     	  (pbo-solve-library-versions library-version
     				      library-versions-involved))))))
     
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
         ;; Add library's custom repositories to the list of repositories
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
     	    ;; Use project's custom repositories to calculate dependencies. 
     	    ;; Append them to the list of repositories before operating
     	    (let ((*cld-repositories* (append (custom-repositories library-version)
     					      *cld-repositories*)))
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
     	    t)))))
     
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
     
|

Libraries
=========

.. code-block:: common-lisp

          
     
     (in-package :cldm)
     
     (defparameter *libraries* (make-hash-table :test #'equalp) "Registered libraries table")
     (defparameter *if-already-registered-library* :append "What to do if a library is already registered. One of :append, :replace, :error, :ignore")
     (defparameter *latest-registered-library* nil "The latest registered library")
     (defparameter *register-libraries* t)
     
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
       (:documentation "A library version repository"))
     
     (defmethod print-object ((version-repository library-version-repository) stream)
       (print-unreadable-object (version-repository stream :type t :identity t)
         (print-library-version (library-version version-repository)
                                stream)
         (format stream " ~A ~A"
                 (name version-repository)
                 (repository-address version-repository))))
     
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
     
|

Pseudo-Boolean Optimization
===========================

Overview
--------

For solving versions constraints Pseudo-Boolean Optimization (PBO) is used.

See: <http://www.mancoosi.org/papers/ase10.pdf>`_

`minisat+ <https://github.com/niklasso/minisatp>`_ is the PBO solver being used at the moment.

.. code-block:: common-lisp

          
     
     (in-package :cldm)
     
     (defparameter *pbo-environment* nil)
     
     (defparameter *constraint-variable-counter* 1)
     
     
     

.. _pbo-constraint:

A ``pbo-constraint`` is a constraint with:

* Terms: x1, x2, ..., xn
* Comparision: A comparison operator
* Result: The equation result
* Comment: a comment that appears in the resulting .pbo file for debugging purposes mostly.

.. code-block:: common-lisp

          
     (defstruct (pbo-constraint
                  (:print-function print-pbo-constraint))
       terms comparison result comment)
     
     (defstruct optimization-function
       terms)
     
     (defun print-pbo-constraint (pbo-constraint stream depth)
       (format stream "[~{~A~} ~A ~A ~S]"
               (pbo-constraint-terms pbo-constraint)
               (pbo-constraint-comparison pbo-constraint)
               (pbo-constraint-result pbo-constraint)
               (pbo-constraint-comment pbo-constraint)))
     
     (defun make-pbo-constraint* (terms comparison result &optional comment)
       (make-pbo-constraint :terms terms
                            :comparison comparison
                            :result result
                            :comment comment))
     
     

Algorithm
---------

Each dependent library and version is encoded as a PBO variable.

Example: hunchentoot-1.0 is x1, and hunchentoot-2.0 is x2

.. code-block:: common-lisp

          
     
     (defun gen-pbo-variable (thing)
       "Return a existing PBO variable, or generate a new one"
       (if (assoc thing *pbo-environment* :test #'library-version=)
           (cdr (assoc thing *pbo-environment* :test #'library-version=))
           ;; else
           (let ((var (make-keyword (format nil "X~A"
                                            *constraint-variable-counter* ))))
             (push (cons thing var) *pbo-environment*)
             (incf *constraint-variable-counter*)
             var)))
     
     


An intermediate representation is used. A list of PBO terms with this form:

``dep1 + dep2 + ... + depn - lib >= 0``

where dep1 .. depn are library versions or a dependent library.

.. code-block:: common-lisp

          
     
     (defun encode-dependency (library-version dependency)
       (let* ((dependency-library (find-library (library-name dependency) nil)))
         ;; Note: we allow the dependency library not to exist here
         ;; This is because the library is not available for some reason, but we rely
         ;; on that the library is availabe in the user local system (i.e. via Quicklisp)
         ;; When the library does not exist, we don't encode any depedencies
         (when dependency-library
           (let ((library-versions (find-library-versions dependency-library dependency)))
             (let ((terms (append
                           (loop for library-version in library-versions
                              collect `(+ 1 ,(gen-pbo-variable
                                              library-version)))
                           `((- 1 ,(gen-pbo-variable library-version))))))
               (make-pbo-constraint* terms
                                     '>= 0
                                     (format nil "~A dependency: ~A"
                                             (library-version-unique-name library-version)
                                             (print-requirement-to-string dependency))))))))
     
     


Conflicts are encoded like: 

``lib1 + lib2 <= 1``

.. code-block:: common-lisp

          
     
     (defun encode-conflict (library-version-1 library-version-2)
       (make-pbo-constraint*
        `((+ 1 ,(gen-pbo-variable library-version-1))
          (+ 1 ,(gen-pbo-variable library-version-2)))
        '<=
        1
        (format nil "Conflict between ~A and ~A"
                (library-version-unique-name library-version-1)
                (library-version-unique-name library-version-2))))
     
     


A library install is encoded like:

``lib >= 1``

.. code-block:: common-lisp

          
     
     (defun encode-install (library-version)
       (make-pbo-constraint*
        `((+ 1 ,(gen-pbo-variable library-version)))
        '>=
        1
        (format nil "Install ~A" (library-version-unique-name library-version))))
     
     (defun library-versions-conflict-p (library-version-1 library-version-2)
       (and (equalp (library-name library-version-1)
                    (library-name library-version-2))
            (and
             (version/== (version library-version-1)
                         (version library-version-2))
             (not (and (equalp (version library-version-1) :max-version)
                       (equalp (version library-version-2) :max-version))))))
     
     (defun encode-library-versions-conflicts (library-versions)
       (loop for library-version-1 in library-versions
          appending
            (loop for library-version-2 in (cdr library-versions)
               when (library-versions-conflict-p library-version-1
                                                 library-version-2)
               collect (encode-conflict library-version-1
                                        library-version-2))))
     
     (defun encode-library-version-dependencies (library-version)
       (let ((dependency-constraints
              (remove-if #'null
                         (loop for dependency in (dependencies library-version)
                            collect
                              (encode-dependency library-version dependency)))))
         dependency-constraints))
     
     (defun encode-install-library-version (library-version library-versions-involved)
       (let ((install-constraint (encode-install library-version))
             (dependencies-constraints
              (loop for library-version in library-versions-involved
                 appending (encode-library-version-dependencies library-version)))
             (conflicts-constraints (encode-library-versions-conflicts
                                     library-versions-involved)))
         (let ((all-constraints (append (list install-constraint)
                                        dependencies-constraints
                                        conflicts-constraints)))
           (values
            all-constraints
            *pbo-environment*
            *constraint-variable-counter*
            (length all-constraints)))))
     
     (defun encode-install-library-versions (library-versions library-versions-involved)
       (let ((install-constraints (loop for library-version in library-versions
                                     collect (encode-install library-version)))
             (dependencies-constraints
              (loop for library-version in library-versions-involved
                 appending (encode-library-version-dependencies library-version)))
             (conflicts-constraints (encode-library-versions-conflicts
                                     library-versions-involved)))
         (let ((all-constraints (append install-constraints
                                        dependencies-constraints
                                        conflicts-constraints)))
           (values
            all-constraints
            *pbo-environment*
            *constraint-variable-counter*
            (length all-constraints)))))
     
     


Serialization
-------------

PBO constraints are then serialized to a Minisat file:

.. code-block:: common-lisp

          
     
     (defun serialize-pbo-constraints (pbo-constraints stream)
       (loop for pbo-constraint in pbo-constraints
          do
            (progn
              (serialize-pbo-constraint pbo-constraint stream)
              (format stream "~%"))))
     
     (defun serialize-pbo-constraint (pbo-constraint stream)
       (format stream "* ~A *~%" (pbo-constraint-comment pbo-constraint))
       (loop for term in (pbo-constraint-terms pbo-constraint)
          do (destructuring-bind (sign constant var) term
               (format stream "~A~A*~A " sign constant
                       (string-downcase (symbol-name var)))))
       (format stream "~A ~A ;"
               (pbo-constraint-comparison pbo-constraint)
               (pbo-constraint-result pbo-constraint)))
     
     


The purpose of all this is to solve an optimization function so that the "best"
library versions are chosen:

.. code-block:: common-lisp

          
     
     (defun create-optimization-function (library-versions-involved)
       (flet ((sort-library-versions-by-freshness (library-versions)
                (sort library-versions #'version> :key #'version)))
         (let ((grouped-library-versions
                (mapcar #'sort-library-versions-by-freshness
                        (group-by library-versions-involved
                                  :key #'library-name
                                  :test #'equalp))))
           (loop for versions-group in grouped-library-versions
              appending
                (loop for library-version in versions-group
                   for wi = 0 then (1+ wi)
                   collect `(+ ,wi ,(gen-pbo-variable
                                     library-version)))))))
     
     (defun serialize-optimization-function (optimization-function stream)
       (loop for term in optimization-function
          do (destructuring-bind (sign constant var) term
               (format stream "~A~A*~A " sign constant
                       (string-downcase (symbol-name var))))))
     
     


PBO equations are serialized to a temporal ``deps.pbo`` file.

.. code-block:: common-lisp

          
     
     (defun pbo-solve-library-versions (library-version library-versions-involved)
       (let ((*pbo-environment* nil)
             (*constraint-variable-counter* 1))
         (multiple-value-bind (constraints pbo-environment
                                           variables-number constraints-number)
             (encode-install-library-version 
     	 library-version 
     	 library-versions-involved)
           (let ((optimization-function
                  (create-optimization-function library-versions-involved)))
             (let ((pbo-file #p"/tmp/deps.pbo"))
               (with-open-file (stream pbo-file
                                       :direction :output
                                       :if-does-not-exist :create
                                       :if-exists :supersede)
                 (format stream "* #variable= ~A #constraint= ~A~%"
                         variables-number
                         constraints-number)
                 (format stream "min: ")
                 (serialize-optimization-function optimization-function stream)
                 (format stream " ;~%" )
                 (serialize-pbo-constraints constraints stream))
               (multiple-value-bind (result error status)
                   (trivial-shell:shell-command
                    (format nil "~A ~A -v0" *minisat+-binary* pbo-file))
     	    (when (equalp status 20)
     	      (error "Dependencies are not satisfiable"))
                 (when (not (or (zerop status)
     			   (equalp status 30)))
                   (error "Error executing ~A ~A -v0" *minisat+-binary* pbo-file))
                 (flet ((find-environment-library-version (var)
                          (car (rassoc var pbo-environment))))
                   (cl-ppcre:register-groups-bind (vars-string)
                       ("\v (.*)" result)
                     (let ((vars (remove-if #'null
                                            (mapcar (compose #'find-environment-library-version
                                                             #'make-keyword
                                                             #'string-upcase)
                                                    (split-sequence:split-sequence #\  vars-string)))))
                       vars)))))))))
     
     (defun pbo-solve-install-library-versions (library-versions library-versions-involved)
       (let ((*pbo-environment* nil)
             (*constraint-variable-counter* 1))
         (multiple-value-bind (constraints pbo-environment
                                           variables-number constraints-number)
             (encode-install-library-versions library-versions library-versions-involved)
           (let ((optimization-function
                  (create-optimization-function library-versions-involved)))
             (let ((pbo-file #p"/tmp/deps.pbo"))
               (with-open-file (stream pbo-file
                                       :direction :output
                                       :if-does-not-exist :create
                                       :if-exists :supersede)
                 (format stream "* #variable= ~A #constraint= ~A~%"
                         variables-number
                         constraints-number)
                 (format stream "min: ")
                 (serialize-optimization-function optimization-function stream)
                 (format stream " ;~%" )
                 (serialize-pbo-constraints constraints stream))
               (multiple-value-bind (result error status)
                   (trivial-shell:shell-command
                    (format nil "~A ~A -v0" *minisat+-binary* pbo-file))
                 (when (not (zerop status))
                   (error "Error executing ~A ~A -v0" *minisat+-binary* pbo-file))
                 (when (cl-ppcre:scan "UNSATISFIABLE" result)
                   (error "Could not satisfy dependencies: ~{~A~^, ~}"
                          (mapcar #'pbo-constraint-comment constraints)))
                 (flet ((find-environment-library-version (var)
                          (car (rassoc var pbo-environment))))
                   (cl-ppcre:register-groups-bind (vars-string)
                       ("\v (.*)" result)
                     (let ((vars (remove-if #'null
                                            (mapcar (compose #'find-environment-library-version
                                                             #'make-keyword
                                                             #'string-upcase)
                                                    (split-sequence:split-sequence #\  vars-string)))))
                       vars)))))))))
     
     


Here is an example ``deps.pbo`` file for installing Hunchentoot library::

     * variable= 20 constraint= 30
     min: +0*x1 +0*x2 +1*x3 +0*x4 +0*x5 +0*x6 +0*x8 +0*x16 +0*x19 +0*x18 +0*x7 +0*x14 +0*x17 +0*x9 +0*x10 +0*x11 +0*x12 +0*x13 +0*x15  ;
     * Install hunchentoot-1.2.26 *
     +1*x1 >= 1 ;
     * hunchentoot-1.2.26 dependency: chunga *
     +1*x2 +1*x3 -1*x1 >= 0 ;
     * hunchentoot-1.2.26 dependency: cl-base64 *
     +1*x4 -1*x1 >= 0 ;
     * hunchentoot-1.2.26 dependency: cl-fad *
     +1*x5 -1*x1 >= 0 ;
     * hunchentoot-1.2.26 dependency: cl-ppcre *
     +1*x6 -1*x1 >= 0 ;
     * hunchentoot-1.2.26 dependency: flexi-streams *
     +1*x7 -1*x1 >= 0 ;
     * hunchentoot-1.2.26 dependency: cl+ssl *
     +1*x8 -1*x1 >= 0 ;
     * hunchentoot-1.2.26 dependency: md5 *
     +1*x9 -1*x1 >= 0 ;
     * hunchentoot-1.2.26 dependency: rfc2388 *
     +1*x10 -1*x1 >= 0 ;
     * hunchentoot-1.2.26 dependency: trivial-backtrace *
     +1*x11 -1*x1 >= 0 ;
     * hunchentoot-1.2.26 dependency: usocket *
     +1*x12 -1*x1 >= 0 ;
     * hunchentoot-1.2.26 dependency: bordeaux-threads *
     +1*x13 -1*x1 >= 0 ;
     * chunga-1.1.5 dependency: trivial-gray-streams *
     +1*x14 -1*x2 >= 0 ;
     * chunga-1.1.1 dependency: trivial-gray-streams *
     +1*x14 -1*x3 >= 0 ;
     * cl-fad-0.7.2 dependency: bordeaux-threads *
     +1*x13 -1*x5 >= 0 ;
     * cl-fad-0.7.2 dependency: alexandria *
     +1*x15 -1*x5 >= 0 ;
     * cl+ssl-latest dependency: cffi *
     +1*x16 -1*x8 >= 0 ;
     * cl+ssl-latest dependency: trivial-gray-streams *
     +1*x14 -1*x8 >= 0 ;
     * cl+ssl-latest dependency: flexi-streams *
     +1*x7 -1*x8 >= 0 ;
     * cl+ssl-latest dependency: bordeaux-threads *
     +1*x13 -1*x8 >= 0 ;
     * cl+ssl-latest dependency: trivial-garbage *
     +1*x17 -1*x8 >= 0 ;
     * cffi-0.12.0 dependency: alexandria *
     +1*x15 -1*x16 >= 0 ;
     * cffi-0.12.0 dependency: trivial-features *
     +1*x18 -1*x16 >= 0 ;
     * cffi-0.12.0 dependency: babel *
     +1*x19 -1*x16 >= 0 ;
     * babel-0.3.0 dependency: trivial-features *
     +1*x18 -1*x19 >= 0 ;
     * babel-0.3.0 dependency: alexandria *
     +1*x15 -1*x19 >= 0 ;
     * flexi-streams-1.0.11 dependency: trivial-gray-streams *
     +1*x14 -1*x7 >= 0 ;
     * bordeaux-threads-0.8.3 dependency: alexandria *
     +1*x15 -1*x13 >= 0 ;
     * Conflict between chunga-1.1.5 and chunga-1.1.1 *
     +1*x2 +1*x3 <= 1 ;
     * Conflict between chunga-1.1.1 and chunga-1.1.5 *
     +1*x3 +1*x2 <= 1 ;          
     

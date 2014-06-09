(in-package #:cldm)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (pushnew :cldm *features*))

(defun verbose-msg (msg &rest args)
  (when *verbose-mode*
    (apply #'format t (cons msg args))))

(defun info-msg (msg &rest args)
  (apply #'format t (cons msg args)))

(defun setup (library-name &optional version (libraries-directory *libraries-directory*))
  "Setup an already loaded (cld) library and its dependencies"
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
        
        ;; Validate the library versions list
					;(validate-library-versions-list library-versions)
	
        (let ((library-versions (pbo-solve-library-versions library-version
							    library-versions-involved)))

	  (info-msg "Libraries to load: ~{~A~^, ~}~%"
		    (mapcar #'library-version-unique-name library-versions))

	  ;; Check the version existance and download if not
	  ;; After that, push to asdf:*central-registry*
	  (loop for version in library-versions
	     do
	       (let ((pathname (cache-library-version version libraries-directory)))
		 (push pathname asdf:*central-registry*)))))))
  (verbose-msg "Done.~%"))

(defun load-library (library-name
		     &key
		       version
		       cld
		       (verbose *verbose-mode*)
		       (solving-mode *solving-mode*)
		       (clean-asdf-environment *clean-asdf-environment*)
		       (load-asdf-system t)
		       (libraries-directory *libraries-directory*))
  "Tries to find a cld for the library and load it.
   Then setup the library and its dependencies"
  (let ((*verbose-mode* verbose)
	(*solving-mode* solving-mode))
    (when clean-asdf-environment
      (setf asdf:*central-registry* nil)
      (asdf:clear-source-registry)
      (asdf:clear-configuration)
      (setf asdf:*system-definition-search-functions* (list 'ASDF/FIND-SYSTEM:SYSDEF-CENTRAL-REGISTRY-SEARCH)))
    (let ((cld (and cld (load-cld (parse-cld-address cld)))))
      (if cld
	  (progn
	    (setup library-name version libraries-directory)
	    (when load-asdf-system
	      (asdf:load-system library-name :force-not (asdf:registered-systems)))
	    t)
	  ;; else
	  (if (find-library library-name nil)
	      (setup library-name version libraries-directory)
	      ;; else
	      (progn
		(loop
		   for cld-repository in *cld-repositories*
		   while (not cld)
		   do (let ((cld-repository (eval cld-repository)))
			(let ((repository-cld (find-cld cld-repository
							library-name)))
			  (setf cld (and repository-cld 
					 (load-cld repository-cld)))
			  (when cld
			    (verbose-msg "~A cld found in ~A~%"
					 library-name
					 cld-repository)))))
		(if cld
		    (progn
		      (setup library-name version)
		      (when load-asdf-system
			;; Avoid reloading already loaded systems
			(asdf:load-system library-name :force-not (asdf:registered-systems)))
		      t)
		    (error "Couldn't find a cld for ~S library~%" library-name))))))))

(defun load-project (library &key
			       version
			       (verbose *verbose-mode*)
			       (solving-mode *solving-mode*)
			       (clean-asdf-environment *clean-asdf-environment*)
			       (load-asdf-system t)
			       (libraries-directory *local-libraries-directory*))
  "Loads a project from its cld"
  
  (let ((*verbose-mode* verbose)
	(*solving-mode* solving-mode))
    (when clean-asdf-environment
      (setf asdf:*central-registry* nil)
      (asdf:clear-source-registry)
      (asdf:clear-configuration)
      (setf asdf:*system-definition-search-functions* (list 'ASDF/FIND-SYSTEM:SYSDEF-CENTRAL-REGISTRY-SEARCH)))
    (verbose-msg "Loading ~A.~%" library)
    (let ((library-version (if version
			       (find-library-version library version)
			       (first (library-versions library)))))
      ;; Load libraries metadata
      (load-library-version library-version)

      ;; Calculate list of library-versions involved
      (let ((library-versions-involved
	     (calculate-library-versions-involved library-version)))
        
	;; Validate the library versions list
					;(validate-library-versions-list library-versions)
	
	(let ((library-versions (pbo-solve-library-versions library-version
							    library-versions-involved)))
	  ;; Remove the project library from the library versions list
	  (setf library-versions (remove (library-name library) library-versions
					 :key #'library-name
					 :test #'equalp))
		    
	  (info-msg "Libraries to load: ~{~A~^, ~}~%" (mapcar #'library-version-unique-name library-versions))

	  ;; Check the version existance and download if not
	  ;; After that, push to asdf:*central-registry*
	  (let ((installed-libraries ()))
	    (loop for version in library-versions
	       do
		 (multiple-value-bind (pathname repository)
		     (cache-library-version version libraries-directory)
		   (push pathname asdf:*central-registry*)
		   (push (list version pathname repository) installed-libraries)))
	    (create-lock-file installed-libraries)))))
    (verbose-msg "Done.~%")
    (when load-asdf-system
      (asdf:load-system (library-name library)
			:force-not (asdf:registered-systems)))
    t))

(defun load-library-version (library-version &key reload)
  "Load a library version dependencies clds"
  (verbose-msg "Loading ~A.~%" library-version)
  (labels ((load-dependency (dependency)
	     "Load a dependency cld, and the cld of dependencies of the dependency"
	     (let* ((library (find-library (library-name dependency)))
		    (library-versions (find-library-versions library dependency)))
	       (loop for library-version in library-versions
		    do (load-library-version library-version :reload reload))))
           (load-dependency-cld (dependency)
	     (let ((cld (and (cld dependency)
			     (load-cld (cld dependency)))))
               (if (not cld)
                   (progn
                     (verbose-msg "No cld could be loaded.~%")
                     (loop
                        for cld-repository in *cld-repositories*
                        while (not cld)
                        do (let ((cld-repository (eval cld-repository)))
			     (let ((repository-cld (find-cld cld-repository
							     (library-name dependency))))
			       (setf cld (and repository-cld (load-cld repository-cld)))
			       (when cld
				 (verbose-msg "~A cld found in ~A~%"
					      (library-name dependency)
					      cld-repository)))))
                     (if cld
			 ;; A cld for the dependency was found, load the dependency
			 (load-dependency dependency)
			 ;; else, no dependency was found. What to do in this case??
			 ;; we can signal an error, or ignore this (signal a warning), as
			 ;; the library version may be loadable from the user system repository
			 ;; anyway (.i.e. Quicklisp)
			 (ecase *solving-mode*
			   (:lenient (warn "Couldn't find a cld for ~A" dependency))
			   (:strict (error "Couldn't find a cld for ~A" dependency)))))
                                        ;else
                   (load-dependency dependency)))))
    (loop for dependency in (dependencies library-version)
       do (progn
            (verbose-msg "Handling ~A.~%" dependency)
	    ;; For each dependency, try to load its cld, if it is not already loaded
            (if (find-library (library-name dependency) nil)
                (progn
                  (verbose-msg "Metadata for ~A is already loaded~%" (library-name dependency))
                  (when reload
                    (verbose-msg "Reloading...")
                    (load-dependency-cld dependency)))
                ;; else
                (load-dependency-cld dependency))))))

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

(defun validate-library-versions-list (versions-list)
  (loop for i from 0 to (1- (length versions-list))
     for vi = (elt versions-list i)
     do
       (loop for j from 1 to (1- (length versions-list))
          for vj = (elt versions-list j)
          when (and (not (equalp i j))
                    (and (equalp (library-name (library vi))
                                 (library-name (library vj)))
                         (and (version vi)
                              (version vj)
                              (not (equalp (version vi)
                                           (version vj))))))
          do (error "Cannot load ~A and ~A" vi vj))))

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

(defun create-lock-file (installed-libraries)
  (let ((lock-file-pathname (merge-pathnames "cldm.lock"
					     (osicat:current-directory))))
    (with-open-file (f lock-file-pathname
		       :direction :output
		       :if-exists :supersede
		       :if-does-not-exist :create)
      (format f "~S"
	      (loop for installed-library in installed-libraries
		 collect
		   (destructuring-bind (library-version library-directory repository) installed-library
		     (list
		      (library-version-unique-name library-version)
		      (list library-directory
			    (directory-checksum library-directory))
		      (list (name repository)
			    (repository-address-sexp (repository-address repository))))))))))

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

(in-package :cldm.cli)

(defun find-asdf-files (&optional (directory (osicat:current-directory)))
  (directory (merge-pathnames "*.asd" directory)))

(defun find-cld-files (&optional (directory (osicat:current-directory)))
  (directory (merge-pathnames "*.cld" directory)))

(defun create-cld-file (name &key force (directory (osicat:current-directory))
			       author cld description 
			       licence maintainer dependencies keywords
			       bug-tracker repositories)
  (let ((cld-filename (or (and name (pathname (format nil "~A.cld" name)))
			  (format nil "~A.cld" 
				  (car (last (pathname-directory 
					      directory)))))))
    (flet ((%create-cld-file ()
	     (let ((cld-template
		    (cldm.cmd::create-cld-template-interactive :name name
							       :cld cld
							       :author author
							       :description description
							       :maintainer maintainer
							       :licence licence
							       :dependencies dependencies
							       :bug-tracker bug-tracker
							       :repositories repositories
							       :keywords keywords
							       :complete t)))
	       (say "~A" cld-template)
	       (when (ask "Create?" :default t)
		 (with-open-file (f cld-filename :direction :output
				    :if-exists :supersede
				    :if-does-not-exist :create)
		   (format f "~A" cld-template))))))

      ;; If the cld file exists, error unless a force option was given
      (let ((cld-file (merge-pathnames cld-filename
				       (osicat:current-directory))))
	(if (probe-file cld-file)
	    (if (not force)
		(progn
		  (format t "The cld file already exist. Use the :force option to overwrite.~%"))
		(%create-cld-file))
	    (%create-cld-file))))))

(defmethod process-command ((command (eql :init)))
  (let ((name (car (clon:remainder))))
    (let ((cld-files (find-cld-files (osicat:current-directory))))
      (when cld-files
	(say "A system seems to have been initialized here: ~A" cld-files)
	(when (not (ask "Continue?" :default t))
	  (return-from process-command))))
    (let* ((asdf-files (find-asdf-files (osicat:current-directory))))
      (if (and asdf-files
	       (ask "There are ASDF systems available. Do you want to take some info from there?" :default t))
	  (let ((asdf-file
		 (if (> (length asdf-files) 1)
		     (choose "Choose the asdf file: " asdf-files)
		     (first asdf-files))))
	    (say "Using ~A" asdf-file)
	    (or (asdf/find-system:load-asd asdf-file)
		(error "Error loading ASDF system file: ~A" asdf-file))
	    (let* ((asdf-system-name (pathname-name asdf-file))
		   (asdf-system (or (asdf/find-system:find-system asdf-system-name)
				    (error "ASDF system could not be loaded: ~A" asdf-system-name))))
	      (create-cld-file name 
			       :force (clon:getopt :long-name "force")
			       :directory (osicat:current-directory)
			       :author (asdf/system:system-author asdf-system)
			       :description (asdf/system:system-description asdf-system)
			       :licence (asdf/system:system-license asdf-system)
			       :maintainer (asdf/system:system-maintainer asdf-system)
			       :bug-tracker (asdf/system:system-bug-tracker asdf-system)
			       :dependencies (asdf/system:system-depends-on asdf-system))))
	  (create-cld-file name 
			   :force (clon:getopt :long-name "force")
			   :directory (osicat:current-directory))))))

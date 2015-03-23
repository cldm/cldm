(defpackage :cldm.cmd
  (:use :cl :duologue))

(in-package :cldm.cmd)

(defun stringp* (str)
  (and (stringp str)
       (not (zerop (length str)))
       str))

(defun search-library (library-name)
  (loop for repo in (cldm::list-cld-repositories)
       do
	 (format t "~A:~%" (cldm::name repo))
	 (let ((search-result 
		(ignore-errors (cldm::search-cld-repository repo (format nil "name:\"~A\"" library-name)))))
	   (loop for elem in search-result
	      do 
		(format t "~A~@[ - ~A~]~%" 
			(cdr (assoc :name elem))
			(stringp* (cdr (assoc :description elem))))))))

(defun create-cld-template (name &key cld description author dependencies &allow-other-keys)
  `(cldm:deflibrary ,name
     :cld ,cld
     :description ,description
     :author ,author
     :dependencies ,dependencies))

(defun create-cld-template-interactive (&rest args &key name cld description author dependencies &allow-other-keys)
  (format t "Are you going to publish the library in a registry?")
  (if (yes-or-no-p)
      (apply #'create-registry-cld-template-interactive args)
      (apply #'create-full-cld-template-interactive args)))

(defun create-registry-cld-template-interactive (&key name cld description author dependencies &allow-other-keys)
  (error "Implement"))

(defun create-full-cld-template-interactive (&key name cld description author dependencies &allow-other-keys)
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
			(let ((library (prompt "Library: " :required-p nil)))
			  (if (not (equalp library ""))
			      (progn
				(let ((version (parse-prompt #'semver:read-version-from-string
							     "Version: " :default "latest")))
				  (push (cons library version) dependencies)))
                                        ; else
			      (return)))))
		 dependencies)))
	(create-cld-template name
			     :cld cld
			     :description description
			     :author author
			     :dependencies (read-dependencies))))))

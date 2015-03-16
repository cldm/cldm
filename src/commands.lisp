(defpackage :cldm.cmd
  (:use :cl))

(in-package :cldm.cmd)

(defun search-library (library-name)
  (loop for repo in (cldm::list-cld-repositories)
       do
	 (format t "~A:~%" (cldm::name repo))
	 (let ((search-result 
		(ignore-errors (cldm::search-cld-repository repo (format nil "name:\"~A\"" library-name)))))
	   (loop for elem in search-result
	      do 
		(format t "~A ~A~%" 
			(cdr (assoc :name elem))
			(cdr (assoc :score elem)))))))

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
  ;; Assign defaults first
  (let ((name (or name
		  (car (last 
			(pathname-directory 
			 (osicat:current-directory)))))))
    (flet ((read-name ()
           (format t "Name [~A]:" name)
           (let ((line (read-line)))
             (format t "~%")
             line))
         (read-description ()
           (format t "Description:")
           (let ((line (read-line)))
             (format t "~%")
             line))
         (read-cld ()
           (format t "CLD:")
           (let ((line (read-line)))
             (format t "~%")
             line))
         (read-author ()
           (format t "Author:")
           (let ((line (read-line)))
             (format t "~%")
             line))
         (read-dependencies ()
           (format t "Enter dependencies.~%")
           (let ((dependencies nil)
                 (continue t))
             (loop while continue
                do
                  (progn
                    (format t "Library:")
                    (let ((library (read-line)))
                      (format t "~%")
                      (if (not (equalp library ""))
                          (progn
                            (let ((version (progn (format t "Version[latest]:")
                                                  (read-line))))
                              (format t "~%")
                              (push (cons library version) dependencies)))
                                        ; else
                          (return)))))
             dependencies)))
    (let ((final-name (let ((read-name (read-name)))
                                (or
                                 (and (not (equalp read-name ""))
                                      read-name)
                                 name)))
          (final-description (or (not (equalp description ""))
                                 (read-description)))
          (final-cld (or (not (equalp cld ""))
                         (read-cld)))
          (final-author (or (not (equalp author ""))
                            (read-author)))
          (final-dependencies (or dependencies
                                  (read-dependencies))))
      (create-cld-template final-name
			   :cld final-cld
			   :description final-description
			   :author final-author
			   :dependencies final-dependencies)))))

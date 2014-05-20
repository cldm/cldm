(in-package :cldm)

(defun deflibrary-file-from-asdf-system (asdf-system cld pathname &key (if-exists :supersede))
  (with-open-file (f pathname
		     :direction :output
		     :if-exists if-exists
		     :if-does-not-exist :create)
    (deflibrary-from-asdf-system asdf-system cld f)))

(defmethod deflibrary-from-asdf-system ((asdf-system string) cld stream)
  (deflibrary-from-asdf-system (asdf:find-system asdf-system) cld stream))

(defmethod deflibrary-from-asdf-system ((asdf-system asdf:system) cld stream)
  (format stream "~S" (make-cld-library-form asdf-system :cld cld)))

(defun make-cld-library-form (asdf-system &key cld repositories)
  (let ((system-name (slot-value asdf-system 'asdf::name)))
    `(cldm:deflibrary ,(intern (string-upcase system-name))
       :cld ,cld
       ,@(when (asdf:system-description asdf-system)
	       (list :description (asdf:system-description asdf-system)))
       ,@(when (asdf:system-author asdf-system)
	       (list :author (asdf:system-author asdf-system)))
       ,@(when (asdf:system-maintainer asdf-system)
	       (list :maintainer (asdf:system-maintainer asdf-system)))
					;,@(when (asdf:system-licence asdf-system)
					;(list :licence (asdf:system-licence asdf-system)))
					;,@(when (asdf:system-homepage asdf-system)
					;(list :homepage (asdf:system-homepage asdf-system)))
					;,@(when (asdf:system-mailto asdf-system)
					;(list :mailto (asdf:system-mailto asdf-system)))
       :versions
       ((:version ,(or (asdf:component-version asdf-system)
		       "latest")
		  :repositories ,(if repositories
				     repositories
				     (list (list :default
						 (list :directory
						       (asdf:system-source-directory asdf-system)))))
		  :depends-on
		  ,(loop for dependency in (asdf:component-sideway-dependencies asdf-system)
		      collect (cond
				((or (symbolp dependency)
				     (stringp dependency))
				 dependency)
				((and (listp dependency)
				      (equalp (first dependency) :version))
				 (list (second dependency) ;; The system name
				       :version (third dependency))) ;; The version
				(t (error "Error parsing asdf dependency ~A" dependency)))))))))

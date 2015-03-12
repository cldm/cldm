(in-package :cldm)

(defclass cached-cld-repository (cld-repository)
  ((cache-directory :initarg :cache-directory
                    :initform nil
                    :accessor cache-directory
                    :documentation "The cache directory"))
  (:documentation "A cld repository in which a cache is maintained in a local directory"))

(defmethod initialize-instance :after ((cld-repository cached-cld-repository) &rest initargs)
  ;; Set default cache directory if none specified
  (when (not (cache-directory cld-repository))
    (setf (cache-directory cld-repository)
	  (ensure-directories-exist
	   (merge-pathnames (format nil "~A/" (name cld-repository))
			    (pathname "~/.cldm/cache/"))))))

(defclass cached-http-cld-repository (http-cld-repository cached-cld-repository)
  ())

(defmethod print-object ((cld-repository cached-http-cld-repository) stream)
  (print-unreadable-object (cld-repository stream :type t :identity t)
    (format stream "~A : ~A (cache: ~A)"
            (name cld-repository)
            (repository-address cld-repository)
            (cache-directory cld-repository))))

(defclass cached-ssh-cld-repository (ssh-cld-repository cached-cld-repository)
  ())

(defmethod print-object ((cld-repository cached-ssh-cld-repository) stream)
  (print-unreadable-object (cld-repository stream :type t :identity t)
    (format stream "~A : ~A (cache: ~A)"
            (name cld-repository)
            (repository-address cld-repository)
            (cache-directory cld-repository))))

(defclass cached-registry-cld-repository (registry-cld-repository cached-cld-repository)
  ())

(defmethod print-object ((cld-repository cached-registry-cld-repository) stream)
  (print-unreadable-object (cld-repository stream :type t :identity t)
    (format stream "~A : ~A (cache: ~A)"
            (name cld-repository)
            (repository-address cld-repository)
            (cache-directory cld-repository))))

(defgeneric clear-cache (cld-repository)
  (:method ((cld-repository cached-cld-repository))
    (verbose-msg "Clearing ~A cache...~%" cld-repository)
    (remove-directory (pathname (cache-directory cld-repository))))
  (:method ((cld-repository cld-repository))
    (error "Cannot clear the cache of this kind of repository")))

(defgeneric cache-cld-repository (cld-repository &key show-progress)
  (:method ((cld-repository cld-repository) &key show-progress)
    (error "Cannot cache ~A" cld-repository)))

(defmethod find-cld :around ((cld-repository cached-cld-repository) library-name)
  (ensure-directories-exist (pathname (cache-directory cld-repository)))
  (let ((cached-file (merge-pathnames
                      (pathname (format nil "~A.cld" library-name))
                      (pathname (cache-directory cld-repository)))))
    (if (probe-file cached-file)
        cached-file
        ;; else
        (let ((downloaded-file (call-next-method)))
          (when downloaded-file
            (let ((command (format nil "cp ~A ~A"
                                   downloaded-file
                                   cached-file)))
              (verbose-msg "~A~%" command)
              (multiple-value-bind (output error status)
                  (trivial-shell:shell-command command)
                (declare (ignore output error))
                (if (zerop status)
                    ;; success
                    cached-file
                    ;; else
                    (progn
                      (verbose-msg "Could not cache cld file ~A~%" downloaded-file)
                      downloaded-file)))))))))

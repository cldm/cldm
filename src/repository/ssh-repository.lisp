(in-package :cldm)

(defclass ssh-cld-repository (cld-repository)
  ((address :initarg :address
            :initform (error "Provide the ssh address")
            :accessor repository-address
            :documentation "The cld repository ssh address"))
  (:documentation "A cld repository accessible via ssh"))

(defmethod print-object ((cld-repository ssh-cld-repository) stream)
  (print-unreadable-object (cld-repository stream :type t :identity t)
    (format stream "~A : ~A"
            (name cld-repository)
            (repository-address cld-repository))))

(defmethod cld-url-address ((cld-repository ssh-cld-repository) library-name)
  (parse-cld-address
   (format nil "~A/~A.cld"
	   (repository-address cld-repository)
	   library-name)))

(defmethod find-cld ((cld-repository ssh-cld-repository) library-name)
  (let ((cld-url-address (cld-url-address cld-repository library-name)))
    (fetch-cld-file cld-url-address)))

(defmethod publish-cld ((cld-repository ssh-cld-repository) cld-pathname)
  (let*  ((cld-address (format nil "~A/~A"
                               (repository-address cld-repository)
                               cld-pathname))
          (command (format nil "scp ~A ~A"
                           (or (probe-file cld-pathname)
                               (error "cld file does not exists ~A" cld-pathname))
                           cld-address)))
    (multiple-value-bind (result error status)
        (trivial-shell:shell-command command)
      (declare (ignore result error))
      (if (equalp status 0)
          (progn
            (verbose-msg "~A published.~%" cld-pathname)
            cld-pathname)
          (error "Error publishing ~A to ~A" cld-pathname cld-repository)))))

(defmethod find-cld :around ((cld-repository ssh-cld-repository) library-name)
  (if *download-session*
      ;; If we are in a download session, check that cld-url-address
      ;; has not previously failed
      (let ((cld-url-address (cld-url-address cld-repository library-name)))
        (if (member cld-url-address *download-session-failed-uris* :test #'equalp)
            ;; The uri has previously failed, do nothing
            (return-from find-cld nil)
            ;; else, the uri has not previously failed, try loading the cld from there
            (let ((cld (call-next-method)))
              (when (not cld)
                ;; the uri failed, add the uri to failed ones
                (push cld-url-address *download-session-failed-uris*))
              cld)))
      ;; else, no download session
      (call-next-method)))


(in-package :cldm)


(defclass registry-cld-repository (cld-repository)
  ((address :initarg :address
            :initform (error "Provide the cld repository url address")
            :accessor repository-address
            :documentation "The cld repository url address"))
  (:documentation "A registry cld repository"))

(defmethod print-object ((cld-repository registry-cld-repository) stream)
  (print-unreadable-object (cld-repository stream :type t :identity t)
    (format stream "~A : ~A"
            (name cld-repository)
            (repository-address cld-repository))))

(defmethod cld-url-address ((cld-repository registry-cld-repository) library-name)
  (parse-cld-address
   (format nil "~A/libraries/~A.cld"
	   (repository-address cld-repository)
	   library-name)))

(defmethod find-cld ((cld-repository registry-cld-repository) library-name)
  (let ((cld-url-address (cld-url-address cld-repository library-name)))
    (fetch-cld-file cld-url-address)))

(defmethod find-cld :around ((cld-repository registry-cld-repository) library-name)
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

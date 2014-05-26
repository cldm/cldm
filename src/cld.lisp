(in-package :cldm)

(defclass cld-address ()
  ()
  (:documentation "A cld address"))

(defclass git-cld-address (cld-address)
  ((git-url :initarg :git-url
            :initform (error "Provide the git url")
            :accessor git-url
            :documentation "The git repository address. i.e. git://git.foo.com/project.git")
   (pathname :initarg :pathname
             :initform (error "Provide the pathname to the cld file")
             :accessor cld-pathname
             :documentation "The pathname to the cld file")
   (branch :initarg :branch
	   :initform "HEAD"
	   :accessor git-branch
	   :documentation "The git branch"))
  (:documentation "A cld in a git repository"))

(defmethod print-object ((cld-address git-cld-address) stream)
  (print-unreadable-object (cld-address stream :type t :identity t)
    (format stream "~A:~A [~A]"
            (git-url cld-address)
            (cld-pathname cld-address)
	    (git-branch cld-address))))

(defclass pathname-cld-address (cld-address)
  ((pathname :initarg :pathname
             :initform (error "Provide the cld pathname")
             :accessor cld-pathname
             :documentation "The pathname of the cld file"))
  (:documentation "A cld in a local filesystem pathname"))

(defmethod print-object ((cld-address pathname-cld-address) stream)
  (print-unreadable-object (cld-address stream :type t :identity t)
    (format stream "~A" (cld-pathname cld-address))))

(defclass http-cld-address (cld-address)
  ((url :initarg :url
        :initform (error "Provide the cld url")
        :accessor cld-url
        :documentation "The cld http url"))
  (:documentation "A cld in an http url"))

(defmethod print-object ((cld-address http-cld-address) stream)
  (print-unreadable-object (cld-address stream :type t :identity t)
    (format stream "~A" (cld-url cld-address))))

(defgeneric parse-cld-address (cld-address)
  (:method ((cld-address pathname))
    cld-address)
  (:method ((cld-address cld-address))
    cld-address)
  (:method ((cld-address string))
    (cond
      ((cl-ppcre:scan "^git://.*:.*$" cld-address)
       (cl-ppcre:register-groups-bind
           (git-url pathname)
           ("^git://(.*):(.*)$" cld-address)
         (make-instance 'git-cld-address
                        :git-url git-url
                        :pathname (pathname pathname))))
      ((cl-ppcre:scan "http(s)://.*$" cld-address)
       (make-instance 'http-cld-address :url cld-address))
      (t
       (make-instance 'pathname-cld-address :pathname (pathname cld-address)))))
  (:method ((cld-address list))
    (ecase (first cld-address)
      (:file (make-instance 'pathname-cld-address :pathname (second cld-address)))
      (:url (make-instance 'http-cld-address :url (second cld-address)))
      (:git (destructuring-bind (url pathname &key branch) (rest cld-address)
	      (make-instance 'git-cld-address
			     :git-url url
			     :pathname (pathname pathname)
			     :branch (or branch "HEAD")))))))

(defgeneric load-cld (cld-address)
  (:method :around (cld-address)
           (let ((pathname (call-next-method)))
             (when pathname
               (let ((*package* (find-package :cldm)))
                 (load pathname)))))
  (:method ((cld-address pathname-cld-address))
    (cld-pathname cld-address))
  (:method ((cld-address git-cld-address))
    (let ((file-namestring (file-namestring (cld-pathname cld-address)))
          (file-directory (let ((dir (directory-namestring (cld-pathname cld-address))))
                            (when (plusp (length dir))
                              dir))))
      (let ((command (format nil "cd /tmp; git archive --remote=~A ~A~@[:~A~] ~A | tar -x"
                             (cl-ppcre:regex-replace-all "~" (git-url cld-address) "~~")
			     (or (git-branch cld-address) "HEAD")
                             file-directory
                             file-namestring)))
        (verbose-msg command)
	(multiple-value-bind (result error status)
	    (trivial-shell:shell-command command)
	  (declare (ignore result error))
	  (when (zerop status)
	    (pathname (format nil "/tmp/~A" file-namestring)))))))
  (:method ((cld-address http-cld-address))
    (cl-ppcre:register-groups-bind (url-path filename)
        ("^(http(s)://.*)/(.*)$" (cld-url cld-address))
      (declare (ignore url-path))
      (let* ((temporal-file (pathname (format nil "/tmp/~A" filename)))
             (command (format nil "wget -O ~A ~A" temporal-file (cld-url cld-address))))
        (verbose-msg command)
        (trivial-shell:shell-command command)
        temporal-file)))
  (:method (cld-address)
    (parse-cld-address cld-address)))
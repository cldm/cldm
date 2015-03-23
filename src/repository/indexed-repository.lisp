(in-package :cldm)

(defvar +index-file-name+ "libraries.idx")

(defclass indexed-cld-repository (cld-repository)
  ((index-file :initarg :index-file
               :initform nil
               :accessor index-file)
   (index :initform nil
          :accessor index)
   (search-index-path :initform nil
		      :accessor search-index-path)
   (search-index :initform nil
                 :accessor search-index)))

(defmethod initialize-instance :after ((cld-repository indexed-cld-repository) &rest initargs)
  (declare (ignore initargs))
  (when (not (index-file cld-repository))
    ;; Set default index file
    (setf (index-file cld-repository)
          (format nil "~A/~A" 
		  (repository-address cld-repository)
                  +index-file-name+)))
  (when (not (search-index-path cld-repository))
    (setf (search-index-path cld-repository)
	  (merge-pathnames "index" 
			   (cache-directory cld-repository))))

  (if (not (probe-file (cached-index-file cld-repository)))
    ;; Download the index
    (update-cld-repository cld-repository)
    ;; else, load the index
    (progn
      (setf (index cld-repository) (read-index-file (cached-index-file cld-repository)))
      (initialize-search-index cld-repository))))

(defclass indexed-ssh-cld-repository (indexed-cld-repository cached-ssh-cld-repository)
  ())

(defclass indexed-http-cld-repository (indexed-cld-repository cached-http-cld-repository)
  ())

(defclass indexed-directory-cld-repository (indexed-cld-repository directory-cld-repository)
  ())

(defun initialize-search-index (cld-repository)
  (verbose-msg "Initializing search index...~%")
  (setf (search-index cld-repository)
	(make-instance 'montezuma:index
		       :path (search-index-path cld-repository))))

(defun remove-search-index (cld-repository)
  (verbose-msg "Removing search index...~%")
  (remove-directory (search-index-path cld-repository)))

(defun build-search-document (library-info)
  (list (cons "name" (getf library-info :name))
        (cons "author" (prin1-to-string (getf library-info :author)))
	(cons "description" (getf library-info :description))
	(cons "licence" (getf library-info :licence))
	(cons "keywords" (getf library-info :keywords))))

(defun build-search-index (cld-repository)
  (verbose-msg "Building search index...~%")
  (loop for library-info in (index cld-repository)
       do
       (montezuma:add-document-to-index (search-index cld-repository)
					(build-search-document library-info))))

(defun cached-index-file (cld-repository)
  (merge-pathnames (pathname +index-file-name+) 
		   (cache-directory cld-repository)))

(defun download-index-file (cld-repository)
  (verbose-msg "Downloading index file ~A~%" (index-file cld-repository))
  (ensure-directories-exist (cache-directory cld-repository))
  (let ((command (format nil "wget -O ~A ~A"
			 (cached-index-file cld-repository)
			 (index-file cld-repository))))
    (multiple-value-bind (output error status)
	(trivial-shell:shell-command command)
      (declare (ignore output error))
      (when (not (zerop status))
	(error "Error downloading repository index: ~A"
	       (index-file cld-repository))))))

(defun read-index-file (pathname)
  (verbose-msg "Reading index ~A...~%" pathname) 
  (read-from-string (file-to-string pathname)))

(defun library-index (library)
  "Create an index from the library description"
  (list :name (library-name library)
        :author (or (library-author library) "")
        :description (or (library-description library) "")
        :licence (or (library-licence library) "")
        :cld (or (cld-address (library-cld library)) "")
	:keywords (prin1-to-string (library-keywords library))))

(defun build-index-file (pathname)
  (let ((*print-pretty* nil))
    (with-open-file (f pathname :direction :output
		       :if-exists :supersede
		       :if-does-not-exist :create)
      (format f "(")
      (loop for library in (list-all-libraries)
	 do
	   (format f "~S~%" (library-index library)))
      (format f ")"))))

(defun build-index-file-from-directory (directory index-file-pathname)
  ;; Load the library definitions first
  (setf *libraries* (make-hash-table :test #'equalp))
  (loop for file in (fad:list-directory directory :follow-symlinks nil)
     when (equalp (pathname-type file) "cld")
     do (load file))
  ;; build the index file
  (build-index-file index-file-pathname))

(defmethod find-cld :around ((cld-repository indexed-cld-repository) library-name)
  "The cld file search succeeds on indexed repositories iff it is found in the index"
  ;; TODO: should we use a hash table instead of list as index??
  (let ((library-info (find library-name (index cld-repository) 
			    :key (lambda (x) (getf x :name))
			    :test #'equalp)))
    (when library-info
      (call-next-method))))

(defgeneric update-cld-repository (cld-repository)
  (:method ((cld-repository cld-repository))
    ;; Do nothing
    ))

(defmethod update-cld-repository ((cld-repository indexed-cld-repository))
  (verbose-msg "Updating ~A...~%" cld-repository)
  (download-index-file cld-repository)
  (setf (index cld-repository) 
	(read-index-file (cached-index-file cld-repository)))
  (remove-search-index cld-repository)
  (initialize-search-index cld-repository)
  (build-search-index cld-repository))

(defmethod search-cld-repository ((cld-repository indexed-cld-repository) term)
  (verbose-msg "Searching for ~A in ~A...~%" term cld-repository)
  (let ((search-result
	 (montezuma:search (search-index cld-repository) term)))
    (loop for result in (montezuma::score-docs search-result)
       collect 
	 (let ((doc (montezuma:get-document (search-index cld-repository)
					    (montezuma:doc result)))
	       (score (montezuma:score result)))
	   (list (cons :name (montezuma:document-value doc "name"))
		 (cons :description (montezuma:document-value doc "description"))
		 (cons :score score))))))

(defun percentage (n total)
  (truncate (/ (* n 100) total)))

(defmethod cache-cld-repository ((cld-repository indexed-cld-repository) &key show-progress)
  (update-cld-repository cld-repository)
  (if show-progress
      (info-msg "Caching ~A: " (name cld-repository))
      (info-msg "Caching ~A ...~%" cld-repository))
  (ensure-directories-exist (pathname (cache-directory cld-repository)))
  (flet ((chars-occupied (percentage)
	   (if (> percentage 9) 3 2)))
    (let ((libraries-number (length (index cld-repository))))
      (loop 
	 for library-info in (index cld-repository)
	 for i = 1 then (1+ i)
	 do   
	   (when show-progress
	     (format t "~A%" (percentage i libraries-number)))
	   (let ((cached-file (merge-pathnames
			       (pathname (format nil "~A.cld" (getf library-info :name)))
			       (pathname (cache-directory cld-repository)))))
	     (let ((downloaded-file (fetch-cld-file (parse-cld-address (getf library-info :cld)))))
	       (when show-progress
		 (finish-output)
		 (dotimes (n (chars-occupied (percentage i libraries-number)))
		   (write-char #\Backspace))) 
	       (when downloaded-file
		 (verbose-msg "~A downloaded." downloaded-file)
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
			 (error "Could not cache cld file ~A~%" downloaded-file)))))))))))


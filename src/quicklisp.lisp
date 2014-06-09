(in-package :cldm)

;; Create CLD files from Quicklisp releases

(defun create-clds-from-quicklisp-dist (releases-file output-directory)
  (with-open-file (f releases-file)
    (read-line f) ;; Discard first line
    (loop for release = (read-line f nil nil)
       while release
       do
	 (destructuring-bind (project url size file-md5
				      content-sha1 prefix &rest system-files)
	     (split-sequence:split-sequence #\  release)
	   (declare (ignorable size file-md5 content-sha1 prefix system-files))
	   (format t "Processing project ~A~%" project)
	   (let ((asdf-file (asdf:search-for-system-definition project)))
	     (when (not asdf-file)
	       (ignore-errors (ql:quickload project))
	       (setf asdf-file (asdf:search-for-system-definition project)))
	     (when asdf-file
	       (format t "Loading asdf file ~A~%" asdf-file)
	       (let ((*package* (find-package :asdf)))
		 (load asdf-file))
	       (let ((asdf-system (asdf:find-system project))
		     (cld (format nil "http://mmontone.github.io/cldm-repo/cld/~A.cld"
				  project)))
		 (format t "Creating cld file...~%")
		 (deflibrary-file-from-asdf-system asdf-system cld
		   (merge-pathnames (pathname (format nil "~A.cld" project))
				    output-directory)
		   :repositories `((:quicklisp (:url ,url))))
		 ;(format t "Continue?:")
		 ;(read)
		 )))))))

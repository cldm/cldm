(in-package :cldm)

(defmacro lexical-< (x1 x2 &rest accessors)
  (if (not accessors)
      't
      (flet ((read-accessor (accessor-spec)
               (if (listp accessor-spec)
                   (destructuring-bind (acc &optional (< '<) (equal 'equalp)) accessor-spec
                     (list acc < equal))
                   (list accessor-spec '< 'equalp))))
        (let ((v1 (gensym))
              (v2 (gensym)))
          (destructuring-bind (acc < equal) (read-accessor (first accessors))
            `(let ((,v1 (,acc ,x1))
                   (,v2 (,acc ,x2)))
               (or (and ,v1 (not ,v2))
                   (and ,v1 ,v2
                        (or (,< ,v1 ,v2)
                            (and (,equal ,v1 ,v2)
                                 (lexical-< ,x1 ,x2 ,@(rest accessors)))))
                   (and (not ,v1) (not ,v2)
                        (lexical-< ,x1 ,x2 ,@(rest accessors))))))))))

(defun lexical-<=* (x y)
  (lexical-< x y car cdr))

(defun tuple< (t1 t2)
  (when (and t1 t2)
    (let ((v1 (first t1))
	  (v2 (first t2)))
      (or (< v1 v2)
	  (and (equalp v1 v2)
	       (tuple< (rest t1)
		       (rest t2)))))))

(defun split (chars str &optional (lst nil) (accm ""))
  (cond
    ((= (length str) 0) (reverse (cons accm lst)))
    (t
     (let ((c (char str 0)))
       (if (member c chars)
	   (split chars (subseq str 1) (cons accm lst) "")
	   (split chars (subseq str 1) 
		  lst 
		  (concatenate 'string
			       accm
			       (string c))))
       ))))

(defun join (str lst &optional (jstr ""))
  (cond
    ((null lst) jstr)
    (t (let ((news (concatenate 'string
				jstr
				(first lst)
				(if (null (rest lst))
				    ""
				    str))))
	 (join str (rest lst) news)))))

(defun group-by (list &key
			(key #'identity)
			(test #'equal))
  (let ((groups nil))
    (loop for item in list
	 do
	 (let ((item-key (funcall key item)))
	   (block find-group
	     ;; Look for a group for the item
	     (loop for group in groups
		   for i from 0
		when (funcall test item-key (funcall key (first group)))
		do (progn
		     (setf (nth i groups) (push item group))
		     (return-from find-group)))
	     ;; Group not found for item, create one
	     (push (list item) groups))))
    (nreverse groups)))

(defun directory-checksum (directory)
  (assert (cl-fad:directory-pathname-p directory)
	  nil "~A is not a directory pathname"
	  directory)
  (multiple-value-bind (output error status)
      (trivial-shell:shell-command (format nil "tar c ~A | md5sum" directory))
    output))

(defun file-to-string (pathname)
  (with-open-file (stream pathname)
    (let ((seq (make-array (file-length stream) :element-type 'character :fill-pointer t)))
      (setf (fill-pointer seq) (read-sequence seq stream))
      seq)))

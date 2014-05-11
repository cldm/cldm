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
	  (tuple< (rest t1)
		  (rest t2))))))

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


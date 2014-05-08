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


(in-package :cldm)

(defstruct (interval (:print-function print-interval))
  from
  to
  (from-type :closed)
  (to-type :closed))

(defun print-interval (interval stream depth)
  (declare (ignore depth))
  (format stream "~A~A, ~A~A"
	  (ecase (interval-from-type interval)
	    (:closed "[")
	    (:opened "("))
	  (interval-from interval)
	  (interval-to interval)
	  (ecase (interval-to-type interval)
	    (:closed "]")
	    (:opened ")"))))

(defun set-interval-from (interval from &optional from-type)
  (setf (interval-from interval) from)
  (when from-type
    (setf (interval-from-type interval) from-type)))

(defun set-interval-to (interval to &optional to-type)
  (setf (interval-to interval) to)
  (when to-type
    (setf (interval-to-type interval) to-type)))

(defun version-min (v1 v2)
  (if (version< v1 v2)
      v1
      v2))

(defun version-max (v1 v2)
  (if (version> v1 v2)
      v1 v2))

(defun interval-intersection (i1 i2)
  (if (or (or (version< (interval-to i1)
			(interval-from i2))
	      (and (version= (interval-to i1)
			     (interval-from i2))
		   (or (equalp (interval-to-type i1) :opened)
		       (equalp (interval-from-type i2) :opened))))
	  (or (version< (interval-to i2)
			(interval-from i1))
	      (and (version= (interval-to i2)
			     (interval-from i1))
		   (or (equalp (interval-to-type i2) :opened)
		       (equalp (interval-from-type i1) :opened)))))
      ;; make an empty interval
      (make-interval :from (read-version-from-string "0.0.0")
		     :to (read-version-from-string "0.0.0")
		     :from-type :opened
		     :to-type :opened)
      ;; else
      (make-interval :from (version-max (interval-from i1)
					(interval-from i2))
		     :from-type (cond
				  ((version= (interval-from i1)
					     (interval-from i2))
				   (or (and (or (equalp (interval-from-type i1) :opened)
						(equalp (interval-from-type i2) :opened))
					    :opened)
				       :closed))
				  ((version> (interval-from i1)
					     (interval-from i2))
				   (interval-from-type i1))
				  (t (interval-from-type i2)))
		     :to (version-min (interval-to i1)
				      (interval-to i2))
		     :to-type (cond
				((version= (interval-to i1)
					   (interval-to i2))
				 (or (and (or (equalp (interval-to-type i1) :opened)
					      (equalp (interval-to-type i2) :opened))
					  :opened)
				     :closed))
				((version< (interval-to i1)
					   (interval-to i2))
				 (interval-to-type i1))
				(t (interval-to-type i2))))))

(defun interval-proper-p (interval)
  "A degenerate interval is any set consisting of a single real number. Some authors include the empty set in this definition. A real interval that is neither empty nor degenerate is said to be proper, and has infinitely many elements."
  (or (version< (interval-from interval)
		(interval-to interval))
      (and (version= (interval-from interval)
		     (interval-to interval))
	   (and (equalp (interval-from-type interval) :closed)
		(equalp (interval-to-type interval) :closed)))))

(defun interval-degenerate-p (interval)
  (not (interval-proper-p interval)))

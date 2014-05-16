(in-package :cldm)

(defclass request ()
  ((pool :initarg :pool
	 :initform (error "Provide the pool")
	 :accessor request-pool)
   (jobs :initform nil
	 :accessor request-jobs))
  
  (:documentation "A Request instance encompasses the set of jobs to be solved by the
    dependency solver."))

(defmethod add-job ((request request) (requirement requirement) job-type)
  (let ((libraries (what-provides (request-pool request) requirement)))
    (push (make-instance 'job
			 :libraries libraries
			 :type job-type
			 :requirement requirement)
	  (request-jobs request))))

(defmethod request-install ((request request) (requirement requirement))
  (add-job request requirement :install)))

(defmethod request-update ((request request) (requirement requirement))
  (add-job request requirement :update))

(defmethod request-remove ((request request) (requirement requirement))
  (add-job request requirement :remove))

(defmethod request-upgrade ((request request))
  (push (make-instance 'job
		       :libraries nil
		       :type :upgrade
		       :requirement nil)
	(request-jobs request)))

(in-package :cldm)

(defstruct decision
  literal
  reason)

(defclass decisions-set ()
  ((pool :initarg :pool
	 :initform (error "Provide the pool")
	 :accessor pool
	 :documentation "The pool of repositories to work with")
   (decision-table :initform (make-hash-table :test #'equalp)
		   :accessor decision-table
		   :documentation "Library id -> decision level mapping")
   (decision-queue :initform nil
		   :accessor decision-queue
		   :documentation "Queue of decision instances"))
  (:documentation "A DecisionsSet instance keeps track of decided literals (and the rational for each decision), and can infer new literals depending on their type."))

(defmethod decide ((decisions-set decisions-set) literal level reason)
  "Add the given literal to the decision set at the given level"
  (add-decision decisions-set literal level)
  (push (make-decision :literal literal :reason reason)
	(decision-queue decisions-set)))

(defmethod satisfy ((decisions-set decisions-set) literal)
  "Return T if the givel literal is satisfied"
  (let ((library-id (abs literal)))
    (let ((positive-case (and (> literal 0)
			      (multiple-value-bind (level found-p)
				  (gethash library-id (decision-table decision-set))
				(and found-p (> level 0)))))
	  (negative-case (and (< literal 0)
			      (multiple-value-bind (level found-p)
				  (gethash library-id (decision-table decision-set)
					   (and found-p (< level 0)))))))
      (or positive-case negative-case))))

(defmethod conflict ((decisions-set decisions-set) literal)
  "Return T if the given literal conflicts with the decision set"
  (let ((library-id (abs literal)))
    (let ((positive-case (and (> literal 0)
			      (multiple-value-bind (level found-p)
				  (gethash library-id (decision-table decisions-set))
				(and found-p (< level 0)))))
	  (negative-case (and (< literal 0)
			      (multiple-value-bind (level found-p)
				  (gethash library-id (decision-table decision-set)
					   (and found-p (> level 0)))))))
      (or positive-case negative-case))))

(defmethod is-decided ((decisions-set decisions-set) literal)
  "Return T if the givel literal has been decided at any level"
  (multiple-value-bind (level found-p)
      (gethash (abs literal) (decision-table decisions-set))
    found-p))

(defmethod is-undecided ((decisions-set decisions-set) literal)
  (not (is-decided decisions-set literal)))

(defmethod is-decided-install ((decisions-set decisions-set) literal)
  (let ((library-id (abs literal)))
    (multiple-value-bind (level found-p)
	(gethash library-id (decision-table decisions-set))
      (and found-p (> level 0)))))

(defmethod at-offset ((decisions-set decisions-set) offset)
  (elt (decision-queue decisions-set) offset))

(defmethod offset-valid-p ((decisions-set decisions-set) offset)
  (and (plusp offset)
       (< offset (length (decision-queue decisions-set)))))

(defmethod add-decision ((decisions-set decisions-set) literal level)
  (let ((library-id (abs literal)))
    (multiple-value-bind (previous-decision found-p)
	(gethash library-id (decision-table decisions-set))
      (if found-p
	  (let ((library (find-library-by-id (pool decisions-set)
					     library-id)))
	    (error "Trying to decide ~A on level ~A, even though ~A was previously decided as ~A"
		   (library-id-to-string library-id)
		   level
		   library
		   previous-decision))
	  ; else
	  (if (> literal 0)
	      (setf (gethash library-id (decision-table decisions-set)) level)
	      ; else
	      (setf (gethash library-id (decision-table decisions-set)) -level))))))

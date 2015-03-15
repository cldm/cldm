(in-package :cldm)

(defclass requirement ()
  ((name :initarg :library-name
	 :initform (error "Provide the library name")
	 :accessor library-name
	 :type string
	 :documentation "The library name")
   (version-constraints :initarg :version-constraints
	  :initform (list :any)
	  :accessor requirement-version-constraints
	  :documentation "A list of constraints")
   (cld :initarg :cld
        :initform nil
        :accessor cld
	:type cld-address
        :documentation "The version meta information address. If this is set, then CLDM fetches the requirement CLD from there if not found elsewhere.")
   (repository :initarg :repository
	       :initform nil
	       :accessor requirement-repository
	       :type repository-address
	       :documentation "A repository address. If this is present, then CLDM fetches the requirement from there, and doesn't calculate any versions"))
   (:documentation "Requirements instances represent a 'library requirement', that is a library + version constraints."))

(defmethod initialize-instance :after ((requirement requirement) &rest initargs)
  (declare (ignore initargs))
  #+nil(if (and (requirement-version-constraints requirement)
	   (not (equalp (first (requirement-version-constraints requirement)) :any))
           (not (cld requirement)))
      (error "Provide a cld for ~A" requirement))
  )

(defun make-requirement (name &rest version-constraints)
  (make-instance 'requirement
		 :library-name name
		 :version-constraints (or version-constraints
					  (list :any))
		 :cld nil))

(defun read-requirement-from-string (string)
  (let ((constraints (parse-requirement-string string)))
    (let* ((distribution-names (mapcar #'first constraints))
	   (distribution-name (first distribution-names)))

      ;; Check distribution names are all the same
      (mapcar (lambda (name)
		(when (not (equalp distribution-name name))
		  (error "Invalid distribution names in requirement ~S" string)))
	      distribution-names)

      (let ((version-constraints (mapcar #'second constraints)))
	(make-instance 'requirement
		       :library-name distribution-name
		       :version-constraints
		       (normalize-version-constraints
			version-constraints)
		       :cld nil)))))

(defun normalize-version-constraints (version-constraints)
  version-constraints)

(defun print-requirement (requirement stream)
  (flet ((print-version-constraint (version-constraint stream)
	   (when (not (equalp version-constraint :any))
	     (destructuring-bind (operation version) version-constraint
	       (format stream " ~A ~A"
		       operation
		       (print-version-to-string version)))))) 
    (format stream "~A" (library-name requirement))
    (let ((version-constraint (first (requirement-version-constraints requirement))))
      (when version-constraint
	(print-version-constraint version-constraint stream)))

    (loop for version-constraint in (rest (requirement-version-constraints requirement))
       do (progn
	    (format stream ", ~A" (library-name requirement))
	    (print-version-constraint version-constraint stream)))
    (when (cld requirement)
      (format stream "(~A)" (cld requirement)))))

(defun print-requirement-to-string (requirement)
  (with-output-to-string (s)
    (print-requirement requirement s)))

(defmethod print-object ((requirement requirement) stream)
  (print-unreadable-object (requirement stream :type t :identity t)
    (print-requirement requirement stream)))

;; Requirements parser

(defrule decimal (+ (or "0" "1" "2" "3" "4" "5" "6" "7" "8" "9"))
  (:function (lambda (list)
	       (parse-integer (format nil "~{~A~}" list)))))

(defrule distribution-name (+ (or (or "0" "1" "2" "3" "4" "5" "6" "7" "8" "9")
				  (character-ranges (#\a #\z) (#\A #\Z) #\_ #\- #\+ #\.)))
  (:text t))

(defrule distribution-name-part (+ (or (or "0" "1" "2" "3" "4" "5" "6" "7" "8" "9")
				       (character-ranges (#\a #\z) (#\A #\Z) #\_ #\+ #\.)))
  (:text t))

(defrule version== "==" (:function (lambda (match)
					 (declare (ignore match))
					 :==)))
(defrule version>= ">=" (:function (lambda (match)
				     (declare (ignore match))
				     :>=)))
(defrule version<= "<=" (:function (lambda (match)
				     (declare (ignore match))
				     :<=)))
(defrule version> ">" (:function (lambda (match)
				   (declare (ignore match))
				   :>)))
(defrule version< "<" (:function (lambda (match)
				   (declare (ignore match))
				   :<)))
(defrule version!= "!=" (:function (lambda (match)
				     (declare (ignore match))
				     :!=)))

(defrule version-comparison (or version==
				version!=
				version>=
				version<=
				version>
				version<))

(defrule version-build (+ (or (or "0" "1" "2" "3" "4" "5" "6" "7" "8" "9")
			      (character-ranges (#\a #\z) (#\A #\Z) #\_)))
  (:text t))

(defrule version-pre-release (+ (or (or "0" "1" "2" "3" "4" "5" "6" "7" "8" "9")
				    (character-ranges (#\a #\z) (#\A #\Z) #\_)))
  (:text t))

(defrule version (and decimal #\. decimal #\. decimal
		      (? (and #\- version-pre-release))
		      (? (and #\+ version-build)))
  (:function (lambda (match)
	       (destructuring-bind (major dot1 minor dot2 patch pre-release build) match
		 (make-semantic-version major minor patch
					(and pre-release
					     (second pre-release))
					(and build
					     (second build)))))))
(defrule spaces (+ #\ ))

(defrule version-constraint (and version-comparison
				 spaces
				 version)
  (:function (lambda (match)
	       (destructuring-bind (comp spaces number) match
		   (list comp number)))))

(defrule distribution-constraint (and distribution-name
				      (? (and spaces
					      version-constraint)))
  (:function (lambda (match)
	       (destructuring-bind (distribution-name version-constraint) match
		 (list distribution-name (if version-constraint
					     (second version-constraint)
					     :any))))))

(defrule requirement (and distribution-constraint (? (and (? spaces) #\, (? spaces) requirement)))
  (:function (lambda (match)
	       (destructuring-bind (constraint more) match
		   (cons constraint (when more (nth 3 more)))))))

(defun parse-requirement-string (string)
  (parse 'requirement string))

(defun requirement-universal-p (requirement)
  "Returns true if the requirement matches any version"
  (equalp (requirement-version-constraints requirement)
	  (list :any)))
  
;; Matching
(defun make-requirement-version-intervals (requirement)
  (let ((intervals (list (make-interval :from :min-version
					:to :max-version))))
    (loop for version-constraint in (requirement-version-constraints requirement)
       do (when (not (equalp version-constraint :any))
	    (destructuring-bind (operation version) version-constraint
	      (ecase operation
		(:== (let ((==interval (make-interval :from version :to version)))
		       (setf intervals
			     (remove-if-not #'interval-proper-p
					    (loop for interval in intervals
					       collect (interval-intersection ==interval interval))))))						   
		(:!= (let ((!=intervals (list (make-interval :from :min-version
							     :to version
							     :to-type :opened)
					      (make-interval :from version
							     :from-type :opened
							     :to :max-version))))
		       (setf intervals
			     (remove-if-not #'interval-proper-p
					    (loop for interval in intervals
					       appending
						 (loop for !=interval in !=intervals
						    collect (interval-intersection interval !=interval)))))))
		(:<= (let ((<=interval (make-interval :from :min-version
						      :to version)))
		       (setf intervals
			     (remove-if-not #'interval-proper-p
					    (loop for interval in intervals
					       collect (interval-intersection interval <=interval))))))
		(:< (let ((<interval (make-interval :from :min-version
						    :to version
						    :to-type :opened)))
		      (setf intervals
			    (remove-if-not #'interval-proper-p
					   (loop for interval in intervals
					      collect (interval-intersection interval <interval))))))
		(:>= (let ((>=interval (make-interval :from version
						      :to :max-version)))
		       (setf intervals
			     (remove-if-not #'interval-proper-p
					    (loop for interval in intervals
					       collect (interval-intersection interval >=interval))))))
		(:> (let ((>interval (make-interval :from version
						    :from-type :opened
						    :to :max-version)))
		      (setf intervals
			    (remove-if-not #'interval-proper-p
					   (loop for interval in intervals
					      collect (interval-intersection interval >interval))))))))))
    intervals))       

(defmethod requirement-matches ((requirement requirement) (provider requirement))
  ;; Return false if names dont match
  (when (not (equalp (library-name requirement)
		     (library-name provider)))
    (return-from requirement-matches nil))

  (let ((requirement-intervals (make-requirement-version-intervals requirement))
	(provider-intervals (make-requirement-version-intervals provider)))
    (let ((intersection-intervals
	   (loop for requirement-interval in requirement-intervals
		appending
		(loop for provider-interval in provider-intervals
		   collect (interval-intersection requirement-interval
				  provider-interval)))))
      (some #'interval-proper-p intersection-intervals))))

(defun requirement-cannot-match-p (requirement)
  (let ((intervals (make-requirement-version-intervals requirement)))
    (not (some #'interval-proper-p intervals))))

(defun requirement= (req1 req2)
  (and (equalp (library-name req1)
	       (library-name req2))
       (set-equal (requirement-version-constraints req1)
		  (requirement-version-constraints req2)
		  :test
		  (lambda (constraint1 constraint2)
		    (or (equalp constraint1 constraint2)
			(and (listp constraint1)
			     (listp constraint2)
			     (equalp (first constraint1)
				     (first constraint2))
			     (version= (second constraint1)
				       (second constraint2))))))))

(defrule library-unique-name (and distribution-name-part (? (and #\- (or version library-unique-name))))
  (:function (lambda (match)
	       (destructuring-bind (dnp rest) match
		 (if (or (null (second rest))
			 (typep (second rest) 'version))
		     (list dnp (second rest))
					;else
		     (list (format nil "~A-~A" dnp (first (second rest)))
			   (second (second rest))))))))

(defun parse-library-string (string)
  (values-list (parse 'library-unique-name string)))

(defun read-requirement-from-library-version-string (string)
    (multiple-value-bind (name version)
	(parse-library-string string)
      (if (equalp version :any)
	  (make-requirement name)
	  (make-requirement name (list :== version)))))

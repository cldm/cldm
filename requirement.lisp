(in-package :cldm)

(defclass requirement ()
  ((name :initarg :name
	 :initform (error "Provide the library name")
	 :accessor requirement-name
	 :type string
	 :documentation "The library name")
   (version-constraints :initarg :version-constraints
	  :initform (list :any)
	  :accessor requirement-version-constraints
	  :documentation "A list of constraints")
   (min-bound :initform :min-version
	      :accessor min-bound
	      :documentation "Min version bound")
   (max-bound :initform :max-version
	      :accessor max-bound
	      :documentation "Max version bound"))
  (:documentation "Requirements instances represent a 'library requirement', that is a library + version constraints."))

(defun make-requirement (name &rest version-constraints)
  (make-instance 'requirement
		 :name name
		 :version-constraints version-constraints))		 

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
		       :name distribution-name
		       :version-constraints
		       (normalize-version-constraints
			version-constraints))))))

(defun normalize-version-constraints (version-constraints)
  ;; TODO
  version-constraints)

(defun print-requirement (requirement stream)
  (flet ((print-version-constraint (version-constraint stream)
	   (when (not (equalp version-constraint :any))
	     (destructuring-bind (operation version) version-constraint
	       (format stream "~A ~A"
		       operation
		       (format nil "~A.~A.~A"
			       (first version)
			       (second version)
			       (third version))))))) 
    (format stream "~A " (requirement-name requirement))
    (print-version-constraint (first (requirement-version-constraints requirement)) stream)

    (loop for version-constraint in (rest (requirement-version-constraints requirement))
       do (progn
	    (format stream ", ~A " (requirement-name requirement))
	    (print-version-constraint version-constraint stream)))))

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

(defrule distribution-name (* (or (or "0" "1" "2" "3" "4" "5" "6" "7" "8" "9")
				  (character-ranges (#\a #\z) (#\A #\Z) #\_ #\-)))
  (:text t))

(defrule version== "==" (:function (lambda (match)
					 (declare (ignore match))
					 '==)))
(defrule version>= ">=" (:function (lambda (match)
				     (declare (ignore match))
				     '>=)))
(defrule version<= "<=" (:function (lambda (match)
				     (declare (ignore match))
				     '<=)))
(defrule version> ">" (:function (lambda (match)
				   (declare (ignore match))
				   '>)))
(defrule version< "<" (:function (lambda (match)
				   (declare (ignore match))
				   '<)))
(defrule version!= "!=" (:function (lambda (match)
				     (declare (ignore match))
				     '!=)))

(defrule version-comparison (or version==
				version!=
				version>=
				version<=
				version>
				version<))

(defrule version-number (and decimal #\. decimal #\. decimal)
  (:function (lambda (match)
	       (destructuring-bind (major dot1 minor dot2 patch) match
		 (list major minor patch)))))

(defrule spaces (+ #\ ))

(defrule version-constraint (and version-comparison
				 spaces
				 version-number)
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
(defmethod requirement-matches ((requirement requirement) (provider requirement))
  ;; Return false if names dont match
  (when (not (equalp (requirement-name requirement)
		     (requirement-name provider)))
    (return-from requirement-matches nil)))

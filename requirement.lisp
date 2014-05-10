(in-package :cldm)

(defclass requirement ()
  ((name :initarg :name
	 :initform (error "Provide the library name")
	 :accessor requirement-name
	 :type string
	 :documentation "The library name")
   (specs :initarg :specs
	  :initform (error "Provide the specs")
	  :accessor requirement-specs
	  :documentation "A list of constraints")
   (min-bound :initform :min-version
	      :accessor min-bound
	      :documentation "Min version bound")
   (max-bound :initform :max-version
	      :accessor max-bound
	      :documentation "Max version bound"))
  (:documentation "Requirements instances represent a 'library requirement', that is a library + version constraints."))

(defun read-requirement-from-string (string)
  (multiple-value-bind (name version) (parse-library-full-name string)
    (make-instance 'requirement :name name
		   :version (list :equal version))))

(defmethod requirement-matches ((requirement requirement) (provider requirement))
  "Return T if provide requirement and this requirement are compatible"
  (when (not (equalp (requirement-name requirement)
		     (requirement-name provider)))
    (return-from requirement-matches nil)))

(defrule decimal (+ (or "0" "1" "2" "3" "4" "5" "6" "7" "8" "9"))
  (:function (lambda (list)
	       (parse-integer (format nil "~{~A~}" list)))))

(defrule distribution-name (* (or (or "0" "1" "2" "3" "4" "5" "6" "7" "8" "9")
				  (character-ranges (#\a #\z) (#\A #\Z) #\_ #\-)))
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
  (parse '(and distribution-name version-equal) string))

(parse 'version== "==")

(parse 'distribution-name "asdf2-asdf")

(parse-requirement-string "asdfad==")

(parse 'version-number "2.3.4")

(parse 'version-constraint ">=   2.3.4")

(parse 'distribution-constraint "hunchentoot >= 1.2.0")
(parse 'distribution-constraint "hunchentoot == 1.2.0")

(parse 'requirement "hunchentoot, hunchentoot <= 1.2.3")

(parse 'requirement "hunchentoot >= 1.2.0")
(parse 'requirement "hunchentoot >= 1.2.0 , hunchentoot <= 1.3.0 , hunchentoot")

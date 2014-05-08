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

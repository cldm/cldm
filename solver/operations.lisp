(in-package :cldm)

(defclass operation ()
  ((reason :initarg :reason
	   :initform nil
	   :accessor reason
	   :documentation "The reason of the operation"))
  (:documentation "Operations baseclass"))

(defclass update-operation (operation)
  ((from-library :initarg :from-library
		 :initform (error "Provide the library to update")
		 :accessor from-library
		 :documentation "The library to update")
   (to-library :initarg :to-library
	       :initform (error "Provide the library to update to")
	       :accessor to-library
	       :documentation "The library to update to"))
  (:documentation "A library update"))

(defclass install-operation (operation)
  ((library :initarg :library
	    :initform (error "Provide the library to install")
	    :accessor library
	    :documentation "The library to install"))
  (:documentation "A library install"))

(defclass uninstall-operation (operation)
  ((library :initarg :library
	    :initform (error "Provide the library to uninstall")
	    :accessor library
	    :documentation "The library to uninstall"))
  (:documentation "A library uninstall"))

(defmethod print-object ((operation update-operation) stream)
  (print-unreadable-object (operation stream :type t :identity t)
    (format stream "Update ~A (~A) to ~A (~A)"
	    (library-name (from-library operation))
	    (library-version (from-library operation))
	    (library-name (to-library operation))
	    (library-version (to-library operation)))))

(defmethod print-object ((operation install-operation) stream)
  (print-unreadable-object (operation stream :type t :identity t)
    (format stream "Install ~A (~A)"
	    (library-name (library operation))
	    (library-version (library operation)))))

(defmethod print-object ((operation uninstall-operation) stream)
  (print-unreadable-object (operation stream :type t :identity t)
    (format stream "Uninstall ~A (~A)"
	    (library-name (library operation))
	    (library-version (library operation)))))


(in-package :cldm)

(defclass solver ()
  ((pool :initarg :pool
	 :initform (error "Provide the pool")
	 :accessor solver-pool
	 :documentation "The pool of repositories to work with")
   (installed-repository
    :initarg :installed-repository
    :initform (error "Provide the installed respository")
    :accessor installed-repository
    :documentation "The repository with installed libraries")
   (policy :initarg :policy
	   :initform (make-instance 'default-solver-policy)
	   :accessor solver-policy
	   :documentation "The solver policy")
   ;; Locals
   (installed-table :accessor installed-table
		    :initform (make-hash-table :test #'equalp)
		    :documentation "Table containing installed libraries")
   (update-table :accessor update-table
		 :initform (make-hash-table :test #'equalp)
		 :documentation "Table that indicates libraries to update")
   (propagate-index :accessor propagate-index
		    :initform -1)
   (learnt-why :accessor learnt-why
	       :initform (make-hash-table :test #'equalp))
   (branches :accessor branches
	     :initform nil))
  (:documentation "Libraries dependencies solver"))

(defmethod solve ((solver solver) (request request))
  )


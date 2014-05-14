(in-package :cldm)

(defclass library-rule ()
  ((pool :initarg :pool
	 :initform (error "Provide the pool")
	 :accessor rule-pool
	 :documentation "The repositories pool")
   (literals :initarg :literals
	     :initform (error "Provide the literals")
	     :accessor rule-literals
	     :documentation "Rule literals")
   (type :initarg :type
	 :initform :unknown
	 :accessor rule-type
	 :documentation "The rule type")
   (reason :initarg :reason
	   :initform (error "Provide a reason")
	   :accessor rule-reason
	   :documentation "Rule reason")
   (reason-details :initarg :reason-details
		   :initform nil
		   :accessor rule-reason-details
		   :documentation "Reason details")
   (job :initarg :job
	:initform nil
	:accessor rule-job
	:documentation "Rule job")
   (id :initarg id
       :initform -1
       :accessor rule-id
       :documentation "Rule id"))
   
  (:documentation "A Rule where literals are library ids attached to a pool. It essentially allows for pretty-printing package names instead of internal ids as used by the SAT solver underneath."))

(defrule rule-literal (and (or #\+ #\-) library-unique-name)
  (:function (lambda (match)
	       (destructuring-bind (operation library) match
		   (cons (make-keyword operation) library)))))

(defrule rule (or (and rule-literal spaces #\| spaces rule)
		  rule-literal)
  (:function (lambda (match)
	       (if (equalp (length match) 5)
		   (list (first match)
			 (nth 4 match))
		   match))))

(defun read-library-rule-from-string (string &key
					       (pool (error "Provide the pool"))
					       (reason (error "Provide the reason"))
					       (reason-details "")
					       (job nil)
					       (id -1))				       
  "Creates a library-rule from a rule string, e.g. '-hunchentoot-1.6.0 | +hunchentoot-1.7.0'
   Because library full name -> id is not 1-to-1 mapping, this may fail  when a package has multiple ids. This is mostly used for testing, to write reference rules a bit more easily."
  (let ((literals (parse 'rule string))
	(library-literals nil))
    (loop for literal in literals
       do (destructuring-bind (operation library-name version) literal
	    (let ((requirement (make-instance 'requirement
					      :name library-name
					      :version-constraints (list
								    (if version
									(list :== version)
									:any)))))
	      (let ((library-candidates (what-provides pool requirement)))
		(cond
		  ((not library-candidates)
		   (error "No candidate for library ~A" (print-requirement-to-string requirement)))
		  ((> (length library-candidates) 1)
		   (error "More than one candidate for library ~A, cannot create rule from it"
			  (print-requirement-to-string requirement)))
		  (t
		   (ecase operation
		     (:+ (push (library-id (first library-candidates)) library-literals))
		     (:- (push (- (library-id (first library-candidates))) library-literals)))))))))
    (make-instance 'library-rule
		   :pool pool
		   :literals library-literals
		   :reason reason
		   :reason-details reason-details
		   :job job
		   :id id)))

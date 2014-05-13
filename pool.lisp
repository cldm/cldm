(in-package :cldm)

(defclass pool ()
  ((repositories :initarg :repositories
		 :initform nil
		 :accessor pool-repositories
		 :documentation "The pool repositories")
   (libraries-by-name :initform (make-hash-table :test #'equalp)
		      :accessor libraries-by-name
		      :documentation "Pool libraries by name")
   (libraries-by-id :initform (make-hash-table :test #'equalp)
		    :accessor libraries-by-id
		    :documentation "Pool libraries by id")
   (libraries-id :initform 1
		 :accessor libraries-id
		 :documentation "The id to be assigned to libraries. It is incremented each time a new library is added"))
  (:documentation "Pool objects model a pool of repositories. Pools are able to find libraries that provide a given requirements (handling the privides concept from library metadata)"))

(defmethod initialize-instance :after ((pool pool) &rest initargs)
  (let ((repositories (pool-repositories pool)))
    (setf (pool-repositories pool) nil)
    (loop for repository in repositories
       do (add-repository pool repository))))

(defmethod add-repository ((pool pool) repository)
  "Adds a repository to the pool. Adds repository libraries to the pool cache"
  
  (push repository (pool-repositories pool))
  (loop for library in (repository-libraries repository)
     do (let ((library-id (libraries-id pool)))
	  (setf (library-id library) library-id)
	  (incf (libraries-id pool))

	  ;; Add library by id to the pool
	  (setf (gethash library-id (libraries-by-id pool))
		library)
	    
	  ;; Add library by name to the pool
	  (push library (gethash (library-name library) (libraries-by-name pool)))

	  ;; Add provided libraries
	  (loop for provide in (library-provides library)
	     do (push library (gethash (requirement-name provide)
				       (libraries-by-name pool))))

	  ;; Add replacement libraries
	  (loop for replace in (library-replaces library)
	     do (push library (gethash (requirement-name replace)
				       (libraries-by-name pool)))))))

(defmethod find-library-by-id ((pool pool) id &optional (error-p t))
  "Retrieve a library from its id"
  (let ((library (gethash id (libraries-by-id pool))))
    (when (and (null library) error-p)
      (error "Library with id ~A not found in ~A" id pool))
    library))

(defmethod has-library ((pool pool) library)
  (not (null (find-library-by-id pool (library-id library) nil))))

(defmethod what-provides ((pool pool) requirement &optional (mode :composer))
  "Returns a list of libraries that provide the given requirement.

   requirement: The requirement to match.
   mode: One of :composer, :direct-only, :include-indirect
           - :composer : behaves like Composer does, i.e. only returns
             libraries that match this requirement directly, unless no
             match is found in which case packages that provide the
             requirement indirectly are returned.
           - :direct-only : only returns libraries that match this
             requirement directly (i.e. provides are ignored).
           - :include-indirect : only returns libraries that match this
             requirement directly or indirectly (i.e. includes libraries
             that provide this library)"
  (let ((name-match nil)
	(strict-matches nil)
	(provided-match nil))
    (loop for library in (gethash (requirement-name requirement)
				  (libraries-by-name pool))
       do (let ((match (library-matches library requirement)))
	    (case match
	      (:match-name (setf name-match t))
	      (:match (progn (setf name-match t)
			     (push library strict-matches)))
	      (:match-provide (push library provided-match))
	      (:match-replace (push library strict-matches)))
	    (ecase mode
	      (:composer (if name-match
			     strict-matches
			     (append strict-matches provided-match)))
	      (:direct-only strict-matches)
	      (:include-indirect (append strict-matches provided-match)))))

    (ecase mode
      (:composer (if name-match
		     strict-matches
		     (append strict-matches provided-match)))
      (:direct-only strict-matches)
      (:include-indirect (append strict-matches provided-match)))))

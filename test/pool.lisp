(in-package :cldm.test)

#.(enable-version-syntax)

(in-suite cldm-test)

(defparameter +hunchentoot-1.0.0+ (read-library-from-string "hunchentoot-1.0.0"))
(defparameter +hunchentoot-1.0.1+ (read-library-from-string "hunchentoot-1.0.1"))
(defparameter +hunchentoot-1.1.1+ (read-library-from-string "hunchentoot-1.1.1"))
(defparameter +hunchentoot-no-threaded-1.1.1+ (read-library-from-string "hunchentoot-no-threaded-1.1.1; depends (hunchentoot == 1.1.1); provides (hunchentoot == 1.1.1)"))

(defun req (string)
  (read-requirement-from-string string))

(deftest pool-find-library-by-id-test ()
  (let ((repository (make-instance 'repository
				   :name "test"
				   :libraries (list +hunchentoot-1.0.0+
						    +hunchentoot-1.0.1+))))
    (let ((pool (make-instance 'pool :repositories (list repository))))
      (is (library= (find-library-by-id pool (library-id +hunchentoot-1.0.0+))
		    +hunchentoot-1.0.0+))
      (is (library= (find-library-by-id pool (library-id +hunchentoot-1.0.1+))
		    +hunchentoot-1.0.1+))
      (signals error
	(library= (find-library-by-id pool (library-id +hunchentoot-1.1.1+))
		  +hunchentoot-1.1.1+)))))

(deftest pool-has-library-test ()
  (let ((repository (make-instance 'repository
				   :name "test"
				   :libraries (list +hunchentoot-1.0.0+
						    +hunchentoot-1.0.1+))))
    (let ((pool (make-instance 'pool :repositories (list repository))))
      (is (has-library pool +hunchentoot-1.0.0+))
      (is (has-library pool +hunchentoot-1.0.1+))
      (is (not (has-library pool +hunchentoot-1.1.1+))))))

(deftest add-repository-test ()
  "Ensure we do not add the same library twice"
  (let ((repository1 (make-instance 'repository
				    :name "test1"
				    :libraries (list +hunchentoot-1.0.0+
						     +hunchentoot-1.0.1+)))
	(repository2 (make-instance 'repository
				    :name "test2"
				    :libraries (list +hunchentoot-1.0.0+))))
    (let ((pool (make-instance 'pool :repositories (list repository1 repository2))))
      (is (equalp (length (what-provides pool (req "hunchentoot"))) 3)))))

(deftest what-provides-simple-test ()
  (let ((repository (make-instance 'repository
				   :name "test"
				   :libraries (list +hunchentoot-1.0.0+
						    +hunchentoot-1.0.1+))))
    (let ((pool (make-instance 'pool :repositories (list repository))))
      (is (set-equal (what-provides pool (req "hunchentoot"))
		     (list +hunchentoot-1.0.0+ +hunchentoot-1.0.1+)
		     :test #'library=))))

  (let ((repository (make-instance 'repository
				   :name "test"
				   :libraries (list
					       +hunchentoot-1.0.0+
					       +hunchentoot-1.1.1+))))
    (let ((pool (make-instance 'pool :repositories (list repository))))
      (is (set-equal (what-provides pool (req "hunchentoot"))
		     (list +hunchentoot-1.0.0+ +hunchentoot-1.1.1+)
		     :test #'library=)))))

(deftest what-provides-direct-only-test ()
  (let ((repository (make-instance 'repository
				   :name "test"
				   :libraries (list +hunchentoot-no-threaded-1.1.1+))))
    (let ((pool (make-instance 'pool :repositories (list repository))))
      (is (set-equal (what-provides pool (req "hunchentoot"))
		     (list +hunchentoot-no-threaded-1.1.1+)
		     :test #'library=)))))

(deftest what-provides-include-indirect-test ()
  (let ((repository (make-instance 'repository
				   :name "test"
				   :libraries (list +hunchentoot-1.0.0+
						    +hunchentoot-1.0.1+
						    +hunchentoot-1.1.1+
						    +hunchentoot-no-threaded-1.1.1+))))
    (let ((pool (make-instance 'pool :repositories (list repository))))
      (is (set-equal (what-provides pool (req "hunchentoot >= 1.0.1"))
		     (list +hunchentoot-1.0.1+
			   +hunchentoot-1.1.1+)
		     :test #'library=))
      (is (set-equal (what-provides pool (req "hunchentoot > 1.0.1"))
		     (list +hunchentoot-1.1.1+)
		     :test #'library=))
      (is (set-equal (what-provides pool (req "hunchentoot >= 1.0.1") :include-indirect)
		     (list +hunchentoot-1.0.1+
			   +hunchentoot-1.1.1+
			   +hunchentoot-no-threaded-1.1.1+)
		     :test #'library=))
      (is (set-equal (what-provides pool (req "hunchentoot > 1.0.1") :include-indirect)
		     (list +hunchentoot-1.1.1+
			   +hunchentoot-no-threaded-1.1.1+)
		     :test #'library=))
      (is (set-equal (what-provides pool (req "hunchentoot >= 1.0.1") :direct-only)
		     (list +hunchentoot-1.0.1+
			   +hunchentoot-1.1.1+)
		     :test #'library=)))))

(deftest what-provides-replaces-test ()
  (let ((hunchentoot-replacement (read-library-from-string "webserver-1.1.2; replaces (hunchentoot == 1.1.1)")))
    (let ((repository (make-instance 'repository
				     :name "test"
				     :libraries (list +hunchentoot-1.1.1+
						      hunchentoot-replacement))))
      (let ((pool (make-instance 'pool :repositories (list repository))))
	(is (set-equal (what-provides pool (req "webserver"))
		       (list hunchentoot-replacement)
		       :test #'library=))
	(is (set-equal (what-provides pool (req "hunchentoot"))
		       (list +hunchentoot-1.1.1+
			     hunchentoot-replacement)
		       :test #'library=))))))

#.(disable-version-syntax)

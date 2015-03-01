(in-package :cldm.test)

#.(enable-version-syntax)

(in-suite cldm-test)

(defparameter +hunchentoot-1.0.0+ (read-library-version-from-string "hunchentoot-1.0.0"))
(defparameter +hunchentoot-1.0.1+ (read-library-version-from-string "hunchentoot-1.0.1"))
(defparameter +hunchentoot-1.1.1+ (read-library-version-from-string "hunchentoot-1.1.1"))

(deftest repository-creation-test ()
  (let ((libraries (list +hunchentoot-1.0.0+
			 +hunchentoot-1.0.1+
			 +hunchentoot-1.1.1+)))
    (let ((repository (make-instance 'repository
				     :name "test"
				     :libraries libraries)))
    (is (set-equal libraries (repository-libraries repository)
		   :test #'library=)))))

(deftest repository-has-library-test ()
  (let ((libraries (list +hunchentoot-1.0.0+
			 +hunchentoot-1.0.1+
			 +hunchentoot-1.1.1+)))
    (let ((repository (make-instance 'repository
				     :name "test"
				     :libraries libraries)))
      (is (has-library repository +hunchentoot-1.0.1+))
      (is (has-library-named repository "hunchentoot"))
      (is (not (has-library-named repository "chunga"))))))

(deftest repository-find-library-test ()
  (let ((libraries (list +hunchentoot-1.0.0+
			 +hunchentoot-1.0.1+
			 +hunchentoot-1.1.1+)))
    (let ((repository (make-instance 'repository
				     :name "test"
				     :libraries libraries)))
      (is (find-library repository "hunchentoot" #v"1.0.0"))
      (is (find-library repository "hunchentoot" #v"1.0.1"))
      (is (not (find-library repository "hunchentoot" #v"1.2.1"))))))

#.(disable-version-syntax)

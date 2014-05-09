(in-package :cldm.test)

(in-suite cldm-test)

(deftest library-creation-test ()
  (let ((lib (make-instance 'library
			    :name "hunchentoot"
			    :version (read-version-from-string "1.0.0"))))
    (is (equalp (library-provides lib) nil))
    (is (equalp (library-dependencies lib) nil))
    (is (equalp (cldm::library-id lib) nil)))

  (let ((provides (list (make-instance 'requirement :name "chunga"))))))

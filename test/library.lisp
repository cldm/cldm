(in-package :cldm.test)

(in-suite cldm-test)

#.(enable-version-syntax)

(deftest library-creation-test ()
  (let ((lib (make-instance 'library
			    :name "hunchentoot"
			    :version #v"1.0.0")))
    (is (equalp (library-provides lib) nil))
    (is (equalp (library-dependencies lib) nil))
    (is (equalp (cldm::library-id lib) nil)))

  (let ((provides (list (make-requirement "chunga" '(:== #v"1.3.0")))))
    (let ((lib (make-instance 'library
			      :name "hunchentoot"
			      :version #v"1.0.0"
			      :provides provides)))
      (is (equalp (library-provides lib) provides)))))

(deftest library-unique-name-test ()
  (let ((lib (make-instance 'library
			    :name "hunchentoot"
			    :version #v"1.0.0")))
    (is (equalp (library-unique-name lib) "hunchentoot-1.0.0"))))

(deftest library-print-test ()
  (let ((lib (make-instance 'library
			    :name "hunchentoot"
			    :version #v"1.0.0")))
    (is (equalp (print-library-to-string lib) "hunchentoot-1.0.0"))))

(deftest read-library-from-string-test ()
  (let ((lib-string "hunchentoot-1.0.0"))
    (let ((lib (read-library-from-string lib-string)))
      (is (library= lib (make-instance 'library
				       :name "hunchentoot"
				       :version #v"1.0.0")))
      (is (equalp (library-unique-name lib) lib-string))
      (is (version= (library-version lib) #v"1.0.0")))))

(deftest library-dependencies-test ()
  (let ((lib-string "hunchentoot-1.0.0; depends (chunga >= 1.2.0)"))
    (let ((lib1 (read-library-from-string lib-string))
	  (lib2 (make-instance 'library
			       :name "hunchentoot"
			       :version #v"1.0.0"
			       :dependencies (list (read-requirement-from-string "chunga >= 1.2.0")))))
      (is (library= lib1 lib2))
      (is (equalp lib-string (print-library-to-string lib1))))))

(deftest library-provides-test ()
  (let ((lib-string "hunchentoot-1.0.0; provides (cl-ppcre == 1.4.0)"))
    (let ((lib1 (read-library-from-string lib-string))
	  (lib2 (make-instance 'library :name "hunchentoot"
			       :version #v"1.0.0"
			       :provides (list (read-requirement-from-string "cl-ppcre == 1.4.0")))))
      (is (library= lib1 lib2))
      (is (print-library-to-string lib2) lib-string))))

(deftest library-multiple-dependencies-test ()
  (let ((lib-string "hunchentoot-1.0.0; depends (chunga >= 1.0.0, cffi >= 2.0.0)"))
    (let ((lib1 (read-library-from-string lib-string))
	  (lib2 (make-instance 'library :name "hunchentoot"
			       :version #v"1.0.0"
			       :dependencies (list (read-requirement-from-string "chunga >= 1.0.0")
						   (read-requirement-from-string "cffi >= 2.0.0")))))
      (is (set-equal (library-dependencies lib1) (library-dependencies lib2) :test #'requirement=))
      (is (library= lib1 lib2))
      (is (equalp (print-library-to-string lib2) lib-string)))))

#.(disable-version-syntax)

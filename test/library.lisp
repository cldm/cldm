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

(deftest read-library-version-from-string-test ()
  (let ((lib-string "hunchentoot-1.0.0"))
    (let ((lib (read-library-version-from-string lib-string)))
      (is (library= lib (make-instance 'library
				       :name "hunchentoot"
				       :version #v"1.0.0")))
      (is (equalp (library-unique-name lib) lib-string))
      (is (version= (library-version lib) #v"1.0.0")))))

(deftest library-dependencies-test ()
  (let ((lib-string "hunchentoot-1.0.0; depends (chunga >= 1.2.0)"))
    (let ((lib1 (read-library-version-from-string lib-string))
	  (lib2 (make-instance 'library
			       :name "hunchentoot"
			       :version #v"1.0.0"
			       :dependencies (list (read-requirement-from-string "chunga >= 1.2.0")))))
      (is (library= lib1 lib2))
      (is (equalp lib-string (print-library-to-string lib1))))))

(deftest library-provides-test ()
  (let ((lib-string "hunchentoot-1.0.0; provides (cl-ppcre == 1.4.0)"))
    (let ((lib1 (read-library-version-from-string lib-string))
	  (lib2 (make-instance 'library :name "hunchentoot"
			       :version #v"1.0.0"
			       :provides (list (read-requirement-from-string "cl-ppcre == 1.4.0")))))
      (is (library= lib1 lib2))
      (is (print-library-to-string lib2) lib-string))))

(deftest library-multiple-dependencies-test ()
  (let ((lib-string "hunchentoot-1.0.0; depends (chunga >= 1.0.0, cffi >= 2.0.0)"))
    (let ((lib1 (read-library-version-from-string lib-string))
	  (lib2 (make-instance 'library :name "hunchentoot"
			       :version #v"1.0.0"
			       :dependencies (list (read-requirement-from-string "chunga >= 1.0.0")
						   (read-requirement-from-string "cffi >= 2.0.0")))))
      (is (set-equal (library-dependencies lib1) (library-dependencies lib2) :test #'requirement=))
      (is (library= lib1 lib2))
      (is (equalp (print-library-to-string lib2) lib-string)))))

(deftest library-matching-test ()
  (let ((hunchentoot-1.0.0 (read-library-version-from-string "hunchentoot-1.0.0")))
    (setf (library-provides hunchentoot-1.0.0)
	  (list (read-requirement-from-string "cl-ppcre")))
    (setf (library-replaces hunchentoot-1.0.0)
	  (list (read-requirement-from-string "webserver")))
    (is (not (library-matches hunchentoot-1.0.0 (read-requirement-from-string "chunga"))))
    (is (not (library-matches hunchentoot-1.0.0 (read-requirement-from-string "alexandria"))))
    (is (equalp
	 (library-matches hunchentoot-1.0.0 (read-requirement-from-string "hunchentoot"))
	 :match))
    (is (equalp
	 (library-matches hunchentoot-1.0.0 (read-requirement-from-library-string "hunchentoot-2.0.0"))
	 :match-name))
    (is (equalp
	 (library-matches hunchentoot-1.0.0 (read-requirement-from-string "cl-ppcre"))
	 :match-provide))
    (is (equalp
	 (library-matches hunchentoot-1.0.0 (read-requirement-from-string "webserver"))
	 :match-replace))
    (is (equalp
	 (library-matches hunchentoot-1.0.0 (read-requirement-from-string "webserver == 2.3.0"))
	 :match-replace))

    (is (equalp
	 (library-matches hunchentoot-1.0.0 (read-requirement-from-string "hunchentoot >= 2.0.0"))
	 :match-name))
    (is (equalp
	 (library-matches hunchentoot-1.0.0 (read-requirement-from-string "hunchentoot <= 2.0.0"))
	 :match))))

(deftest valid-library-name-test ()
  (let ((valid (list "chunga" "chunga-1.2.3"
		     "md5-3.4.5")))
    (loop for string in valid
       do (is (valid-library-name-p string))))
  (let ((invalid (list "" 23 nil t "lala-" "-0.2.1" "chunga-1.2"
		       "chunga-1.2.a")))
    (loop for string in invalid
	 do (is (not (valid-library-name-p string))))))

#.(disable-version-syntax)

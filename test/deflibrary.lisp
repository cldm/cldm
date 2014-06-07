(in-package :cldm.test)

;; Version extension tests

(deftest version-extesion-error-test ()
  (signals error
    (deflibrary my-library :cld "mylibrary.cld"
		:versions
		((:version "1.2.3" :extends "1.2.1"
			   :add-repositories ((:quicklisp (:url "http://quicklisp/mylibrary.tgz"))))
		 (:version "1.2.0"
			   :depends-on (("dep1" :version-constraints ((:== "1.0.0")))
					("dep2" :version-constraints ((:== "1.2.0"))))
			   :repositories ((:official (:url "http://my-library.org/mylibrary-1.2.0.tgz"))))))))

(deftest basic-version-extension-test ()
  (let ((library
	 (deflibrary my-library :cld "mylibrary.cld"
		     :versions
		     ((:version "1.2.3" :extends "1.2.0"
				:stability :beta
				:description "Beta tested")
		      (:version "1.2.0"
				:stability :alpha
				:depends-on (("dep1" :version-constraints ((:== "1.0.0")))
					     ("dep2" :version-constraints ((:== "1.2.0"))))
				:repositories ((:official (:url "http://my-library.org/mylibrary-1.2.0.tgz"))))))))

    (let ((my-library-1.2.3 (find (cldm::read-version-from-string "1.2.3")
				  (cldm::library-versions library)
				  :key #'cldm::version
				  :test #'cldm::version=)))
      (is (equalp (cldm::stability my-library-1.2.3) :beta))
      (is (equalp (cldm::description my-library-1.2.3) "Beta tested"))
      (is (find "dep1" (cldm::dependencies my-library-1.2.3)
		:key #'cldm::library-name :test #'equalp))
      (is (find "dep2" (cldm::dependencies my-library-1.2.3)
		:key #'cldm::library-name :test #'equalp)))))

(deftest version-repository-overwriting-test ()
  (let ((library
	 (deflibrary my-library :cld "mylibrary.cld"
		     :versions
		     ((:version "1.2.3" :extends "1.2.0"
				:repositories ((:quicklisp (:url "http://quicklisp/mylibrary.tgz"))))
		      (:version "1.2.0"
				:depends-on (("dep1" :version-constraints ((:== "1.0.0")))
					     ("dep2" :version-constraints ((:== "1.2.0"))))
				:repositories ((:official (:url "http://my-library.org/mylibrary-1.2.0.tgz"))))))))

    (let ((my-library-1.2.3 (find (cldm::read-version-from-string "1.2.3")
				  (cldm::library-versions library)
				  :key #'cldm::version
				  :test #'cldm::version=)))
      (is (find :quicklisp (cldm::repositories my-library-1.2.3)
		:key #'cldm::name
		:test #'equalp))
      (is (not (find :official (cldm::repositories my-library-1.2.3)
		     :key #'cldm::name
		     :test #'equalp))))))

(deftest version-repository-replacement-test ()
  (let ((library
	 (deflibrary my-library :cld "mylibrary.cld"
		     :versions
		     ((:version "1.2.3" :extends "1.2.0"
				:add-repositories ((:official (:url "http://my-library.net/mylibrary.tgz"))))
		      (:version "1.2.0"
				:depends-on (("dep1" :version-constraints ((:== "1.0.0")))
					     ("dep2" :version-constraints ((:== "1.2.0"))))
				:repositories ((:official (:url "http://my-library.org/mylibrary-1.2.0.tgz"))
					       (:github (:url "http://www.github.com/mylibrary-1.2.0.tgz"))))))))

    (let ((my-library-1.2.3 (find (cldm::read-version-from-string "1.2.3")
				  (cldm::library-versions library)
				  :key #'cldm::version
				  :test #'cldm::version=)))
      (is (length (cldm::repositories my-library-1.2.3)) 2)
      (is (find :github (cldm::repositories my-library-1.2.3)
		:key #'cldm::name
		:test #'equalp))
      (is (find :official (cldm::repositories my-library-1.2.3)
		:key #'cldm::name
		:test #'equalp))
      (is (equalp
	   (cldm::url (cldm::repository-address (first (cldm::repositories my-library-1.2.3))))
	   "http://my-library.net/mylibrary.tgz")))))
  
(deftest version-repository-addition-test ()
  (let ((library
	 (deflibrary my-library :cld "mylibrary.cld"
		     :versions
		     ((:version "1.2.3" :extends "1.2.0"
				:add-repositories ((:quicklisp (:url "http://quicklisp/mylibrary.tgz"))))
		      (:version "1.2.0"
				:depends-on (("dep1" :version-constraints ((:== "1.0.0")))
					     ("dep2" :version-constraints ((:== "1.2.0"))))
				:repositories ((:official (:url "http://my-library.org/mylibrary-1.2.0.tgz"))))))))

    (let ((my-library-1.2.3 (find (cldm::read-version-from-string "1.2.3")
				  (cldm::library-versions library)
				  :key #'cldm::version
				  :test #'cldm::version=)))
      (is (find :quicklisp (cldm::repositories my-library-1.2.3)
		:key #'cldm::name
		:test #'equalp))
      (is (find :official (cldm::repositories my-library-1.2.3)
		:key #'cldm::name
		:test #'equalp)))))

(deftest version-repository-removal-test ()
  (let ((library
	 (deflibrary my-library :cld "mylibrary.cld"
		     :versions
		     ((:version "1.2.3" :extends "1.2.0"
				:remove-repositories (:quicklisp))
		      (:version "1.2.0"
				:depends-on (("dep1" :version-constraints ((:== "1.0.0")))
					     ("dep2" :version-constraints ((:== "1.2.0"))))
				:repositories ((:official (:url "http://my-library.org/mylibrary-1.2.0.tgz"))
					       (:quicklisp (:url "http://quicklisp/mylibrary.tgz"))
					       (:github (:url "http://github.com/mylibrary.tgz"))))))))
    (let ((my-library-1.2.3 (find (cldm::read-version-from-string "1.2.3")
				  (cldm::library-versions library)
				  :key #'cldm::version
				  :test #'cldm::version=)))
      (is (not (find :quicklisp (cldm::repositories my-library-1.2.3)
		     :key #'cldm::name
		     :test #'equalp)))
      (is (find :official (cldm::repositories my-library-1.2.3)
		:key #'cldm::name
		:test #'equalp))
      (is (find :github (cldm::repositories my-library-1.2.3)
		:key #'cldm::name
		:test #'equalp))))

  (let ((library
	 (deflibrary my-library :cld "mylibrary.cld"
		     :versions
		     ((:version "1.2.3" :extends "1.2.0"
				:remove-repositories (:quicklisp :github))
		      (:version "1.2.0"
				:depends-on (("dep1" :version-constraints ((:== "1.0.0")))
					     ("dep2" :version-constraints ((:== "1.2.0"))))
				:repositories ((:official (:url "http://my-library.org/mylibrary-1.2.0.tgz"))
					       (:quicklisp (:url "http://quicklisp/mylibrary.tgz"))
					       (:github (:url "http://github.com/mylibrary.tgz"))))))))
    (let ((my-library-1.2.3 (find (cldm::read-version-from-string "1.2.3")
				  (cldm::library-versions library)
				  :key #'cldm::version
				  :test #'cldm::version=)))
      (is (not (find :quicklisp (cldm::repositories my-library-1.2.3)
		     :key #'cldm::name
		     :test #'equalp)))
      (is (find :official (cldm::repositories my-library-1.2.3)
		:key #'cldm::name
		:test #'equalp))
      (is (not (find :github (cldm::repositories my-library-1.2.3)
		:key #'cldm::name
		:test #'equalp))))))

(deftest version-dependency-replacement-test ()
  (let ((library
	 (deflibrary my-library :cld "mylibrary.cld"
		     :versions
		     ((:version "1.2.3" :extends "1.2.0"
				:stability :beta
				:description "Beta tested"
				:add-dependencies (("dep2" :version-constraints ((:== "2.0.0")))))
		      (:version "1.2.0"
				:stability :alpha
				:depends-on (("dep1" :version-constraints ((:== "1.0.0")))
					     ("dep2" :version-constraints ((:== "1.2.0"))))
				:repositories ((:official (:url "http://my-library.org/mylibrary-1.2.0.tgz"))))))))

    (let ((my-library-1.2.3 (find (cldm::read-version-from-string "1.2.3")
				  (cldm::library-versions library)
				  :key #'cldm::version
				  :test #'cldm::version=)))
      (is (equalp (cldm::stability my-library-1.2.3) :beta))
      (is (equalp (cldm::description my-library-1.2.3) "Beta tested"))
      (is (find "dep1" (cldm::dependencies my-library-1.2.3)
		:key #'cldm::library-name :test #'equalp))
      (let ((dep2 (find "dep2" (cldm::dependencies my-library-1.2.3)
		       :key #'cldm::library-name :test #'equalp)))
	(is (second (first (cldm::requirement-version-constraints dep2)))
	    (read-version-from-string "2.0.0"))))))

(deftest version-dependency-overwriting-test ()
  (let ((library
	 (deflibrary my-library :cld "mylibrary.cld"
		     :versions
		     ((:version "1.2.3" :extends "1.2.0"
				:stability :beta
				:description "Beta tested"
				:depends-on (("dep3" :version-constraints ((:== "1.0.0")))))
		      (:version "1.2.0"
				:stability :alpha
				:depends-on (("dep1" :version-constraints ((:== "1.0.0")))
					     ("dep2" :version-constraints ((:== "1.2.0"))))
				:repositories ((:official (:url "http://my-library.org/mylibrary-1.2.0.tgz"))))))))

    (let ((my-library-1.2.3 (find (cldm::read-version-from-string "1.2.3")
				  (cldm::library-versions library)
				  :key #'cldm::version
				  :test #'cldm::version=)))
      (is (equalp (cldm::stability my-library-1.2.3) :beta))
      (is (equalp (cldm::description my-library-1.2.3) "Beta tested"))
      (is (not (find "dep1" (cldm::dependencies my-library-1.2.3)
		:key #'cldm::library-name :test #'equalp)))
      (is (not (find "dep2" (cldm::dependencies my-library-1.2.3)
		     :key #'cldm::library-name :test #'equalp)))
      (is (find "dep3" (cldm::dependencies my-library-1.2.3)
		:key #'cldm::library-name :test #'equalp)))))

(deftest version-dependency-addition-test ()
  (let ((library
	 (deflibrary my-library :cld "mylibrary.cld"
		     :versions
		     ((:version "1.2.3" :extends "1.2.0"
				:stability :beta
				:description "Beta tested"
				:add-dependencies (("dep3" :version-constraints ((:== "1.0.0")))))
		      (:version "1.2.0"
				:stability :alpha
				:depends-on (("dep1" :version-constraints ((:== "1.0.0")))
					     ("dep2" :version-constraints ((:== "1.2.0"))))
				:repositories ((:official (:url "http://my-library.org/mylibrary-1.2.0.tgz"))))))))

    (let ((my-library-1.2.3 (find (cldm::read-version-from-string "1.2.3")
				  (cldm::library-versions library)
				  :key #'cldm::version
				  :test #'cldm::version=)))
      (is (equalp (cldm::stability my-library-1.2.3) :beta))
      (is (equalp (cldm::description my-library-1.2.3) "Beta tested"))
      (is (find "dep1" (cldm::dependencies my-library-1.2.3)
		:key #'cldm::library-name :test #'equalp))
      (is (find "dep2" (cldm::dependencies my-library-1.2.3)
		:key #'cldm::library-name :test #'equalp))
      (is (find "dep3" (cldm::dependencies my-library-1.2.3)
		:key #'cldm::library-name :test #'equalp)))))

(deftest version-dependency-removal-test ()
  (let ((library
	 (deflibrary my-library :cld "mylibrary.cld"
		     :versions
		     ((:version "1.2.3" :extends "1.2.0"
				:stability :beta
				:description "Beta tested"
				:remove-dependencies ("dep2"))
		      (:version "1.2.0"
				:stability :alpha
				:depends-on (("dep1" :version-constraints ((:== "1.0.0")))
					     ("dep2" :version-constraints ((:== "1.2.0")))
					     ("dep3" :version-constraints ((:== "1.0.0"))))
				:repositories ((:official (:url "http://my-library.org/mylibrary-1.2.0.tgz"))))))))

    (let ((my-library-1.2.3 (find (cldm::read-version-from-string "1.2.3")
				  (cldm::library-versions library)
				  :key #'cldm::version
				  :test #'cldm::version=)))
      (is (equalp (cldm::stability my-library-1.2.3) :beta))
      (is (equalp (cldm::description my-library-1.2.3) "Beta tested"))
      (is (equalp (length (cldm::dependencies my-library-1.2.3)) 2))
      (is (find "dep1" (cldm::dependencies my-library-1.2.3)
		:key #'cldm::library-name :test #'equalp))
      (is (not (find "dep2" (cldm::dependencies my-library-1.2.3)
		     :key #'cldm::library-name :test #'equalp)))
      (is (find "dep3" (cldm::dependencies my-library-1.2.3)
		:key #'cldm::library-name :test #'equalp)))))

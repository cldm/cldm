(defpackage cldm.test
  (:use :cl :cldm :alexandria :stefil)
  (:export :cldm-test))

(in-package :cldm.test)

(in-root-suite)

(defsuite cldm-test)

(in-suite cldm-test)

(deftest version-parsing-test ()
  (let ((valid-versions (list "1.2.0" "0.1.2"
			      "1.2.0-alpha" "1.2.0-alpha.1"
			      "1.2.0+build" "1.2.0+build.1"
			      "1.2.0-alpha+build" "1.2.0-alpha.1+build.2")))
    (loop for version in valid-versions
	 do (is (valid-version-p version))))

  (let ((invalid-versions (list "1.2" "1.2.a")))
    (loop for version in invalid-versions
	 do (is (not (valid-version-p version)))))
  (let ((pre-releases (list "1.0.0-alpha" "1.0.0-alpha.1" "1.0.0-0.3.7" "1.0.0-x.7.z.92")))
    ))

(deftest version-construction-test ()
  (let ((v1 (read-version-from-string "1.2.0" 'semantic-version))
	(v2 (make-instance 'semantic-version
			   :major 1
			   :minor 2
			   :patch 0)))
    (is (version= v1 v2))))

(deftest version-invalid-arguments-test ()
  (signals error
    (make-semantic-version "a" 2 0))
  (signals error
    (make-semantic-version 1 "a" 0))
  (signals error
    (make-semantic-version 1 2 "a"))
  (signals error
    (make-semantic-version 1 2 0 :pre-release "alpha"))
  (signals error
    (make-semantic-version 1 2 0 :build "build.1"))
  (signals error
    (read-version-from-string "1.2.a" 'semantic-version)))

(deftest version-printing-test ()
  (let ((version-strings (list "1.2.0" "0.1.2"
			      "1.2.0-alpha" "1.2.0-alpha.1"
			      "1.2.0+build" "1.2.0+build.1"
			      "1.2.0-alpha+build" "1.2.0-alpha.1+build.2")))
    (loop for version-string in version-strings
       do (is (prin1-to-string (read-version-from-string version-string 'semantic-version))
	      version-string))))

(deftest version-comparison-test ()
  (is (version= #v"1.2.0" #v"1.2.0"))
  (is (not (version= #v"1.2.0" #v"1.2.1")))
  
  (is (version/= #v"1.2.0" #v"1.2.1"))
  (is (not (version/= #v"1.2.0" #v"1.2.0")))

  (is (version> #v"1.2.1" #v"1.2.0"))
  (is (not (version> #v"1.2.0" #v"1.2.0")))
  (is (not (version> #v"1.2.0" #v"1.2.1")))

  (is (version>= #v"1.2.1" #v"1.2.0"))
  (is (version>= #v"1.2.0" #v"1.2.0"))
  (is (not (version>= #v"1.20" #v"1.2.1")))

  (is (version< #v"1.2.0" #v"1.2.1"))
  (is (not (version< #v"1.2.0" #v"1.2.0")))
  (is (not (version< #v"1.2.1" #v"1.2.0")))

  (is (version<= #v"1.2.0" #v"1.2.1"))
  (is (version<= #v"1.2.0" #v"1.2.0"))
  (is (not (version<= #v"1.2.1" #v"1.2.0"))))

(deftest rfc-example-test ()
  (let ((increasing-versions (mapcar #'read-version-from-string
				     (list "1.0.0-alpha"
					   "1.0.0-alpha.1"
					   "1.0.0-beta.2"
					   "1.0.0-beta.11"
					   "1.0.0-rc.1"
					   "1.0.0-rc.1+build.1"
					   "1.0.0"
					   "1.0.0+0.3.7"
					   "1.3.7+build"
					   "1.3.7+build.2.b8f12d7"
					   "1.3.7+build.11.e0f985a"))))
    (loop for v1 in increasing-versions
	 for v2 in (cdr increasing-versions)
	 do (progn
	      (is (version< v1 v2))
	      (is (version<= v1 v2))))

    (let ((decreasing-versions (reverse increasing-versions)))
      (loop for v1 in decreasing-versions
	   for v2 in (cdr decreasing-versions)
	   do (progn
		(is (version> v1 v2))
		(is (version>= v1 v2)))))))

(deftest min-max-version-test ()
  (is (version< :min-version #v"0.0.0"))
  (is (version<= :min-version #v"0.0.0"))
  (is (not (version> :min-version #v"0.0.0")))
  (is (not (version>= :min-version #v"0.0.0")))

  (is (version> :max-version #v"99.99.99"))
  (is (version>= :max-version #v"99.99.99"))
  (is (not (version< :max-version #v"99.99.99")))
  (is (not (version<= :max-version #v"99.99.99")))

  (is (not (version< :min-version :min-version)))
  (is (version<= :min-version :min-version))
  (is (not (version> :min-version :min-version)))
  (is (version>= :min-version :min-version))
  (is (version= :min-version :min-version))

  (is (not (version< :max-version :max-version)))
  (is (version<= :max-version :max-version))
  (is (not (version> :max-version :max-version)))
  (is (version>= :max-version :max-version))
  (is (version= :max-version :max-version)))

(deftest build-metadata-version-precedence-test ()
  "Build metadata SHOULD be ignored when determining version precedence. Thus two versions that differ only in the build metadata, have the same precedence. Examples: 1.0.0-alpha+001, 1.0.0+20130313144700, 1.0.0-beta+exp.sha.5114f85."
  (let ((versions (mapcar #'read-version-from-string
			  (list "1.0.0-alpha+001"
				"1.0.0+20130313144700"
				"1.0.0-beta+exp.sha.5114f85"))))
    (loop for v1 in versions
	 for v2 in (cdr versions)
	 do (progn
	      (is (not (version< v1 v2)))
	      (is (not (version> v1 v2)))))))

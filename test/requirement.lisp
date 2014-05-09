(in-package :cldm.test)

(in-suite cldm-test)

(deftest requirement-parser-test ()
  (let ((requirement (read-requirement-from-string "hunchentoot >= 1.3.0")))
    (is (equalp (requirement-name requirement) "hunchentoot"))
    (is (equalp (requirement-spec requirement) '((>= #v"1.3.0"))))
    (is (requirement= requirement (make-instance 'requirement
						 :name "hunchentoot"
						 :spec `((>= #v"1.3.0")))))

    (let ((requirement (read-requirement-from-string
			"hunchentoot >= 1.3.0, hunchentoot <= 2.0.0")))
      (is (equalp (requirement-name requirement) "hunchentoot"))
      (is (equalp (requirement-spec requirement) '((>= #v"1.3.0")
						   (<= #v"2.0.0" ))))
      (is (requirement= requirement (make-requirement "hunchentoot"
						      '(>= #v"1.3.0")
						      '(<= #v"2.0.0")))))

    (let ((requirement (read-requirement-from-string
			"hunchentoot == 1.3.0")))
      (is (equalp (requirement-name requirement) "hunchentoot"))
      (is (equalp (requirement-spec requirement)) '((== #v"1.3.0")))
      (is (requirement= requirement (make-requirement "hunchentoot" '(== #v"1.3.0")))))  

    (let ((requirement (read-requirement-from-string
			"hunchentoot == 1.3.0, hunchentoot == 1.4.0")))
      (is (equalp (requirement-name requirement) "hunchentoot"))
      (is (equalp (requirement-spec requirement)) '((== #v"1.3.0")
						    (== #v"1.4.0")))
      (is (requirement-cannot-match-p requirement)))

    (let ((requirement (read-requirement-from-string "hunchentoot")))
      (is (equalp (requirement-name requirement) "hunchentoot"))
      (is (equalp (requirement-spec requirement) nil))
      (is (requirement= requirement (make-requirement "hunchentoot"))))

    (signals error
      (read-requirement-from-string "hunchentoot <= 1.2.0, hunchentoot >= 2.3.2"))))

(deftest requirement-printing-test ()
  (let ((requirement-strings (list "hunchentoot <= 1.3.0"
				   "hunchentoot >= 1.3.0"
				   "hunchentoot == 2.0.0"
				   "hunchentoot"
				   "hunchentoot *"
				   "hunchentoot > 1.3.0, hunchentoot < 2.0.0"
				   "hunchentoot != 1.3.0"
				   "hunchentoot < 3.0.0")))
    (loop for requirement-string in requirement-strings
	 do (let ((requirement (read-requirement-from-string requirement-string)))
	      (is (equalp (print-requirement-to-string requirement)
			  requirement-string))))))

(deftest requirement-matches-test ()
  (flet ((req (string)
	   (read-requirement-from-string string)))
    (let ((requirement (req "hunchentoot >= 1.3.0, hunchentoot <= 1.4.0")))
      (is (requirement-matches requirement (req "hunchentoot")))
      (is (not (requirement-matches requirement (req "hunchentoot == 1.2.0"))))
      (is (requirement-matches requirement (req "hunchentoot == 1.3.0")))
      (is (requirement-matches requirement (req "hunchentoot == 1.4.0")))
      (is (not (requirement-matches requirement (req "hunchentoot == 1.5.0"))))

      

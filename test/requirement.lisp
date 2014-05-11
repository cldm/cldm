(in-package :cldm.test)

#.(enable-version-syntax)

(in-suite cldm-test)

(deftest requirement-parser-test ()
  (let ((requirement (read-requirement-from-string "hunchentoot >= 1.3.0")))
    (is (requirement= requirement (make-instance 'requirement
						 :name "hunchentoot"
						 :version-constraints `((:>= #v"1.3.0")))))

    (let ((requirement (read-requirement-from-string
			"hunchentoot >= 1.3.0, hunchentoot <= 2.0.0")))
      (is (requirement= requirement (make-requirement "hunchentoot"
						      '(:>= #v"1.3.0")
						      '(:<= #v"2.0.0")))))

    (let ((requirement (read-requirement-from-string
			"hunchentoot == 1.3.0")))
      (is (requirement= requirement (make-requirement "hunchentoot" '(:== #v"1.3.0"))))) 

    (let ((requirement (read-requirement-from-string
			"hunchentoot == 1.3.0, hunchentoot == 1.4.0")))
      (is (requirement= requirement (make-requirement "hunchentoot"
						      '(:== #v"1.3.0")
						      '(:== #v"1.4.0")))))

    (let ((requirement (read-requirement-from-string "hunchentoot")))
      (is (requirement= requirement (make-requirement "hunchentoot"))))))

(deftest requirement-cannot-match-test ()
  (let ((requirement (read-requirement-from-string
			"hunchentoot >= 1.3.0, hunchentoot <= 2.0.0")))
    (is (not (requirement-cannot-match-p requirement))))

  (let ((requirement (read-requirement-from-string
			"hunchentoot >= 2.0.0, hunchentoot <= 1.3.0")))
    (is (requirement-cannot-match-p requirement)))
  
  (let ((requirement (read-requirement-from-string
		      "hunchentoot == 1.3.0, hunchentoot == 1.4.0")))
    (is (requirement-cannot-match-p requirement)))

  (let ((requirement (read-requirement-from-string
			"hunchentoot == 1.3.0")))
    (is (not (requirement-cannot-match-p requirement)))))  

(deftest requirement-printing-test ()
  (let ((requirement-strings (list "hunchentoot <= 1.3.0"
				   "hunchentoot >= 1.3.0"
				   "hunchentoot == 2.0.0"
				   "hunchentoot"
				   ;"hunchentoot *"
				   "hunchentoot > 1.3.0, hunchentoot < 2.0.0"
				   "hunchentoot != 1.3.0"
				   "hunchentoot < 3.0.0")))
    (loop for requirement-string in requirement-strings
	 do (let ((requirement (read-requirement-from-string requirement-string)))
	      (is (equalp (print-requirement-to-string requirement)
			  requirement-string))))))

(defun req (string)
  (read-requirement-from-string string))

(deftest requirement-matches-test ()
  (let ((requirement (req "hunchentoot >= 1.3.0, hunchentoot <= 1.4.0")))
      ;; equality constraints
      (is (requirement-matches requirement (req "hunchentoot")))
      (is (not (requirement-matches requirement (req "hunchentoot == 1.2.0"))))
      (is (requirement-matches requirement (req "hunchentoot == 1.3.0")))
      (is (requirement-matches requirement (req "hunchentoot == 1.4.0")))
      (is (not (requirement-matches requirement (req "hunchentoot == 1.5.0"))))

      ;; different library name
      (is (not (requirement-matches requirement (req "cl-ppcre == 1.3.0"))))

      ;; range
      (is (requirement-matches requirement (req "hunchentoot >= 1.3.0")))
      (is (requirement-matches requirement (req "hunchentoot >= 1.4.0")))
      (is (not (requirement-matches requirement (req "hunchentoot >= 1.5.0"))))

      (is (requirement-matches requirement (req "hunchentoot >= 1.3.0, hunchentoot <= 1.5.0")))
      (is (requirement-matches requirement (req "hunchentoot >= 1.4.0, hunchentoot <= 1.5.0")))
      (is (not (requirement-matches requirement (req "hunchentoot >= 1.5.0, hunchentoot <= 1.6.0"))))
      (is (requirement-matches requirement (req "hunchentoot >= 1.2.0, hunchentoot <= 1.3.0")))
      (is (not (requirement-matches requirement (req "hunchentoot >= 1.2.0, hunchentoot <= 1.2.5")))))

    (let ((requirement (req "hunchentoot == 1.3.0")))
      (is (requirement-matches requirement (req "hunchentoot == 1.3.0")))
      (is (not (requirement-matches requirement (req "hunchentoot == 1.2.0"))))
      (is (requirement-matches requirement (req "hunchentoot >= 1.3.0")))
      (is (requirement-matches requirement (req "hunchentoot <= 1.4.0"))))

    (let ((requirement (req "hunchentoot > 1.3.0, hunchentoot < 1.4.0")))
      (is (requirement-matches requirement (req "hunchentoot == 1.3.5")))
      (is (not (requirement-matches requirement (req "hunchentoot == 1.3.0"))))
      (is (not (requirement-matches requirement (req "hunchentoot == 1.4.0"))))
      (is (requirement-matches requirement (req "hunchentoot >= 1.3.2, hunchentoot <= 1.3.4")))
      (is (requirement-matches requirement (req "hunchentoot != 1.3.2"))))

    #+nil(let ((req1 (req "hunchentoot >= 1.3.0, hunchentoot <= 1.5.0, hunchentoot <= 1.4.0"))
	  (req2 (req "hunchentoot <= 1.3.0, hunchentoot <= 1.4.0")))
      (is (requirement= req1 req2)))

    (let ((requirement (req "hunchentoot >= 1.3.0, hunchentoot <= 1.2.0")))
      (is (not (requirement-matches requirement (req "hunchentoot")))))

    (let ((requirement (req "hunchentoot > 1.3.0, hunchentoot < 1.4.0")))
      (is (requirement-matches requirement (req "hunchentoot")))
      (is (requirement-matches requirement (req "hunchentoot == 1.3.5")))
      (is (not (requirement-matches requirement (req "hunchentoot == 1.3.0"))))
      (is (not (requirement-matches requirement (req "hunchentoot == 1.4.0")))))

    (let ((requirement (req "hunchentoot == 1.3.0"))
	  (lib-requirement (read-requirement-from-library-string "hunchentoot-1.3.0")))
      (is (requirement= requirement lib-requirement))))

#.(disable-version-syntax)

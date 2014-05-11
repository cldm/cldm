(in-package :cldm)

(defclass version ()
  ()
  (:documentation "Library version"))

(defclass semantic-version (version)
  ((major :initarg :major
	  :accessor version-major
	  :initform (error "Provide the major version number")
	  :type integer
	  :documentation "The major version number")
   (minor :initarg :minor
	  :accessor version-minor
	  :initform (error "Provide the minor version number")
	  :type integer
	  :documentation "The minor version number")
   (patch :initarg :patch
	  :accessor version-patch
	  :initform (error "Provide the patch version number")
	  :type integer
	  :documentation "The patch (or micro) version number")
   (pre-release :initarg :pre-release
		:accessor version-pre-release
		:initform nil
		:type integer
		:documentation "The pre release version number")
   (build :initarg :build
	  :accessor version-build
	  :initform nil
	  :type integer
	  :documentation "The build version number"))
  (:documentation "Instances represent a full version according to the semantic version specs (version 2.0.0-rc1 of the spec). http://semver.org/ . The main features of this class are validation and version comparison."))

(defparameter +version-re+ "^(\\d+).(\\d+).(\\d+)(?:-([0-9a-zA-Z-]+(?:\\.[0-9a-zA-Z-]+)*))?(?:\\+([0-9a-zA-Z-]+(?:\\.[0-9a-zA-Z-]+)*))?$")

(defun version-valid-p (string)
  (not (null (ppcre:scan +version-re+ string))))

(defun read-version-from-string (string &optional (class 'semantic-version))
  (when (not (version-valid-p string))
    (error "Could not parse version string ~S" string))
  (ppcre:register-groups-bind (major minor patch pre-release build)
      (+version-re+ string)
    (make-instance class
		   :major (parse-integer major)
		   :minor (parse-integer minor)
		   :patch (parse-integer patch)
		   :pre-release pre-release
		   :build build)))

(defmethod initialize-instance :after ((version semantic-version) &rest initargs)
  ;; Validate the version
  (let ((version-string (print-version-to-string version)))
    (when (not (version-valid-p version-string))
      (error "Version ~S is not valid" version-string))))

(defun print-version (version stream)
  (format stream "~A.~A.~A"
	  (version-major version)
	  (version-minor version)
	  (version-patch version))
  (when (version-pre-release version)
    (format stream "-~A" (version-pre-release version)))
  (when (version-build version)
    (format stream "+~A" (version-build version))))

(defun print-version-to-string (version)
  (with-output-to-string (s)
    (print-version version s)))

(defmethod print-object ((version semantic-version) stream)
  (format stream "#v\"~A\"" (print-version-to-string version)))

;; Version comparison
(defmethod version= (version1 version2)
  nil)
(defmethod version= ((version1 version) (version2 version))
  (and (equalp (version-major version1)
	       (version-major version2))
       (equalp (version-minor version1)
	       (version-minor version2))
       (equalp (version-patch version1)
	       (version-patch version2))))

(defmethod version== (version1 version2)
  nil)
(defmethod version== ((version1 version) (version2 version))
  (and (version= version1 version2)
       (equalp (version-pre-release version1)
	       (version-pre-release version2))
       (equalp (version-build version1)
	       (version-build version2))))

(defmethod version/= (version1 version2)
  t)

(defmethod version/= ((version1 version) (version2 version))
  (not (version= version1 version2)))

(defmethod version/== (version1 version2)
  t)
(defmethod version/== ((version1 version) (version2 version))
  (not (version== version1 version2)))

(defun tuple< (t1 t2)
  (when (and t1 t2)
    (let ((v1 (first t1))
	  (v2 (first t2)))
      (or (< v1 v2)
	  (tuple< (rest t1)
		  (rest t2))))))

(defmethod version< ((version1 (eql :min-version)) version2)
  t)
(defmethod version< (version1 (version2 (eql :max-version)))
  t)
(defmethod version< ((version1 (eql :max-version)) version2)
  nil)
(defmethod version< (version1 (version2 (eql :min-version)))
  nil)
(defmethod version< ((version1 version) (version2 version))
  (tuple< (list (version-major version1)
		(version-minor version1)
		(version-patch version1))
	  (list (version-major version2)
		(version-minor version2)
		(version-patch version2))))

(defun version<= (version1 version2)
  (or (version= version1 version2)
      (version< version1 version2)))

(defun version> (version1 version2)
  (not (version<= version1 version2)))

(defun version>= (version1 version2)
  (or (version= version1 version2)
      (version> version1 version2)))

(defun make-semantic-version (major minor patch &optional pre-release build)
  (make-instance 'semantic-version
		 :major major
		 :minor minor
		 :patch patch
		 :pre-release pre-release
		 :build build))

;; Version syntax

(defvar *previous-readtables* nil)
(defun version-syntax-reader (stream subchar arg)
  (declare (ignore subchar arg))
  (read-version-from-string (read stream t)))

(defun %enable-version-syntax ()
  "Internal function used to enable reader syntax and store current
readtable on stack."
  (push *readtable*
        *previous-readtables*)
  (setq *readtable* (copy-readtable))
  (set-dispatch-macro-character #\# #\v #'version-syntax-reader)
  (values))

(defun %disable-version-syntax ()
  "Internal function used to restore previous readtable." 
  (if *previous-readtables*
    (setq *readtable* (pop *previous-readtables*))
    (setq *readtable* (copy-readtable nil)))
  (values))

(defmacro enable-version-syntax ()
  "Enable version reader syntax."
  `(eval-when (:compile-toplevel :load-toplevel :execute)
    (%enable-version-syntax)))

(defmacro disable-version-syntax ()
  "Restore readtable which was active before last call to
ENABLE-VERSION-SYNTAX. If there was no such call, the standard
readtable is used."
  `(eval-when (:compile-toplevel :load-toplevel :execute)
    (%disable-version-syntax)))


#+nil(defsyntax version-syntax
  (:dispatch-macro-char #\# #\v #'version-syntax-reader))

;; TODO: do something about version precedence and pre-release
;;
;; Precedence refers to how versions are compared to each other when ordered. Precedence MUST be calculated by separating the version into major, minor, patch and pre-release identifiers in that order (Build metadata does not figure into precedence). Precedence is determined by the first difference when comparing each of these identifiers from left to right as follows: Major, minor, and patch versions are always compared numerically. Example: 1.0.0 < 2.0.0 < 2.1.0 < 2.1.1. When major, minor, and patch are equal, a pre-release version has lower precedence than a normal version. Example: 1.0.0-alpha < 1.0.0. Precedence for two pre-release versions with the same major, minor, and patch version MUST be determined by comparing each dot separated identifier from left to right until a difference is found as follows: identifiers consisting of only digits are compared numerically and identifiers with letters or hyphens are compared lexically in ASCII sort order. Numeric identifiers always have lower precedence than non-numeric identifiers. A larger set of pre-release fields has a higher precedence than a smaller set, if all of the preceding identifiers are equal. Example: 1.0.0-alpha < 1.0.0-alpha.1 < 1.0.0-alpha.beta < 1.0.0-beta < 1.0.0-beta.2 < 1.0.0-beta.11 < 1.0.0-rc.1 < 1.0.0.

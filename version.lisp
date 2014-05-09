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

(defun read-version-from-string (string &optional (class 'semantic-version))
  (let ((version-re "^(\\d+).(\\d+).(\\d+)(?:-([0-9a-zA-Z-]+(?:\\.[0-9a-zA-Z-]+)*))?(?:\\+([0-9a-zA-Z-]+(?:\\.[0-9a-zA-Z-]+)*))?$"))
    (when (not (ppcre:scan version-re string))
      (error "Could not parse version string ~S" string))
    (ppcre:register-groups-bind (major minor patch pre-release build)
	(version-re string)
      (make-instance class
		     :major (parse-integer major)
		     :minor (parse-integer minor)
		     :patch (parse-integer patch)
		     :pre-release pre-release
		     :build build))))

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

(set-dispatch-macro-character
 #\# #\v
 (lambda (stream subchar arg)
   (declare (ignore subchar arg))
   (read-version-from-string (read stream t))))

(defmethod print-object ((version semantic-version) stream)
  (format stream "#v\"~A\"" (print-version-to-string version)))

(defun version= (version1 version2)
  (and (equalp (version-major version1)
	       (version-major version2))
       (equalp (version-minor version1)
	       (version-minor version2))
       (equalp (version-patch version1)
	       (version-patch version2))
       (equalp (version-pre-release version1)
	       (version-pre-release version2))
       (equalp (version-build version1)
	       (version-build version2))))

(defun version< (version1 version2)
  )

(defun version> (version1 version2)
  )

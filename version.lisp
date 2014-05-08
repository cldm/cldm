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

(defun read-version-from-string (string))

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

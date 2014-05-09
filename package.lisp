(defpackage #:cldm
  (:use #:cl #:alexandria)
  (:export #:read-version-from-string
	   #:print-version
	   #:print-version-to-string
	   #:version
	   #:semantic-version
	   #:version-major
	   #:version-minor
	   #:version-patch
	   #:version-pre-release
	   #:version-build
	   #:version=
	   #:version/=
	   #:version<
	   #:version<=
	   #:version>
	   #:version>=))

(defpackage #:cldm
  (:use #:cl #:alexandria #:cl-syntax #:esrap)
  (:export #:read-version-from-string
	   #:print-version
	   #:print-version-to-string
	   #:version
	   #:semantic-version
	   #:make-semantic-version
	   #:version-valid-p
	   #:version-major
	   #:version-minor
	   #:version-patch
	   #:version-pre-release
	   #:version-build
	   #:version=
	   #:version==
	   #:version/=
	   #:version/==
	   #:version<
	   #:version<=
	   #:version>
	   #:version>=
	   #:enable-version-syntax
	   #:disable-version-syntax

	   #:read-requirement-from-string
	   #:read-requirement-from-library-string
	   #:requirement-matches
	   #:print-requirement
	   #:requirement
	   #:print-requirement-to-string
	   #:requirement-name
	   #:requirement=
	   #:requirement-version-constraints
	   #:requirement-cannot-match-p
	   #:make-requirement

	   #:library
	   #:library-name
	   #:library-dependencies
	   #:library-provides
	   #:library-conflicts
	   #:library-suggests
	   #:library-replaces
	   #:library-unique-name
	   #:library-version
	   #:print-library
	   #:print-library-to-string
	   #:read-library-from-string
	   #:library=
	   #:library-matches
	   ))

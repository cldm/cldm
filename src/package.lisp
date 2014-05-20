(defpackage #:cldm
  (:use #:cl #:alexandria #:cl-syntax #:esrap #:puri)
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
	   #:valid-library-name-p
	   #:print-library
	   #:print-library-to-string
	   #:read-library-from-string
	   #:library=
	   #:library-matches
	   #:library-id

	   #:repository
	   #:repository-name
	   #:find-library
	   #:find-libraries
	   #:has-library-named
	   #:has-library
	   #:repository-libraries
	   #:repository-length

	   #:pool
	   #:pool-repositories
	   #:add-repository
	   #:find-library-by-id
	   #:has-library
	   #:what-provides

	   #:deflibrary
	   #:list-cld-libraries
	   #:find-cld-library
	   #:setup
	   #:load-library
	   #:with-libraries-directory
	   #:with-cld-repositories
	   ))

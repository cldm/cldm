(defpackage #:cldm
  (:use #:cl #:alexandria #:anaphora #:cl-syntax #:esrap #:puri)
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
	   #:read-requirement-from-library-version-string
	   #:requirement-matches
	   #:print-requirement
	   #:requirement
	   #:print-requirement-to-string
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
	   #:read-library-version-from-string
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
	   
	   #:http-cld-repository
	   #:ssh-cld-repository
	   #:directory-cld-repository
	   #:cached-http-cld-repository
	   #:cached-ssh-cld-repository
	   #:indexed-http-cld-repository
	   #:indexed-ssh-cld-repository
	   #:find-cld-repository
	   #:publish-cld

	   #:deflibrary
	   #:list-all-libraries
	   #:find-library
	   #:install-library
	   #:load-library
	   #:install-project
	   #:load-project
	   #:update-project
	   #:with-libraries-directory
	   #:with-cld-repositories
	   #:list-cld-repositories
	   
	   #:*cld-repositories*
	   #:*minisat+-binary*
	   #:*libraries-directory*
	   #:*local-libraries-directory*

	   ;; asdf
	   #:deflibrary-file-from-asdf-system
	   #:deflibrary-from-asdf-system
	   ))

(defpackage cldm-loader
  (:use :cl)
  (:export #:*libraries-directory*
	   #:*local-libraries-directory*))	   

(in-package :cldm-loader)

(defparameter *libraries-directory*
  (pathname "~/.cldm/cache/libraries/"))

(defparameter *local-libraries-directory*
  (merge-pathnames (pathname "lib/")
		   (osicat:current-directory)))

;; From CL-FAD:

(defun pathname-as-directory (pathspec)
  "Converts the non-wild pathname designator PATHSPEC to directory
form."
  (let ((pathname (pathname pathspec)))
    (when (wild-pathname-p pathname)
      (error "Can't reliably convert wild pathnames."))
    (cond ((not (directory-pathname-p pathspec))
           (make-pathname :directory (append (or (pathname-directory pathname)
                                                 (list :relative))
                                             (list (file-namestring pathname)))
                          :name nil
                          :type nil
                          :defaults pathname))
          (t pathname))))

(defun directory-wildcard (dirname)
  "Returns a wild pathname designator that designates all files within
the directory named by the non-wild pathname designator DIRNAME."
  (when (wild-pathname-p dirname)
    (error "Can only make wildcard directories from non-wildcard directories."))
  (make-pathname :name #-:cormanlisp :wild #+:cormanlisp "*"
                 :type #-(or :clisp :cormanlisp) :wild
                       #+:clisp nil
                       #+:cormanlisp "*"
                 :defaults (pathname-as-directory dirname)))

(defun list-directory (dirname &key (follow-symlinks t))
  "Returns a fresh list of pathnames corresponding to all files within
   the directory named by the non-wild pathname designator DIRNAME.
   The pathnames of sub-directories are returned in directory form -
   see PATHNAME-AS-DIRECTORY.

  If FOLLOW-SYMLINKS is true, then the returned list contains
truenames (symlinks will be resolved) which essentially means that it
might also return files from *outside* the directory.  This works on
all platforms.

  When FOLLOW-SYMLINKS is NIL, it should return the actual directory
contents, which might include symlinks.  Currently this works on SBCL
and CCL."
  (declare (ignorable follow-symlinks))
  (when (wild-pathname-p dirname)
    (error "Can only list concrete directory names."))
  #+:ecl
  (let ((dir (pathname-as-directory dirname)))
    (concatenate 'list
                 (directory (merge-pathnames (pathname "*/") dir))
                 (directory (merge-pathnames (pathname "*.*") dir))))
  #-:ecl
  (let ((wildcard (directory-wildcard dirname)))
    #+:abcl (system::list-directory dirname)
    #+:sbcl (directory wildcard :resolve-symlinks follow-symlinks)
    #+(or :cmu :scl :lispworks) (directory wildcard)
    #+(or :openmcl :digitool) (directory wildcard :directories t :follow-links follow-symlinks)
    #+:allegro (directory wildcard :directories-are-files nil)
    #+:clisp (nconc (directory wildcard :if-does-not-exist :keep)
                    (directory (clisp-subdirectories-wildcard wildcard)))
    #+:cormanlisp (nconc (directory wildcard)
                         (cl::directory-subdirs dirname)))
  #-(or :sbcl :cmu :scl :lispworks :openmcl :allegro :clisp :cormanlisp :ecl :abcl :digitool)
  (error "LIST-DIRECTORY not implemented"))

(defun component-present-p (value)
  "Helper function for DIRECTORY-PATHNAME-P which checks whether VALUE
   is neither NIL nor the keyword :UNSPECIFIC."
  (and value (not (eql value :unspecific))))

(defun directory-pathname-p (pathspec)
  "Returns NIL if PATHSPEC \(a pathname designator) does not designate
a directory, PATHSPEC otherwise.  It is irrelevant whether file or
directory designated by PATHSPEC does actually exist."
  (and
    (not (component-present-p (pathname-name pathspec)))
    (not (component-present-p (pathname-type pathspec)))
    pathspec))

;; ASDF plugging

;; A very bad and dumb ASDF system searcher for now
;; Try to improve using repositories metadata, among other things
(defun asdf-system-directory-search (name directory)
  (loop for dir in (list-directory directory)
     for asd-file = (merge-pathnames (pathname (format nil "~A.asd" name)) dir)
     when (and (directory-pathname-p dir)
               (probe-file asd-file))
     return asd-file))

(defun asdf-system-search (name)
  (let ((system-name (or (and (symbolp name)
                              (string-downcase (symbol-name name)))
                         name)))
    (let ((libraries-directories (list *local-libraries-directory* *libraries-directory*)))
      (loop for libraries-directory in libraries-directories
         for system-filename = (asdf-system-directory-search system-name libraries-directory)
         when system-filename
         return system-filename))))

(push 'asdf-system-search asdf:*system-definition-search-functions*)

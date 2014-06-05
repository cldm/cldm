(in-package :cldm)

(defclass library-version-repository ()
  ((library-version :initarg :library-version
                    :initform nil
                    :accessor library-version
                    :documentation "The library version of the repository")
   (name :initarg :name
         :initform (error "Provide the repository name")
         :accessor name
         :documentation "The repository name")
   (address :initarg :address
            :initform (error "Provide the repository address")
            :accessor repository-address
            :documentation "The repository address. Can be a pathname, an url or a git reference"))
  (:documentation "A library version repository"))

(defmethod print-object ((version-repository library-version-repository) stream)
  (print-unreadable-object (version-repository stream :type t :identity t)
    (print-library-version (library-version version-repository)
                           stream)
    (format stream " ~A ~A"
            (name version-repository)
            (repository-address version-repository))))

(defmacro deflibrary (name &body options)
  (destructuring-bind (&key author maintainer description
                            licence cld versions tags)
      options
    `(make-instance 'library
                    :name ',(if (symbolp name)
                                (string-downcase (symbol-name name))
                                name)
                    :author ',author
                    :maintainer ',maintainer
                    :description ,description
                    :licence ,licence
                    :cld (parse-cld-address ',cld)
                    :versions ,(parse-library-versions versions)
                    :tags ',tags)))

(defun parse-library-versions (versions)
  `(list
    ,@(loop for version in versions
         collect
           (destructuring-bind (version-keyword version &key stability description repositories depends-on)
               version
             (assert (equalp version-keyword :version) nil "Invalid token ~A" version-keyword)
             `(make-instance 'library-version
                             :version (read-version-from-string ,version)
                             :stability ,stability
                             :description ,description
                             ,@(let ((repositories (parse-version-repositories repositories)))
                                    (when repositories
                                      (list :repositories repositories)))
                             ,@(let ((dependencies (parse-version-dependencies depends-on)))
                                    (when dependencies
                                      (list :dependencies dependencies))))))))

(defun parse-version-repositories (repositories)
  `(list
    ,@(loop for repository in repositories
         collect
           (destructuring-bind (name address) repository
             `(make-instance 'library-version-repository
                             :name ,name
                             :address ,(parse-version-repository-address address))))))

(defun parse-version-dependencies (dependencies)
  `(list
    ,@(loop for dependency in dependencies
         collect
           (if (listp dependency)
               (destructuring-bind (library-name &key version cld) dependency
                 `(make-instance 'requirement
                                 :library-name ',(if (symbolp library-name)
                                                     (string-downcase (symbol-name library-name))
                                                     library-name)
                                 ,@(when version
                                         (list :version `(read-version-from-string ,version)))
                                 ,@(when cld
                                         (list :cld `(parse-cld-address ',cld)))))
                                        ;else
               `(make-instance 'requirement
                               :library-name ,(if (symbolp dependency)
                                                  (string-downcase (symbol-name dependency))
						  dependency))))))

(defun parse-version-repository-address (address)
  (cond
    ((pathnamep address)
     `(make-instance 'directory-repository-address
                     :directory ,address))
    ((and (listp address)
          (equalp (first address) :directory))
     `(make-instance 'directory-repository-address
                     :directory ,(second address)))
    ((and (listp address)
          (equalp (first address) :git))
     (destructuring-bind (url &rest args)
         (rest address)
       `(make-instance 'git-repository-address
                       :url ,url
                       ,@args)))
    ((and (listp address)
          (equalp (first address)
                  :url))
     `(make-instance 'url-repository-address
                     :url ,(second address)))
    (t (error "Invalid repository ~A" address))))


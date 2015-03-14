(in-package :cldm.cli)

(defun create-cld-template-batch (project-name &key cld description author dependencies &allow-other-keys)
  `(cldm:deflibrary ,project-name
     :cld ,cld
     :description ,description
     :author ,author
     :dependencies ,dependencies))

(defun create-cld-template-interactive (project-name &key cld description author dependencies &allow-other-keys)
  (flet ((read-project-name ()
           (format t "Project name [~A]:" project-name)
           (let ((line (read-line)))
             (format t "~%")
             line))
         (read-description ()
           (format t "Description:")
           (let ((line (read-line)))
             (format t "~%")
             line))
         (read-cld ()
           (format t "CLD:")
           (let ((line (read-line)))
             (format t "~%")
             line))
         (read-author ()
           (format t "Author:")
           (let ((line (read-line)))
             (format t "~%")
             line))
         (read-dependencies ()
           (format t "Enter dependencies.~%")
           (let ((dependencies nil)
                 (continue t))
             (loop while continue
                do
                  (progn
                    (format t "Library:")
                    (let ((library (read-line)))
                      (format t "~%")
                      (if (not (equalp library ""))
                          (progn
                            (let ((version (progn (format t "Version:")
                                                  (read-line))))
                              (format t "~%")
                              (push (cons library version) dependencies)))
                                        ; else
                          (return)))))
             dependencies)))
    (let ((final-project-name (let ((read-project-name (read-project-name)))
                                (or
                                 (and (not (equalp read-project-name ""))
                                      read-project-name)
                                 project-name)))
          (final-description (or (not (equalp description ""))
                                 (read-description)))
          (final-cld (or (not (equalp cld ""))
                         (read-cld)))
          (final-author (or (not (equalp author ""))
                            (read-author)))
          (final-dependencies (or dependencies
                                  (read-dependencies))))
      (create-cld-template-batch final-project-name
                                 :cld final-cld
                                 :description final-description
                                 :author final-author
                                 :dependencies final-dependencies))))

(defun create-cld-template (project-name &rest keys &key cld description author dependencies interactive)
  (declare (ignorable cld description author dependencies))
  (if interactive
      (apply #'create-cld-template-interactive project-name keys)
      (apply #'create-cld-template-batch project-name keys)))

(defmethod process-command ((command (eql :init)))
  (let ((project-name (car (clon:remainder))))
    ;; Check that the project name was given
    (when (null project-name)
      (format t "Project name is missing.~%")
      (clon:exit 1))
    (let ((cld-filename (pathname (format nil "~A.cld" project-name))))
      (flet ((create-cld-file ()
               (let ((cld-template
                      (create-cld-template-interactive
                       project-name
                       :cld (clon:getopt :long-name "cld")
                       :author (clon:getopt :long-name "author")
                       :description (clon:getopt :long-name "description"))))
                 (format t "~A~%" cld-template)
                 (format t "Create? [yes]")
                 (let ((answer (read-line)))
                   (when (or (equalp answer "")
                             (not (equalp answer "no")))
                     (with-open-file (f cld-filename :direction :output
                                        :if-exists :supersede
                                        :if-does-not-exist :create)
                       (format f "~A" cld-template)))))))

        ;; If the cld file exists, error unless a force option was given
        (let ((cld-file (merge-pathnames cld-filename
                                         (osicat:current-directory))))
          (if (probe-file cld-file)
              (if (not (clon:getopt :long-name "force"))
                  (progn
                    (format t "The cld file already exist. Use the --force option to overwrite.~%")
                    (clon:exit 1))
                  (create-cld-file))
              (create-cld-file)))))))

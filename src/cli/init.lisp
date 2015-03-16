(in-package :cldm.cli)

(defmethod process-command ((command (eql :init)))
  (let ((name (car (clon:remainder))))
    (let ((cld-filename (pathname (format nil "~A.cld" name))))
      (flet ((create-cld-file ()
               (let ((cld-template
                      (cldm.cmd::create-cld-template-interactive
                       :name name
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

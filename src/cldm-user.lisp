(defpackage :cldm-user
  (:use :cl)
  (:shadow :cl :require)
  (:export :install
           :require
           :search
           :install-project
           :load-project
           :load-current-project
           :repo-list
           :init))

(in-package :cldm-user)

(defun install (library &rest args
                &key version
                  cld
                  (verbose cldm::*verbose-mode*)
                  (solving-mode cldm::*solving-mode*)
                  (libraries-directory cldm::*libraries-directory*)
                  (clear-registered-libraries t)
                  (interactive t))
  (declare (ignorable version cld verbose solving-mode libraries-directory
                      clear-registered-libraries interactive))
  (apply #'cldm:install-library (string-downcase (string library))
         args))

(defun require (library &rest args
                &key
                  version
                  (clean-asdf-environment cldm::*clean-asdf-environment*)
                  (libraries-directory cldm::*libraries-directory*))
  (declare (ignorable version libraries-directory clean-asdf-environment))
  (apply #'cldm::load-library (string-downcase (string library)) args))

(defun install-project (project &rest args
                        &key
                          version
                          libraries-directory
                          (verbose cldm::*verbose-mode*)
                          (solving-mode cldm::*solving-mode*)
                          (clear-registered-libraries t)
                          (interactive t))
  (declare (ignorable version libraries-directory
                      verbose solving-mode
                      clear-registered-libraries
                      interactive))
  (apply #'cldm::install-project project args))

(defun load-project (project &rest args
                     &key
                       libraries-directory
                       (clean-asdf-environment cldm::*clean-asdf-environment*))
  (declare (ignorable libraries-directory
                      clean-asdf-environment))
  (apply #'cldm::load-project project args))

(defun load-current-project (&rest args &key libraries-directory
                                          (clean-asdf-environment cldm::*clean-asdf-environment*))
  (declare (ignorable libraries-directory clean-asdf-environment))
  (apply #'load-project (osicat:current-directory) args))

(defun repo-list ()
  (cldm::list-cld-repositories))

#|

\section{Download sessions}

|#

#|
\ignore{

|#

(in-package :cldm)

#|
}

\subsection{Overview}

Download sessions are a way of tracking which urls failed when trying to download a cld from them. 

\subsection{Variables}

When \emph{*download-session-enabled-p*} is T, then a download session is started when downloading CLD files.
|#

(defvar *download-session-enabled-p* t)

#|
\emph{*download-session*} is the current download session:
|#

(defvar *download-session* nil)

#|
Failed URIs are tracked by adding them to \emph{*download-session-failed-uris*} list
|#

(defvar *download-session-failed-uris* nil)

#|
\subsection{with-download-session}

A download session is started with the \emph{with-download-session} macro.

|#

(defun call-with-download-session (function)
  (let ((*download-session* t)
        (*download-session-failed-uris* nil))
    (let ((repos (list-cld-repositories)))
      (loop for repo in repos
         do (start-download-session repo))
      (unwind-protect
           (funcall function)
        (loop for repo in repos
           do (stop-download-session repo))))))

(defmacro with-download-session ((&optional (enabled-p '*download-session-enabled-p*)) &body body)
  `(when ,enabled-p
     (call-with-download-session (lambda () ,@body))))

(defgeneric start-download-session (cld-repository)
  (:method ((cld-repository cld-repository))))

(defgeneric stop-download-session (cld-repository)
  (:method ((cld-repository cld-repository))))

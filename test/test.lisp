(in-package :cldm.test)

(in-root-suite)

(defsuite cldm-test)

(in-suite cldm-test)

(defun run-tests ()
  (without-debugging
    (cldm-test)))

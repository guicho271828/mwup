#|
  This file is a part of mwup project.
  Copyright (c) 2015 Masataro Asai (guicho2.71828@gmail.com)
|#

(in-package :cl-user)
(defpackage :mwup.test
  (:use :cl
        :mwup
        :fiveam
        :pddl.macro-action :pddl :alexandria :iterate
        :uiop/run-program)
  (:shadowing-import-from :iterate :minimize :maximize))
(in-package :mwup.test)



(def-suite :mwup)
(in-suite :mwup)

;; run test with (run! test-name) 

(test mwup
  
  )

(defun launch (&rest args)
  (run-program (list* "ros" (namestring (asdf:system-relative-pathname :mwup "mwup.ros")) args)
               :output t
               :error-output t))

(test ros
  (finishes
    (launch)
    (launch "--plain")
    (launch "--enhance-only")))


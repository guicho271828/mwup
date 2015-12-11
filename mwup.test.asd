#|
  This file is a part of mwup project.
  Copyright (c) 2015 Masataro Asai (guicho2.71828@gmail.com)
|#


(in-package :cl-user)
(defpackage mwup.test-asd
  (:use :cl :asdf))
(in-package :mwup.test-asd)


(defsystem mwup.test
  :author "Masataro Asai"
  :mailto "guicho2.71828@gmail.com"
  :description "Test system of mwup"
  :license "WTFPL"
  :depends-on (:mwup
               :fiveam)
  :components ((:module "t"
                :components
                ((:file "package"))))
  :perform (test-op :after (op c)
                    (uiop:run-program
                     (format nil "make -C ~a"
                             (asdf:system-source-directory :mwup))
                     :output t
                     :error-output t)
                    (uiop:run-program
                     (format nil "~a/t/cgroup-setup.sh"
                             (asdf:system-source-directory :mwup))
                     :output t
                     :error-output t
                     ;; for tests on CI
                     :ignore-error-status t)
                    (eval (read-from-string "(every #'fiveam::TEST-PASSED-P (5am:run! :mwup))"))))

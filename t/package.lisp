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

(defun launch-script (&rest args)
  (let* ((*default-pathname-defaults*
          (asdf:system-source-directory :mwup))
         (cmd (format nil
                      "cd ~a; cgexec -g cpu,cpuacct,memory:/~a ros ~a~{ ~s~}"
                      (namestring *default-pathname-defaults*)
                      (run-program "whoami" :output '(:string :stripped t))
                      (namestring (merge-pathnames "mwup.ros"))
                      (mapcar #'namestring (flatten args)))))
    (format t "~&~a~%" cmd)
    (run-program cmd
                 :output t
                 :error-output t
                 :force-shell t)))

(defun launch (&rest args)
  (let* ((*default-pathname-defaults*
          (asdf:system-source-directory :mwup))
         (cmd (format nil
                      "cd ~a; cgexec -g cpu,cpuacct,memory:/~a ~a ~{ ~s~}"
                      (namestring *default-pathname-defaults*)
                      (run-program "whoami" :output '(:string :stripped t))
                      (namestring (merge-pathnames "mwup"))
                      (mapcar #'namestring (flatten args)))))
    (format t "~&~a~%" cmd)
    (run-program cmd
                 :output t
                 :error-output t
                 :force-shell t)))

(test ros-dry-runs
  (finishes
    (launch-script))
  (finishes
    (launch))
  (finishes
    (launch "--plain"))
  (finishes
    (launch "--enhance-only"))
  (finishes
    (launch "-v" "--plain")))

(test plain
  (let ((*default-pathname-defaults*
         (asdf:system-source-directory :mwup)))
    (finishes
      (launch "-v" "--plain" "t/test1/p01.pddl"))
    (finishes
      (launch "-v" "--plain" "t/test2/p01.pddl" "t/test2/domain.pddl"))
    (finishes
      (launch "-v" "--plain" "t/test3/p01.pddl" "t/test3/domain.pddl"
              (directory (merge-pathnames "t/test3/p01.macro.*"))))))

(test macros
  (let ((*default-pathname-defaults*
         (asdf:system-source-directory :mwup)))
    (finishes
      (launch "-v" "--validation" "t/test1/p01.pddl"))
    (finishes
      (launch "-v" "--validation" "t/test2/p01.pddl" "t/test2/domain.pddl"))
    (finishes
      (launch "-v" "--validation" "t/test3/p01.pddl" "t/test3/domain.pddl"
              (directory (merge-pathnames "t/test3/p01.macro.*"))))
    (finishes
      (launch "-v" "--validation" "--add-macro-cost"
              "t/test3/p01.pddl" "t/test3/domain.pddl"
              (directory (merge-pathnames "t/test3/p01.macro.*"))))))

(test junk
  (let ((*default-pathname-defaults*
         (asdf:system-source-directory :mwup)))
    ;; does not work due to cgroup
    ;; (finishes
    ;;   (let (*verbose* *junk*)
    ;;     (mwup::main "-v" "--junk" "2" "10" "t/test2/p01.pddl" "t/test2/domain.pddl")))
    (finishes
      (launch "-v" "--junk" "2" "10" "t/test2/p01.pddl" "t/test2/domain.pddl"))
    (finishes
      (launch "-v" "--junk" "2" "0.5" "t/test2/p01.pddl" "t/test2/domain.pddl"))))

(test junk-limit
  (let ((*default-pathname-defaults*
         (asdf:system-source-directory :mwup)))
    ;; does not work due to cgroup
    ;; (finishes
    ;;   (let (*verbose* *junk*)
    ;;     (mwup::main "-v" "--junk" "2" "10" "t/test2/p01.pddl" "t/test2/domain.pddl")))
    (finishes
      (launch "-v" "--junk" "2" "10" "--junk-limit" "5" "t/test2/p01.pddl" "t/test2/domain.pddl"))))




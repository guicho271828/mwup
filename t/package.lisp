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
                 :output *standard-output*
                 :error-output *error-output*
                 )))

(defun launch-online (&rest args)
  (let ((*default-pathname-defaults* (asdf:system-source-directory :mwup)))
    (assert (apply #'mwup::mwup-run args))))

(test ros-dry-runs
  (finishes
    (launch-online))
  (finishes
    (launch-online "--plain"))
  (finishes
    (launch-online "--enhance-only"))
  (signals error
    (launch-online "--nosuchflag")))

(test plain
  (let ((*default-pathname-defaults*
         (asdf:system-source-directory :mwup)))
    (finishes
      (launch-online "--validation" "--plain" "t/test1/p01.pddl"))
    (finishes
      (launch-online "--validation" "--plain" "t/test2/p01.pddl" "t/test2/domain.pddl"))
    (finishes
      (launch-online "--validation" "--plain" "t/test3/p01.pddl" "t/test3/domain.pddl"
              (directory (merge-pathnames "t/test3/p01.macro.*"))))))

(test macros
  (let ((*default-pathname-defaults*
         (asdf:system-source-directory :mwup)))
    (finishes
      (launch-online "--validation" "t/test1/p01.pddl"))
    (finishes
      (launch-online "--validation" "t/test1/p01.pddl" "t/test1/domain.pddl"))
    (finishes
      (launch-online "--validation" "t/test1/p01.pddl" "t/test1/domain.pddl"
              (directory (merge-pathnames "t/test1/p01.macro.*"))))
    (finishes
      (launch-online "--validation" "--add-macro-cost"
              "t/test1/p01.pddl" "t/test1/domain.pddl"
              (directory (merge-pathnames "t/test1/p01.macro.*"))))))

(test junk
  (signals error
    ;; old arguments
    (launch-online "--junk" "2" "10" "--junk-limit" "5" "t/test3/p01.pddl" "t/test3/domain.pddl"))
  (signals error
    (launch-online "--junk" "2" "10" "--junk-limit" "5000" "t/test3/p01.pddl" "t/test3/domain.pddl"))
  ;; 
  ;; 
  (dolist (arg '("10" "0" "5000" ":infinity"))
    (finishes
      (launch-online "--validation" "--junk" "2" arg "t/test3/p01.pddl" "t/test3/domain.pddl")))
  (dolist (arg '("-1" "0.5" ":someother"))
    (signals error
      (launch-online "--validation" "--junk" "2" arg "t/test3/p01.pddl" "t/test3/domain.pddl"))))

(test gc
  (finishes
    (launch-online "--validation" "--junk" "2" "10" "--megabytes-consed-between-gcs" "10" "t/test3/p01.pddl" "t/test3/domain.pddl")))

(test fastjunk
  (dolist (arg '("10" "0" "5000" ":infinity"))
    (finishes
      (launch-online "--validation" "--fastjunk" "2" arg "t/test3/p01.pddl" "t/test3/domain.pddl")))
  (dolist (arg '("-1" "0.5" ":someother"))
    (signals error
      (launch-online "--validation" "--fastjunk" "2" arg "t/test3/p01.pddl" "t/test3/domain.pddl"))))

(test seed
  (finishes
    (launch-online "--validation" "--seed" "2016" "--junk" "2" "10" "t/test2/p01.pddl" "t/test2/domain.pddl"))
  (finishes
    (launch-online "--validation" "--seed" "t" "--junk" "2" "10" "t/test2/p01.pddl" "t/test2/domain.pddl")))

(test junk-type
  (dolist (arg '(":greedy" ":reservoir"))
    (finishes
      (launch-online "--junk" "2" "10" "--junk-type" arg "t/test2/p01.pddl" "t/test2/domain.pddl")))
  (dolist (arg '(":someother"))
    (signals error
      (launch-online "--junk" "2" "10" "--junk-type" arg "t/test2/p01.pddl" "t/test2/domain.pddl"))))

(test relative-greedy
  (dolist (arg '("10" "0" "0.00001" "5000"))
    (finishes
      (launch-online "--validation" "--junk-type" ":relative-greedy"
              "--junk" "2" arg "t/test3/p01.pddl" "t/test3/domain.pddl")))
  (dolist (arg '("-1" ":someother" ":infinity"))
    (signals error
      (launch-online "--validation" "--junk-type" ":relative-greedy"
              "--junk" "2" arg "t/test3/p01.pddl" "t/test3/domain.pddl"))))

(test init
  (dolist (arg '("10" "0" "5000" ":infinity"))
    (finishes
      (launch-online "--validation" "--junk" "2" arg "--junk-type" ":init" "t/test3/p01.pddl" "t/test3/domain.pddl")))
  (dolist (arg '("-1" "0.5" ":someother"))
    (signals error
      (launch-online "--validation" "--junk" "2" arg "--junk-type" ":init" "t/test3/p01.pddl" "t/test3/domain.pddl"))))

(test relative-init
  (dolist (arg '("10" "0" "0.00001" "5000"))
    (finishes
      (launch-online "--validation" "--junk-type" ":relative-init"
              "--junk" "2" arg "t/test3/p01.pddl" "t/test3/domain.pddl")))
  (dolist (arg '("-1" ":someother" ":infinity"))
    (signals error
      (launch-online "--validation" "--junk-type" ":relative-init"
              "--junk" "2" arg "t/test3/p01.pddl" "t/test3/domain.pddl"))))

(test mangle
  (finishes
   (launch-online "--validation" "--plain" "--mangle" "t/test3/p01.pddl" "t/test3/domain.pddl"))
  (finishes
   (launch-online "--validation" "--junk-type" ":relative-greedy" "--junk" "3" "5"
                  "--mangle" "t/test3/p01.pddl" "t/test3/domain.pddl")))



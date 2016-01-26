#|
  This file is a part of mwup project.
  Copyright (c) 2015 Masataro Asai (guicho2.71828@gmail.com)
|#

(in-package :cl-user)
(defpackage :mwup.test
  (:use :cl
        :mwup
        :1am :alexandria :iterate
        :uiop/run-program)
  (:shadowing-import-from :iterate :minimize :maximize))
(in-package :mwup.test)

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
  (let ((*default-pathname-defaults* (asdf:system-source-directory :mwup))
        (mwup::*enhance-only* nil)
        (mwup::*verbose* nil)
        (mwup::*validation* nil)
        (mwup::*plain* nil)
        (mwup::*lift* nil)
        (mwup::*seed* t)
        (mwup::*junk* nil)
        (mwup::*junk-type* :reservoir)
        (mwup::*iterated* nil)
        (mwup::*add-macro-cost* nil)
        (mwup::*remove-cost* nil)
        (mwup::*search* "fd-clean")
        (mwup::*options* "--search-options --if-unit-cost --heuristic hlm,hff=lm_ff_syn(lm_rhw(reasonable_orders=true)) --search lazy_greedy([hff,hlm],preferred=[hff,hlm]) --if-non-unit-cost --heuristic hlm1,hff1=lm_ff_syn(lm_rhw(reasonable_orders=true,lm_cost_type=one,cost_type=one)) --search lazy_greedy([hff1,hlm1],preferred=[hff1,hlm1],cost_type=one,reopen_closed=false) --always")
        (mwup::*mangle* nil))
    (apply #'mwup::mwup-run args)))

(test ros-dry-runs
  (is
    (launch-online))
  (is
    (launch-online "--plain"))
  (is
    (launch-online "--enhance-only"))
  (signals error
    (launch-online "--nosuchflag")))

(test plain
  (let ((*default-pathname-defaults*
         (asdf:system-source-directory :mwup)))
    (is
      (launch-online "--validation" "--plain" "t/test1/p01.pddl"))
    (is
      (launch-online "--validation" "--plain" "t/test2/p01.pddl" "t/test2/domain.pddl"))
    (is
      (apply #'launch-online "--validation" "--plain" "t/test1/p01.pddl" "t/test1/domain.pddl"
             (directory (merge-pathnames "t/test1/p01.macro.*"))))
    (is
      (apply #'launch-online "--validation" "--plain" "t/test3/p01.pddl" "t/test3/domain.pddl"
             (directory (merge-pathnames "t/test3/p01.macro.*"))))))

(test macros
  (let ((*default-pathname-defaults*
         (asdf:system-source-directory :mwup)))
    (is
      (launch-online "--validation" "t/test1/p01.pddl"))
    (is
      (launch-online "--validation" "t/test1/p01.pddl" "t/test1/domain.pddl"))
    (is
      (apply #'launch-online "--validation" "t/test1/p01.pddl" "t/test1/domain.pddl"
             (directory (merge-pathnames "t/test1/p01.macro.*"))))
    (is
      (apply #'launch-online "--validation" "--add-macro-cost"
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
    (is
      (launch-online "--validation" "--junk" "2" arg "t/test3/p01.pddl" "t/test3/domain.pddl")))
  (dolist (arg '("-1" "0.5" ":someother"))
    (signals error
      (launch-online "--validation" "--junk" "2" arg "t/test3/p01.pddl" "t/test3/domain.pddl"))))

(test gc
  (is
    (launch-online "--validation" "--junk" "2" "10" "--megabytes-consed-between-gcs" "10" "t/test3/p01.pddl" "t/test3/domain.pddl")))

(test fastjunk
  (dolist (arg '("10" "0" "5000" ":infinity"))
    (is
      (launch-online "--validation" "--fastjunk" "2" arg "t/test3/p01.pddl" "t/test3/domain.pddl")))
  (dolist (arg '("-1" "0.5" ":someother"))
    (signals error
      (launch-online "--validation" "--fastjunk" "2" arg "t/test3/p01.pddl" "t/test3/domain.pddl"))))

(test seed
  (is
    (launch-online "--validation" "--seed" "2016" "--junk" "2" "10" "t/test2/p01.pddl" "t/test2/domain.pddl"))
  (is
    (launch-online "--validation" "--seed" "t" "--junk" "2" "10" "t/test2/p01.pddl" "t/test2/domain.pddl")))

(test junk-type
  (dolist (arg '(":greedy" ":reservoir"))
    (is
      (launch-online "--junk" "2" "10" "--junk-type" arg "t/test2/p01.pddl" "t/test2/domain.pddl")))
  (dolist (arg '(":someother"))
    (signals error
      (launch-online "--junk" "2" "10" "--junk-type" arg "t/test2/p01.pddl" "t/test2/domain.pddl"))))

(test relative-greedy
  (dolist (arg '("10" "0" "0.00001" "5000"))
    (is
      (launch-online "--validation" "--junk-type" ":relative-greedy"
              "--junk" "2" arg "t/test3/p01.pddl" "t/test3/domain.pddl")))
  (dolist (arg '("-1" ":someother" ":infinity"))
    (signals error
      (launch-online "--validation" "--junk-type" ":relative-greedy"
              "--junk" "2" arg "t/test3/p01.pddl" "t/test3/domain.pddl"))))

(test init-macro
  (dolist (arg '("10" "0" "5000" ":infinity"))
    (is
      (launch-online "--validation" "--junk" "2" arg "--junk-type" ":init" "t/test3/p01.pddl" "t/test3/domain.pddl")))
  (dolist (arg '("-1" "0.5" ":someother"))
    (signals error
      (launch-online "--validation" "--junk" "2" arg "--junk-type" ":init" "t/test3/p01.pddl" "t/test3/domain.pddl"))))

(test relative-init
  (dolist (arg '("10" "0" "0.00001" "5000"))
    (is
     (launch-online "--validation" "--junk-type" ":relative-init"
              "--junk" "2" arg "t/test3/p01.pddl" "t/test3/domain.pddl")))
  (dolist (arg '("-1" ":someother" ":infinity"))
    (signals error
      (launch-online "--validation" "--junk-type" ":relative-init"
              "--junk" "2" arg "t/test3/p01.pddl" "t/test3/domain.pddl"))))

(test mangle
  (is
   (launch-online "--validation" "--plain" "--mangle" "t/test3/p01.pddl" "t/test3/domain.pddl"))
  (is
   (launch-online "-v" "--validation" "--junk" "2" ":infinity"
                  "--mangle" "t/test2/p01.pddl" "t/test2/domain.pddl")))

(test build
  (uiop:run-program
   (format nil "make -j -C ~a"
           (asdf:system-source-directory :mwup))
   :output t
   :error-output t)
  (launch)
  (launch "--validation" "--plain" "t/test1/p01.pddl")
  (launch "--validation" "t/test1/p01.pddl")
  (dolist (arg '("10" "0" "5000" ":infinity"))
    (launch "--validation" "--junk" "2" arg "t/test3/p01.pddl" "t/test3/domain.pddl")
    (launch "--validation" "--fastjunk" "2" arg "t/test3/p01.pddl" "t/test3/domain.pddl")
    (launch "--validation" "--junk" "2" arg "--junk-type" ":init"
                   "t/test3/p01.pddl" "t/test3/domain.pddl"))
  (launch "--validation" "--plain" "--mangle"
          "t/test3/p01.pddl" "t/test3/domain.pddl")
  (dolist (arg '("10" "0.1" "0"))
    (launch "--validation" "--junk" "2" arg "--junk-type" ":relative-greedy"
            "t/test3/p01.pddl" "t/test3/domain.pddl")
    (launch "--validation" "--junk" "2" arg "--junk-type" ":relative-init"
            "t/test3/p01.pddl" "t/test3/domain.pddl"))
  (dolist (arg '(":greedy" ":reservoir"))
    (launch "--junk" "2" "10" "--junk-type" arg "t/test2/p01.pddl" "t/test2/domain.pddl")))

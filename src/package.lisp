#|
  This file is a part of mwup project.
  Copyright (c) 2015 Masataro Asai (guicho2.71828@gmail.com)
|#

(in-package :cl-user)
(defpackage mwup
  (:use :cl :pddl.macro-action :pddl :alexandria :iterate))
(in-package :mwup)

;; blah blah blah.

(defvar *build-date*
    (multiple-value-bind (second minute hour date month year) (get-decoded-time)
      (format nil "~2,,,'0@a:~2,,,'0@a:~2,,,'0@a ~2,,,'0@a/~2,,,'0@a, ~a"
              hour minute second month date year)))

(defun consume-until-hyphen (list next)
  (labels ((rec (acc list)
             (ematch list
               ((list* "-" rest)
                (funcall next (format nil "~{~a~^ ~}" (nreverse acc)) rest))
               ((list* string rest)
                (rec (cons string acc) rest)))))
    (rec nil list)))

(defun main (args)
  (when *verbose* (print args))
  (let ((*package* (find-package :mwup)))
    (match args
      ;; debug options
      ((list* "-v" rest)
       (setf *verbose* t)
       (main rest))
      ((list* "--validation" rest)
       (setf *validation* t)
       (main rest))
      ((list* "--plain" rest)
       (format t "~&; Plain mode was activated, CAP runs only the main planner.")
       (setf *use-plain-planner* t)
       (main rest))
      ((list* "-t" time rest)
       (setf *hard-time-limit* (parse-integer time))
       (main rest))
      ((list* "-m" memory rest)
       (setf *memory-limit* (parse-integer memory))
       (main rest))
      ;; mwup?
      ((list* "--force-lifted" rest)
       (setf *ground-macros* nil)
       (main rest))
      ((list* "--iterated" rest)
       (setf *iterated* t)
       (main rest))
      ;; cost options
      ((list* "--add-macro-cost" rest)
       (setf *add-macro-cost* t)
       (main rest))
      ((list* "--remove-cost" rest)
       (setf *remove-cost* t)
       (main rest))
      ((list* "--search" planner rest)
       (setf *main-search* planner)
       (consume-until-hyphen
        rest
        (lambda (options rest)
          (setf *main-options* options)
          (main rest))))
      ((list* _ _)
       (format t "~%; Build date : ~a~%" *build-date*)
       (apply #'solve (mapcar #'merge-pathnames argv)))
      (nil
       (format *error-output* "~&Usage: component-planner PROBLEM [DOMAIN] [MACROPLANS...]~
               ~%~@{~4t~40<~(~a~)~;~{~a ~}~> : ~@(~a~)~%~}"
               '-----------------debug-options---------- nil "-------------------------------"
               '-v nil "Become more verbose"
               '--validation nil "run the validator after the planning"
               '--------------macro-options---------- nil "-------------------------------"
               '--plain nil "Do not add the macros."
               '--force-lifted nil "Lift the macro actions."
               '--split '(method) "Disables the macro-action grounding."
               '----------computational-resource-------- nil "-------------------------------"
               '-t '(sec) "time limit for the main search. NOT the total limit"
               '-m '(memory-in-kb) "memory limit for main search and subproblems. NOT the total limit"
               '--------underlying-planner-options------ nil "-------------------------------"
               '--search '(planner options... -) "Specify MainPlanner. Options end with a \"-\"."
               "" nil "Where PLANNER is one of: fd-clean,ff-clean,probe-clean,"
               "" nil "marvin1-clean,marvin2-clean,lpg-clean,mp-clean."
               '-------planner-compatibility-options---- nil "-------------------------------"
               '--add-macro-cost nil "Unit-cost domains are converted into action-cost domains. In those domains macro actions are then given a cost same as its length."
               '--remove-cost nil "Remove :action-costs from the resulting domain."
               '-------------shortcuts/aliases---------- nil "-------------------------------"
               )
       (format *error-output* "~%DOMAIN is by default domain.pddl, or <problemname>-domain.pddl in the same directory")
       (format *error-output* "~%MACROPLANS are by default <problemname>.macro.?[0-9]* in the same directory")
       (format *error-output* "~%Build date : ~a" *build-date*)
       (terpri *error-output*))
      (_
       (format *error-output* "~%Invalid Arguments!~%")
       (main nil)
       (error "~&Invalid Arguments!~2%")))))

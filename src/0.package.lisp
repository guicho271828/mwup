#|
  This file is a part of mwup project.
  Copyright (c) 2015 Masataro Asai (guicho2.71828@gmail.com)
|#

(in-package :cl-user)
(defpackage mwup
  (:use :cl :pddl.macro-action :pddl :alexandria :iterate :trivia
        :guicho-utilities)
  (:shadowing-import-from :pddl :minimize :maximize))
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

(defvar *enhance-only* nil "when non-nil, enhance the domain with macros but do not solve the problem")
(defvar *verbose* nil "")
(defvar *validation* nil "")
(defvar *plain* nil "")
(defvar *lift* nil "")
(defvar *junk* nil
  "A list of 2 integers. 1st element is the length of the junk macros.
2nd element specifies the ratio of macros retained from all macros of that length, in percentage.
If *junk* is NIL, no junk macros should be added.")
(defvar *junk-limit* nil
  "The maximum number of junk macros")
(defvar *iterated* nil "")
(defvar *add-macro-cost* nil "Add the action costs to the domain if it is a unit-cost domain.
Primitive actions are given a cost of 1. Macro actions are given a cost same as its length.
Ignored when *remove-main-problem-cost* is T.")

(defvar *remove-cost* nil "When non-nil, the problem and the domain solved
by the external planner could be modified so that it does not have
any :action-costs, so that any pure STRIPS-based planners can be
used. Supercedes *add-macro-cost*.")
(defvar *search* "fd-clean"
  "search command in planner-scripts.")
(defvar *options* "--search-options --if-unit-cost --heuristic hlm,hff=lm_ff_syn(lm_rhw(reasonable_orders=true)) --search lazy_greedy([hff,hlm],preferred=[hff,hlm]) --if-non-unit-cost --heuristic hlm1,hff1=lm_ff_syn(lm_rhw(reasonable_orders=true,lm_cost_type=one,cost_type=one)) --search lazy_greedy([hff1,hlm1],preferred=[hff1,hlm1],cost_type=one,reopen_closed=false) --always"
  "search options to pass to the underlying planner. default value is for
fd-clean and specifies those equivalent to LAMA2011.")

#+nil (defvar ** nil "")

(defun main (&rest args)
  (let ((*package* (find-package :mwup)))
    (parse args)))

(defun parse (args)
  "parse the options"
  (when *verbose* (print args))
    ;; special variables are set globally, in order to handle multithreaded environment correctly
    (match args
      ;; debug options
      ((list* "-v" rest)
       (setf *verbose* t)
       (parse rest))
      ((list* "--validation" rest)
       (setf *validation* t)
       (parse rest))
      ((list* "--enhance-only" rest)
       (setf *enhance-only* t)
       (parse rest))
      ((list* "--plain" rest)
       (format t "~&; Plain mode was activated, CAP runs only the main planner.")
       (setf *plain* t)
       (parse rest))
      ((list* "-t" time rest)
       (setf *hard-time-limit* (parse-integer time))
       (parse rest))
      ((list* "-m" memory rest)
       (setf *memory-limit* (parse-integer memory))
       (parse rest))
      ;; mwup?
      ((list* "--force-lifted" rest)
       (setf *lift* t)
       (parse rest))
      ((list* "--iterated" rest)
       (setf *iterated* t)
       (parse rest))
      ((list* "--junk" length percentage rest)
       (setf *junk* (list (read-from-string length)
                          (read-from-string percentage)))
       (parse rest))
      ((list* "--junk-limit" count rest)
       (setf *junk-limit* (read-from-string count))
       (parse rest))
      ;; cost options
      ((list* "--add-macro-cost" rest)
       (setf *add-macro-cost* t)
       (parse rest))
      ((list* "--remove-cost" rest)
       (setf *remove-cost* t)
       (parse rest))
      ((list* "--search" planner rest)
       (setf *search* planner)
       (consume-until-hyphen
        rest
        (lambda (options rest)
          (setf *options* options)
          (parse rest))))
      ((list* _ _)
       (format t "~%; Build date : ~a~%" *build-date*)
       (uiop:quit
        (if (apply #'solve (mapcar #'merge-pathnames args)) 0 2)))
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
       (format *error-output* "~%Base impl : ~a ~a" (lisp-implementation-type) (lisp-implementation-version))
       (terpri *error-output*))
      (_
       (format *error-output* "~%Invalid Arguments!~%")
       (parse nil))))

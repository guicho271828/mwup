#|
  This file is a part of mwup project.
  Copyright (c) 2015 Masataro Asai (guicho2.71828@gmail.com)
|#

(in-package :cl-user)
(defpackage mwup
  (:use :cl :pddl.macro-action :pddl :alexandria :iterate :trivia
        :guicho-utilities
        :priority-queue)
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

(defvar *enhance-only* nil "When non-nil, enhance the domain with macros but do not solve the problem")
(defvar *verbose* nil "Become more verbose")
(defvar *validation* nil "Run the validator after the planning")
(defvar *mode* :junk-macro "One of :PLAIN, :JUNK-MACRO, :EVAL-MACRO.")
(defvar *lift* nil "Lift the macro actions.")
(defvar *seed* t "Random seed, either an unsigned integer or a symbol T, which means a fresh random value.")
(defvar *junk* nil
  "A list of 2 integers. 1st element is the length of the junk macros.
2nd element specifies the number of junk macro.
If *junk* is NIL, no junk macros should be added.")
(defvar *macro-prefix* "JUNK-"
  "A string prefix for macro operators, which may affect the successor ordering of the underlying planner.")

;; (declaim (type (member :reservoir :greedy :relative-greedy :init) *junk-type*))
(defvar *junk-type* :reservoir
  "Specify the type of junk generation. one
  of :reservoir, :greedy, :relative-greedy. :greedy is equivalent to
  --fastjunk. :relative-greedy treats QUANTITY argument as a percentage,
  and compute the actual quantity relative to the number of
  primitive actions. Quantity could be :INFINITY.")
(defvar *iterated* nil "")

(defvar *transformers* (list 'junk-tag-wrapper 'filter-trivial-macros 'basic-enhance)
  "list of functions which take 3 arguments and return 3 values: domain, problem, macros (in this order).")

(defvar *search* "fd-clean"
  "search command in planner-scripts.")
(defvar *options* "--search-options --if-unit-cost --heuristic hlm,hff=lm_ff_syn(lm_rhw(reasonable_orders=true)) --search lazy_greedy([hff,hlm],preferred=[hff,hlm]) --if-non-unit-cost --heuristic hlm1,hff1=lm_ff_syn(lm_rhw(reasonable_orders=true,lm_cost_type=one,cost_type=one)) --search lazy_greedy([hff1,hlm1],preferred=[hff1,hlm1],cost_type=one,reopen_closed=false) --always"
  "search options to pass to the underlying planner. default value is for
fd-clean and specifies those equivalent to LAMA2011.")

(defvar *keep-tmp* nil
  "Do not cleanup the temporary directory where the enhanced domain definition is written.")

(define-condition invalid-arguments (simple-error) ())

(defun main (&rest args)
  (uiop:quit
   (if (handler-case (apply #'mwup-run args)
         (invalid-arguments ()
           (format *error-output* "~%Invalid Arguments!~%")
           (parse nil)                  ; returns t
           nil))
       0 2)))

(defun mwup-run (&rest args)
  "separated for testing purpose"
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
    ((list* "-d" rest)
     (setf *keep-tmp* t)
     (parse rest))
    ((list* "--validation" rest)
     (setf *validation* t)
     (parse rest))
    ((list* "--enhance-only" rest)
     (setf *enhance-only* t)
     (parse rest))
    ((list* "--macro-prefix" prefix rest)
     (setf *macro-prefix* prefix)
     (parse rest))
    ((list* "--plain" rest)
     (format t "~&; Plain mode was activated, CAP runs only the main planner.")
     (setf *mode* :plain)
     (parse rest))
    ((list* "--mode" mode rest)
     (setf *mode* (read-from-string mode))
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
    ((list* "--junk" length quantity rest)
     (setf *junk* (list (read-from-string length)
                        (read-from-string quantity)))
     (parse rest))
    ((list* "--fastjunk" rest)
     (setf *junk-type* :greedy)
     (format t "~& Fast junk generation activated, sacrificing uniformness")
     (parse (list* "--junk" rest)))
    ((list* "--junk-type" type rest)
     (setf *junk-type* (read-from-string type))
     (parse rest))
    ((list* "--seed" seed rest)
     (setf *seed* (read-from-string seed))
     (parse rest))
    ;; wrappers
    ((list* "--mangle" rest)
     (let ((*transformers* (cons #'mangle-wrapper *transformers*)))
       (parse rest)))
    ((list* "--add-macro-cost" rest)
     (let ((*transformers* (cons #'add-macro-cost *transformers*)))
       (parse rest)))
    ((list* "--remove-cost" rest)
     (let ((*transformers* (cons #'remove-cost *transformers*)))
       (parse rest)))
    ((list* "--search" planner rest)
     (setf *search* planner)
     (consume-until-hyphen
      rest
      (lambda (options rest)
        (setf *options* options)
        (parse rest))))
    ((list* "--megabytes-consed-between-gcs" bytes rest)
     (setf (sb-ext:bytes-consed-between-gcs)
           (* 1024 1024 (parse-integer bytes)))
     (parse rest))
    ((list* (string* #\-) _)
     (error 'invalid-arguments))
    ((list* _ _)
     (format t "~%; Build date : ~a~%" *build-date*)
     (apply #'solve-with-timer (mapcar #'merge-pathnames args)))
    (nil
     (format *error-output* "~&Usage: mwup PROBLEM [DOMAIN] [MACROPLANS...]~
               ~%~@{~4t~40<~(~a~)~;~{~a ~}~> : ~@(~a~)~%~}"
             '-----------------debug-options---------- nil "-------------------------------"
             '-v nil (documentation '*verbose* 'variable)
             '-d nil (documentation '*keep-tmp* 'variable)
             '--seed '(seed) (documentation '*seed* 'variable)
             '--megabytes-consed-between-gcs '(megabytes) "GC tuning"
             '--validation nil (documentation '*validation* 'variable)
             '--enhance-only nil (documentation '*enhance-only* 'variable)
             '--------------macro-options---------- nil "-------------------------------"
             '--mode '(mode) (documentation '*mode* 'variable)
             '--plain nil "Do not add the macros. Equivalent to --mode :plain"
             '--mangle nil (documentation '*mangle* 'variable)
             '--macro-prefix '(prefix) (documentation '*macro-prefix* 'variable)
             '--junk '(length quantity) (documentation '*junk* 'variable)
             '--fastjunk '(length quantity) "Same as --junk and --junk-type :greedy"
             '--junk-type '(type) (documentation '*junk-type* 'variable)
             '--force-lifted nil (documentation '*lift* 'variable)
             '----------computational-resource-------- nil "-------------------------------"
             '-t '(sec) "Time limit for the main search. NOT the total limit"
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
     (terpri *error-output*)
     t)
    (_
     (error 'invalid-arguments))))

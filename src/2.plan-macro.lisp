(in-package :mwup)

(defun plan-macro (dpath ppath macro-paths)
  (multiple-value-bind (dname domain) (suppress (parse-file dpath nil t))
    (multiple-value-bind (pname problem) (suppress (parse-file ppath nil t))
      (print dname) (print domain) (print pname) (print problem)
      (finalize-plans-macros
       dpath ppath
       (solve-problem-enhancing problem
                                (lambda (problem)
                                  (multiple-value-call
                                      #'cost-handling-wrapper
                                    (multiple-value-call
                                        #'filter-trivial-macros
                                      (enhance problem domain
                                               (macros-from-plans problem domain macro-paths)))))
                                :time-limit 1 ; satisficing
                                :name *search*
                                :options *options*
                                :verbose *verbose*
                                :iterated *iterated*)))))

(defun macros-from-plans (problem domain macro-paths)
  (iter (for path in macro-paths)
        (collect
            (ground-macro-action
             (actions
              (pddl-plan :path path :domain domain :problem problem))))))

(defun finalize-plans-macros (dpath ppath plans)
  (and plans
       (iter (for plan in plans)
             (for i from 1)
             (for plp =
                  (merge-pathnames
                   (format nil "~a.plan.~a"
                           (pathname-name ppath) i)))
             (when (probe-file plp) (delete-file plp))
             (write-plan plan plp *default-pathname-defaults* t)
             (when *validation*
               (always
                (validate-plan dpath ppath plp :verbose *verbose*))))))

(defun solve-problem-enhancing (problem method &rest test-problem-args)
  (format t "~&Enhancing the problem with macros.")
  (multiple-value-bind (eproblem edomain macros) (funcall method problem)
    (format t "~&Enhancement finished on:~%   ~a~%-> ~a" (name problem) (name eproblem))
    (format t "~&Solving the enhanced problem with the main planner ~a." *search*)
    (unless *enhance-only*
      (when (zerop (length macros)) (signal 'no-macro))
      (let* ((dir (mktemp "enhanced"))
             (plans (handler-bind ((unix-signal
                                    (lambda (c)
                                      (format t "~&main search terminated")
                                      (invoke-restart
                                       (find-restart 'pddl:finish c)))))
                      (apply #'test-problem-common
                             (write-pddl eproblem "eproblem.pddl" dir)
                             (write-pddl edomain "edomain.pddl" dir)
                             test-problem-args))))
        (format t "~&~a plans found, decoding the result plan." (length plans))
        (mapcar (lambda (plan i)
                  (terpri)
                  (block nil
                    (pprint-logical-block (*standard-output* nil :per-line-prefix (format nil "Plan ~a " i))
                      (return 
                        (decode-plan-all macros plan edomain eproblem)))))
                plans (iota (length plans)))))))

(defun decode-plan-all (macros plan edomain eproblem)
  (handler-bind ((warning #'muffle-warning)
                 (undefined-predicate
                  (lambda (c)
                    (when (and (not *lift*)
                               (eq 'equal (name c)))
                      (invoke-restart 'ignore)))))
    (reduce #'decode-plan macros
            ;;            ^^^^^^
            ;; based on edomain/eproblem: incompatible with edomain/eproblem
            :from-end t
            :initial-value (pddl-plan :path plan :domain edomain :problem eproblem))))




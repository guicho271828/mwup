(in-package :mwup)

(defmethod plan :around ((mode (eql :junk-macro)) dpath ppath)
  (handler-case
      (call-next-method)
    (no-macro ()
      (plan :plain dpath ppath))))

(defmethod plan ((mode (eql :junk-macro)) dpath ppath)
  (let ((*macro-paths* (or *macro-paths* (find-macros ppath))))
    (multiple-value-bind (dname domain) (suppress (parse-file dpath nil t))
      (multiple-value-bind (pname problem) (suppress (parse-file ppath nil t))
        (print dname) (print domain) (print pname) (print problem)
        (finalize-plans-macros
         dpath ppath
         (solve-problem-enhancing problem
                                  (lambda (problem)
                                    (multiple-value-call
                                        #'mangle-wrapper
                                      (multiple-value-call
                                          #'cost-handling-wrapper
                                        (multiple-value-call
                                            #'filter-trivial-macros
                                          (enhance problem domain
                                                   (append
                                                    (maybe-junk-macros problem domain)
                                                    (macros-from-plans problem domain *macro-paths*)))))))
                                  :time-limit 1 ; satisficing
                                  :name *search*
                                  :options *options*
                                  :verbose *verbose*
                                  :iterated *iterated*))))))

(defun macros-from-plans (problem domain macro-paths)
  (iter (for path in macro-paths)
        (handler-case
            (collect
                (nullary-macro-action
                 (actions
                  (pddl-plan :path path :domain domain :problem problem))))
          (zero-length-plan ()
            (tformat t "Skipping a zero-length plan ~a." path)))))

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
  (tformat t "Enhancing the problem with macros.")
  (multiple-value-bind (eproblem edomain macros) (funcall method problem)
    (tformat t "Enhancement finished on:~%   ~a~%-> ~a" (name problem) (name eproblem))
    (when *verbose* (room nil))
    (tformat t "Collecting Garbage...")
    (sb-ext:gc :full t)
    (tformat t "GC finished.")
    (when *verbose* (room nil))
    (tformat t "Solving the enhanced problem with the main planner ~a." *search*)
    (unless *enhance-only*
      (when (zerop (length macros)) (signal 'no-macro))
      (with-temp (dir "enhanced")
        (let* ((paths (handler-bind ((trivial-signal:unix-signal
                                      (lambda (c)
                                        (tformat t "main search terminated")
                                        (invoke-restart
                                         (find-restart 'pddl:finish c)))))
                        (apply #'test-problem-common
                               (write-pddl eproblem "eproblem.pddl" dir)
                               (write-pddl edomain "edomain.pddl" dir)
                               test-problem-args))))
          (tformat t "~a plans found." (length paths))
          (tformat t "~%decoding the result plan.")
          (let ((macros (map 'vector #'demangle macros)))
            (mapcar (lambda (plan i)
                      (terpri)
                      (block nil
                        (pprint-logical-block (*standard-output* nil :per-line-prefix (format nil "Plan ~a " i))
                          (return
                            (decode-plan-all macros plan)))))
                    (mapcar (lambda (path)
                              (pddl-plan :actions (map 'vector #'demangle
                                                       (actions (pddl-plan :path path
                                                                           :domain edomain
                                                                           :problem eproblem)))
                                         :domain (demangle edomain)
                                         :problem (demangle eproblem)))
                            paths)
                    (iota (length paths)))))))))

(defun decode-plan-all (macros plan)
  (handler-bind ((warning #'muffle-warning)
                 (undefined-predicate
                  (lambda (c)
                    (when (and (not *lift*)
                               (eq 'equal (name c)))
                      (invoke-restart 'ignore)))))
    #+nil
    (tformat t "~:@<~;Decoding plan ~a with macros ~a~:@>"
             (map 'vector #'name (actions plan))
             (map 'vector #'name macros))
    ;; ^^^^ too many output
    (reduce #'decode-plan macros :from-end t :initial-value plan)))





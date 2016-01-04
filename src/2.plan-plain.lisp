(in-package :mwup)

(defun just-copy-file (src dest)
  (ensure-directories-exist dest)
  (let ((command (format nil "cp -v ~a ~a" (namestring src) (namestring dest))))
    (format t "~&; ~a" command)
    (uiop:run-program command))
  (namestring dest))

(defun plan-plain (dpath ppath)
  (let ((dir (mktemp "plain")))
    (finalize-plans-plain
     dpath ppath
     (handler-bind ((trivial-signal:unix-signal
                     (lambda (c)
                       (tformat t "plain search terminated")
                       (invoke-restart
                        (find-restart 'pddl:finish c)))))
       (if *remove-cost*
           (multiple-value-bind (dname *domain*) (suppress (parse-file dpath nil t))
             (declare (ignorable dname))
             (multiple-value-bind (pname *problem*) (suppress (parse-file ppath nil t))
               (declare (ignorable pname))
               (test-problem-common
                (write-pddl (remove-costs *problem*) "problem.pddl" dir)
                (write-pddl (remove-costs *domain*) "domain.pddl" dir)
                :name *search*
                :options *options*
                :verbose *verbose*
                :iterated *iterated*)))
           (test-problem-common
            (just-copy-file ppath (format nil "~a/problem.pddl" dir))
            (just-copy-file dpath (format nil "~a/domain.pddl" dir))
            :name *search*
            :options *options*
            :verbose *verbose*
            :iterated *iterated*))))))

(defun finalize-plans-plain (dpath ppath plans)
  (and plans
       (iter (for path in plans)
             (for i from 1)
             (for new-path =
                  (merge-pathnames
                   (format nil "~a.plan.~a"
                           (pathname-name ppath) i)))
             (when (probe-file new-path) (delete-file new-path))
             (uiop:run-program
              (format nil "/bin/cp ~a ~a" (namestring path) (namestring new-path)))
             (when *validation*
               (always
                (validate-plan dpath ppath new-path :verbose *verbose*))))))

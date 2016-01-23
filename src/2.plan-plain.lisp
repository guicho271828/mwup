(in-package :mwup)

(defun just-copy-file (src dest)
  (ensure-directories-exist dest)
  (let ((command (format nil "cp -v ~a ~a" (namestring src) (namestring dest))))
    (format t "~&; ~a" command)
    (uiop:run-program command))
  (namestring dest))

(defun plan-plain (dpath ppath)
  (let ((dir (mktemp "plain")))
    (handler-bind ((trivial-signal:unix-signal
                    (lambda (c)
                      (tformat t "plain search terminated")
                      (invoke-restart
                       (find-restart 'pddl:finish c)))))
      (multiple-value-bind (dname domain) (suppress (parse-file dpath nil t))
        (declare (ignorable dname))
        (multiple-value-bind (pname problem) (suppress (parse-file ppath nil t))
          (declare (ignorable pname))
          (let ((domain (funcall (compose (if *remove-cost* #'remove-costs #'identity)
                                          (if *mangle* #'mangle #'identity))
                                 domain))
                (problem (funcall (compose (if *remove-cost* #'remove-costs #'identity)
                                           (if *mangle* #'mangle #'identity))
                                  problem)))
            (let ((paths (test-problem-common
                          (write-pddl problem "problem.pddl" dir)
                          (write-pddl domain "domain.pddl" dir)
                          :name *search*
                          :options *options*
                          :verbose *verbose*
                          :iterated *iterated*)))
              (finalize-plans-plain dpath ppath paths domain problem))))))))

(defun finalize-plans-plain (dpath ppath paths domain problem)
  (and paths
       (iter (for path in paths)
             (for i from 1)
             (for new-path =
                  (merge-pathnames
                   (format nil "~a.plan.~a"
                           (pathname-name ppath) i)))
             (when (probe-file new-path) (delete-file new-path))
             (write-plan (pddl-plan :actions (map 'vector #'demangle
                                                  (actions (pddl-plan :path path
                                                                      :domain domain
                                                                      :problem problem)))
                                    :domain (demangle domain)
                                    :problem (demangle problem))
                         new-path *default-pathname-defaults* t)
             (when *validation*
               (always
                (validate-plan dpath ppath new-path :verbose *verbose*))))))

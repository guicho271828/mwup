(in-package :mwup)

(defmacro with-temp ((var name) &body body)
  `(let ((,var (mktemp ,name)))
     (unwind-protect
         (progn ,@body)
       (uiop:run-program (format nil "rm -rf ~a" (namestring ,var))))))

(defmethod plan :around ((mode (eql :plain)) dpath ppath)
  (if (or *remove-cost* *mangle*)
      (call-next-method)
      (plan :plain-safe dpath ppath)))

(defmethod plan ((mode (eql :plain)) dpath ppath)
  (with-temp (dir "plain")
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
             (when (probe-file new-path)
               (tformat t "Deleting ~a" new-path)
               (delete-file new-path))
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

(defun just-copy-file (src dest)
  (ensure-directories-exist dest)
  (uiop:run-program
   (format nil "cp ~a ~a" (namestring src) (namestring dest)))
  (namestring dest))


(defmethod plan ((mode (eql :plain-safe)) dpath ppath)
  (format t "~&Safe Plain mode. Plans are not parsed, and just copied to the tmp directory without processing.")
  (with-temp (dir "plain")
    (let ((plans
           (handler-bind ((trivial-signal:unix-signal
                           (lambda (c)
                             (format t "~&main search terminated")
                             (invoke-restart
                              (find-restart 'pddl:finish c)))))
             (test-problem-common
              (just-copy-file ppath (format nil "~a/problem.pddl" dir))
              (just-copy-file dpath (format nil "~a/domain.pddl" dir))
              :name *search*
              :options *options*
              :verbose *verbose*
              :iterated *iterated*))))
      (finalize-plans-plain-safe dpath ppath plans))))

(defun finalize-plans-plain-safe (dpath ppath paths)
  (and paths
       (iter (for path in paths)
             (for i from 1)
             (for new-path =
                  (merge-pathnames
                   (format nil "~a.plan.~a"
                           (pathname-name ppath) i)))
             (when (probe-file new-path) (delete-file new-path))
             (uiop:run-program
              (list "/bin/cp" (namestring path) (namestring new-path)))
             (when *validation*
               (always
                (validate-plan dpath ppath new-path :verbose *verbose*))))))


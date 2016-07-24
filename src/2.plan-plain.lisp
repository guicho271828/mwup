(in-package :mwup)

(defmacro with-temp ((var name) &body body)
  `(let ((,var (mktemp ,name)))
     (unwind-protect
         (progn ,@body)
       (unless *keep-tmp*
         (uiop:run-program (format nil "rm -rf ~a" (namestring ,var)))))))

(defmethod solve ((mode (eql :plain)) dpath ppath)
  (multiple-value-bind (dname domain)
      (suppress
        (handler-case (parse-file dpath nil t)
          (error ()
            (tformat t "~%Failed to parse ~A in plain mode! Falling back to safe-plain"
                     dpath)
            (return-from solve
              (solve :plain-safe dpath ppath)))))
    (multiple-value-bind (pname problem)
        (suppress
          (handler-case (parse-file ppath nil t)
            (error ()
              (tformat t "~%Failed to parse ~a in plain mode! Falling back to safe-plain"
                       ppath)
              (return-from solve
                (solve :plain-safe dpath ppath)))))
      (print dname) (print domain) (print pname) (print problem)
      (finalize-plans-macros
       dpath ppath
       (solve-problem-enhancing (lambda ()
                                  (funcall (apply #'multiple-value-compose *transformers*)
                                           problem domain nil))
                                :time-limit 1 ; satisficing
                                :name *search*
                                :options *options*
                                :verbose *verbose*
                                :iterated *iterated*)))))

(defun just-copy-file (src dest)
  (ensure-directories-exist dest)
  (uiop:run-program
   (format nil "cp ~a ~a" (namestring src) (namestring dest)))
  (namestring dest))

(defmethod solve ((mode (eql :plain-safe)) dpath ppath)
  (tformat t "~&Safe Plain mode. Plans are not parsed, and just copied to the tmp directory without processing.")
  (with-temp (dir "plain")
    (let ((plans
           (handler-bind ((trivial-signal:unix-signal
                           (lambda (c)
                             (tformat t "~&main search terminated")
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


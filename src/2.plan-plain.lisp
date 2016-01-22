(in-package :mwup)

(defun just-copy-file (src dest)
  (ensure-directories-exist dest)
  (let ((command (format nil "cp -v ~a ~a" (namestring src) (namestring dest))))
    (format t "~&; ~a" command)
    (uiop:run-program command))
  (namestring dest))

(defgeneric mangle (object))
(defgeneric demangle (object))

(defvar *demangle* (make-hash-table))

(defmethod mangle :around (object)
  (let ((result (call-next-method)))
    (setf (gethash result *demangle*) object)
    result))

(defmethod mangle ((domain pddl-domain))
  (let ((*domain* (shallow-copy domain)))
    (setf (actions *domain*)
          (mapcar #'mangle (shuffle (actions *domain*))))
    *domain*))

(defmethod mangle ((a pddl-action))
  (shallow-copy a
                :name (gensym "MANGLED")
                :domain *domain*))

(defmethod demangle (object)
  (gethash object *demangle* object))

(defmethod demangle ((ga pddl-ground-action))
  (shallow-copy ga
                :name (name (demangle (action (domain ga) (name ga))))
                :domain (demangle (domain ga))))

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
                (problem (funcall (if *remove-cost* #'remove-costs #'identity) problem)))
            (let ((plans (test-problem-common
                          (write-pddl problem "problem.pddl" dir)
                          (write-pddl domain "domain.pddl" dir)
                          :name *search*
                          :options *options*
                          :verbose *verbose*
                          :iterated *iterated*)))
              (finalize-plans-plain dpath ppath plans domain problem))))))))

(defun finalize-plans-plain (dpath ppath plans domain problem)
  (and plans
       (iter (for path in plans)
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

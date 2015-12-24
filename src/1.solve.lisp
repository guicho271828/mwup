
(in-package :mwup)

(defmacro suppress (&body body)
  `(handler-bind ((warning #'muffle-warning))
     ,@body))

(defun find-domain (problem-path)
  (format t "~&finding the domain file...")
  (print
   (block nil
     (let ((dpath (make-pathname :defaults problem-path :name "domain")))
       (when (probe-file dpath) (return dpath)))
     (let ((dpath (make-pathname :defaults problem-path :name
                                 (format nil "~a-domain" (pathname-name problem-path)))))
       (when (probe-file dpath) (return dpath)))
     (error "~& Failed to infer the domain pathname from problem pathname!~%Problem: ~a~%Candidate: ~a~%Candidate: ~a"
            problem-path
            (make-pathname :defaults problem-path :name "domain")
            (make-pathname :defaults problem-path :name (format nil "~a-domain" (pathname-name problem-path)))))))


(defun find-macros (problem-path)
  (format t "~&finding macros...")
  (ematch problem-path
    ((pathname name)
     (let ((wild (merge-pathnames (format nil "~a.macro*.*" name)
                                  problem-path)))
       (print wild)
       (print (directory wild))))))

(define-condition no-macro () ())

(defvar *start*)

(defun solve (ppath &optional (dpath (find-domain ppath)) &rest macro-paths)
  (setf *start* (get-universal-time))
  (let ((macro-paths (or macro-paths
                         (find-macros ppath))))
    (unwind-protect
        (if *plain*
            (plan-plain dpath ppath)
            (handler-case
                (plan-macro dpath ppath macro-paths)
              (no-macro ()
                (plan-plain dpath ppath))))
      (format t "~&Wall time: ~a sec~%"
              (- (get-universal-time) *start*)))))



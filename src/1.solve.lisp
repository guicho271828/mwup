
(in-package :mwup)

(defmacro suppress (&body body)
  `(handler-bind ((warning #'muffle-warning))
     ,@body))

(defun find-domain (problem-path)
  (let ((dpath (make-pathname :defaults problem-path :name "domain")))
    (when (probe-file dpath) (return-from find-domain dpath)))
  (let ((dpath (make-pathname :defaults problem-path :name
                              (format nil "~a-domain" (pathname-name problem-path)))))
    (when (probe-file dpath) (return-from find-domain dpath)))
  (error "~& Failed to infer the domain pathname from problem pathname!~%Problem: ~a~%Candidate: ~a~%Candidate: ~a"
         problem-path
         (make-pathname :defaults problem-path :name "domain")
         (make-pathname :defaults problem-path :name (format nil "~a-domain" (pathname-name problem-path)))))

(define-condition no-macro () ())

(defvar *start*)

(defun solve (ppath &optional (dpath (find-domain ppath)) &rest macro-paths)
  (setf *start* (get-universal-time))
  (unwind-protect
          (if (or *plain* (null macro-paths))
              (plan-plain dpath ppath)
              (handler-case
                  (plan-macro dpath ppath macro-paths)
                (no-macro ()
                  (plan-plain dpath ppath))))
        (format t "~&Wall time: ~a sec~%"
                (- (get-universal-time) *start*))))



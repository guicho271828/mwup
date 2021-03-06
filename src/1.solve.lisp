
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
  (tformat t "finding macros...")
  (let ((wild (merge-pathnames (format nil "~a.macro*.*" (pathname-name problem-path))
                               problem-path)))
    (tformat t "~a" wild)
    (tformat t "~a" (directory wild))
    (directory wild)))

(defvar *start*)

(defun elapsed ()
  (- (get-universal-time) *start*))

(defun tformat (stream format-string &rest args)
  (fresh-line stream)
  (pprint-logical-block (stream nil :per-line-prefix (format nil "~9@<(t=~d)~>" (elapsed)))
    (apply #'format stream format-string args)))

(defvar *macro-paths* nil)

(defun solve-with-timer (ppath &optional (dpath (find-domain ppath)) &rest *macro-paths*)
  (setf *start* (get-universal-time))
  (setf *random-state* (sb-ext:seed-random-state *seed*))
  (unwind-protect
      (solve *mode* dpath ppath)
    (format t "~&Wall time: ~a sec~%" (elapsed))))

(defgeneric solve (mode dpath ppath))

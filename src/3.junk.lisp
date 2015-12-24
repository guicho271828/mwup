;;; setup

(in-package :mwup)

;;; util

(defun map-product-if (function list &rest more-lists)
  "from alexandria:map-product, but do not collect element if nil is returned"
  (labels ((%map-product (f lists)
             (let ((more (cdr lists))
                   (one (car lists)))
               (if (not more)
                   (loop for arg in one
                         for res = (funcall f arg)
                         when res
                           collect res)
                   (mappend (lambda (x)
                              (%map-product (curry f x) more))
                            one)))))
    (%map-product (ensure-function function) (cons list more-lists))))

;; (map-product (lambda (&rest args) (when (every #'oddp args) args))  '(1 2 3) '(4 5 6))
;; ; (NIL (1 5) NIL NIL NIL NIL NIL (3 5) NIL)
;; (map-product-if (lambda (&rest args) (when (every #'oddp args) args))  '(1 2 3) '(4 5 6))
;; ; ((1 5) (3 5))

;;; main

(defun maybe-junk-macros (problem domain)
  (ematch *junk*
    (nil nil)
    ((list length percentage)
     (format t "~&Adding junk macros of length ~a, with ~a% probability" length percentage)
     (check-type length (integer 2))
     (check-type percentage (integer 0 100))
     (junk-macros length
                  (/ percentage 100)
                  (objects problem)
                  (actions domain)))))

(defun junk-macros (length probability objects actions)
  (labels ((rec (length macro)
             (if (zerop length)
                 (when (< probability (random 1.0))
                   macro)
                 (mappend
                  (lambda (a)
                    (collect-for-all-possible-parameters
                     a objects
                     (lambda (params)
                       (let ((ga (ground-action a params)))
                         (unless (conflict macro ga)
                           (rec (1- length)
                                (merge-ground-actions macro ga)))))))
                  actions))))
    (mappend
     (lambda (a)
       (collect-for-all-possible-parameters
        a objects
        (lambda (params)
          (rec (1- length)
               (ground-action a params)))))
     actions)))

(defun collect-for-all-possible-parameters (a objects fn)
  (apply #'map-product-if fn
         (all-possible-parameters a objects)))

(function-cache:defcached all-possible-parameters (a objects)
  (iter (for param in (parameters a))
        (collect
            (remove-if-not (rcurry #'pddl-typep (type param)) objects))))


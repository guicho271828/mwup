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
     (handler-bind ((warning #'muffle-warning))
       (junk-macros length
                    (/ percentage 100)
                    (get-all-ground-actions domain problem)
                    domain problem)))))

(defun junk-macros (length probability actions *domain* *problem*)
  (let (acc (total 0) (added 0))
    (format t "~&Number of instantiated ground macros: ~a" (length actions))
    (labels ((rec (length macro list)
               (if (zerop length)
                   (progn
                     (incf total)
                     (when (< (random 1.0) probability)
                       (push list acc)
                       (incf added)))
                   (map nil
                        (lambda (a)
                          (unless (conflict macro a)
                            (rec (1- length)
                                 (merge-ground-actions macro a)
                                 (cons a list))))
                        actions))))
      (map nil
           (lambda (a)
             (rec (1- length) a (list a)))
           actions))
    (format t "~&Total possible junk macros: ~a~%Added macros: ~a" total added)
    (map 'list
         (lambda (actions)
           (nullary-macro-action (coerce actions 'vector)))
         acc)))

(defun get-all-ground-actions (domain problem)
  (let* ((dir (mktemp "dump"))
         (pp (write-pddl problem "problem.pddl" dir))
         (dp (write-pddl domain "domain.pddl" dir))
         (plans (test-problem-common pp dp
                                     :verbose nil ;; *verbose* ;; only during debugging --- confuse log parser
                                     :name "dump-action-clean"
                                     :time-limit 1 ; satisficing
                                     :memory 2000000
                                     :hard-time-limit 3600
                                     :options "--search-options --search eager(tiebreaking([g()]))")))
    (assert (= 1 (length plans)))
    (actions
     (pddl-plan :domain domain :problem problem
                :path (first plans)))))



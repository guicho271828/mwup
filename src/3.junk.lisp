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
    ((list length quantity)
     (tformat t "Adding junk macros: length: ~a quantity: ~A" length quantity)
     (check-type length (integer 2))
     (check-type quantity (integer 0))
     (handler-bind ((warning #'muffle-warning))
       (junk-macros length
                    quantity
                    (get-all-ground-actions domain problem)
                    domain problem)))))

;; index begins from 1
;; (loop for i from 1 to k
;;       do
;;    (setf (aref r i) (aref s i)))
;; (loop for i from (1+ k) to n
;;       for j = (1+ (random i))
;;       do
;;    (if (<= j k)
;;        (setf (aref r i) (aref s i))))

;; 0-index
;; (loop for i below k
;;       do
;;    (setf (aref r i) (aref s i)))
;; (loop for i from k below n
;;       for j = (random i)
;;       do
;;    (if (<= j k)
;;        (setf (aref r i) (aref s i))))

(defun junk-macros (length quantity actions *domain* *problem*)
  "Use Reservoir Sampling (Algorithm R by Jeffrey Vitter) https://en.wikipedia.org/wiki/Reservoir_sampling
quantity = k
count    = i
"
  (let ((count 0)
        (reservoir (make-array quantity)))
    (tformat t "Number of instantiated ground actions: ~a" (length actions))
    (tformat t "Generating macro actions using Reservoir Sampling")
    (labels ((rec (length macro list)
               (if (zerop length)
                   (progn
                     (if (< count quantity)
                         (setf (aref reservoir count) list)
                         (let ((j (random count)))
                           (when (< j quantity)
                             (setf (aref reservoir j) list))))
                     (incf count))
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
    (tformat t "Total possible junk macros: ~a" count)
    (map 'list
         (lambda (actions)
           (nullary-macro-action (coerce actions 'vector)))
         reservoir)))

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



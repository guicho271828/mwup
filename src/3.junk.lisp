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

(define-condition minimum-requirement () ())

(defun append-junk-tag (macro)
  (ematch macro
    ((pddl-action :name (place name))
     (setf name (symbolicate *macro-prefix* name))
     macro)))

(defun maybe-junk-macros (problem domain)
  (mapcar #'append-junk-tag
          (handler-bind ((warning #'muffle-warning))
            (let ((actions (get-all-ground-actions domain problem)))
              (ematch *junk*
                (nil nil)
                ((list length :infinity)
                 (check-type length (integer 2))
                 (ecase *junk-type*
                   ((:reservoir :greedy)
                    (tformat t "Adding all junk macros: length: ~a quantity: ~A" length :infinity)
                    (all-macros length actions domain problem))
                   (:init
                    (tformat t "Adding all init macros: length: ~a quantity: ~A" length :infinity)
                    (all-init-macros length MOST-POSITIVE-FIXNUM actions domain problem))))
                ((list length (and (number) param))
                 (check-type length (integer 2))
                 (check-type param (real 0))
                 (ecase *junk-type*
                   (:reservoir
                    (check-type param (integer 0))
                    (tformat t "Adding junk macros: length: ~a quantity: ~A" length param)
                    (junk-macros-reservoir length param actions domain problem))
                   (:greedy
                    (check-type param (integer 0))
                    (tformat t "Adding junk macros: length: ~a quantity: ~A" length param)
                    (handler-case
                        (progn
                          (tformat t "Try standard Reservoir Sampling method to get the minimum required number of macros")
                          ;; for cases where there are too few ground macros
                          (junk-macros-reservoir length param actions domain problem))
                      (minimum-requirement ()
                        (tformat t "There are required number of macros, switching to the Naive Sampling")
                        (junk-macros-greedy length param actions domain problem))))
                   (:relative-greedy
                    (let* ((prim-len (length actions))
                           (quantity (ceiling (* prim-len param 1/100))))
                      (tformat t "Adding junk macros: length: ~a quantity: ~A -- ~a% relative to ~A" length quantity param prim-len)
                      (handler-case
                          (progn
                            (tformat t "Try standard Reservoir Sampling method to get the minimum required number of macros")
                            ;; for cases where there are too few ground macros
                            (junk-macros-reservoir length quantity actions domain problem))
                        (minimum-requirement ()
                          (tformat t "There are required number of macros, switching to the Naive Sampling")
                          (junk-macros-greedy length quantity actions domain problem)))))
                   (:init
                    (check-type param (integer 0))
                    ;; generate from the initial state
                    (tformat t "Adding init macros: length: ~a param: ~A" length param)
                    (handler-case
                        (progn
                          (tformat t "Try instantiating all macros to get the minimum required number of macros")
                          ;; for cases where there are too few ground macros
                          (all-init-macros length param actions domain problem))
                      (minimum-requirement ()
                        (init-macros length param actions domain problem))))
                   (:relative-init
                    (let* ((prim-len (length actions))
                           (quantity (ceiling (* prim-len param 1/100))))
                      (tformat t "Adding init macros relative to the number of primitive actions: length: ~a, percentage: ~a, quantity: ~a"
                               length param quantity)
                      (handler-case
                          (progn
                            (tformat t "Try instantiating all macros to get the minimum required number of macros")
                            ;; for cases where there are too few ground macros
                            (all-init-macros length quantity actions domain problem))
                        (minimum-requirement ()
                          (init-macros length quantity actions domain problem))))))))))))

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

(defun junk-macros-reservoir (length quantity actions *domain* *problem*)
  "Use Reservoir Sampling (Algorithm R by Jeffrey Vitter) https://en.wikipedia.org/wiki/Reservoir_sampling
quantity = k
count    = i
"
  (let ((count 0)
        (reservoir (make-array quantity)))
    (tformat t "Number of instantiated ground actions: ~a" (length actions))
    (tformat t "Generating macro actions using Reservoir Sampling")
    (when (plusp quantity)
      (labels ((rec (length macro list)
                 (if (zerop length)
                     (progn
                       (if (< count quantity)
                           (setf (aref reservoir count) list)
                           (let ((j (random count)))
                             (signal 'minimum-requirement)
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
      (iter (for actions in-vector reservoir)
            (when (listp actions)
              (collect
                  (nullary-macro-action (nreverse (coerce actions 'vector)))))))))

#+nil
(defun junk-macros-reservoir-opt (length quantity actions *domain* *problem*)
  "Use Reservoir Sampling (Algorithm R by Jeffrey Vitter) https://en.wikipedia.org/wiki/Reservoir_sampling
quantity = k
count    = i
optimized using type information, but not that effective since the inner functions are slow
"
  (declare (optimize (speed 3) (debug 0) (safety 0))
           ((SIMPLE-ARRAY standard-object (*)) actions)
           (fixnum length quantity))
  (let ((count 0)
        (reservoir (make-array quantity)))
    (declare (fixnum count))
    (tformat t "Number of instantiated ground actions: ~a" (length actions))
    (tformat t "Generating macro actions using Reservoir Sampling")
    (labels ((rec (length macro list)
               (declare (fixnum length quantity))
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
    (iter (for actions in-vector reservoir)
          (when (listp actions)
           (collect
               (nullary-macro-action (nreverse (coerce actions 'vector))))))))

(defun junk-macros-greedy (length quantity actions *domain* *problem*)
  "Faster alternative which randomly selects from the next applicable action.
This sacrifices the uniformness of the sampling because the branches with
less siblings have high probability of being selected."
  (when (plusp quantity)
    (let ((hash (make-hash-table :test #'equal)))
      (tformat t "Number of instantiated ground actions: ~a" (length actions))
      (tformat t "Generating macro actions using Naive Sampling")
      (labels ((rec (length macro list)
                 (if (zerop length)
                     list
                     ;;#+slow
                     (let* ((len (length actions)))
                       (iter (until (zerop len))
                             (for i = (random len))
                             (for a = (aref actions i))
                             (if (conflict macro a)
                                 (progn
                                   (decf len)
                                   (rotatef (aref actions i) (aref actions len)))
                                 (return
                                   (rec (1- length)
                                        (merge-ground-actions macro a)
                                        (cons a list))))))
                     #+slow
                     (let* ((len (length actions))
                            (flags (make-array len :element-type 'bit :initial-element 0)))
                       (iter (generate count from 1 below len)
                             (for i = (random len))
                             (if (zerop (aref flags i))
                                 (setf (aref flags i) 1)
                                 (next-iteration))
                             (next count)
                             ;; (format t "~4<~a~> ~4<~a~> ~a~%" count len flags)
                             ;; (sleep 0.2)
                             (for a = (aref actions i))
                             (unless (conflict macro a)
                               (return
                                 (rec (1- length)
                                      (merge-ground-actions macro a)
                                      (cons a list))))))
                     #+slow
                     (let ((candidates (remove-if (curry #'conflict macro) actions)))
                       (unless (zerop (length candidates))
                         (let ((a (random-elt candidates)))
                           (rec (1- length)
                                (merge-ground-actions macro a)
                                (cons a list))))))))
        (iter (generate count from 1 below quantity)
              (for a = (random-elt actions))
              (for path = (rec (1- length) a (list a)))
              (when path
                (unless (gethash path hash)
                  (setf (gethash path hash) (nullary-macro-action (nreverse (coerce path 'vector))))
                  (next count)))
              (finally
               (return
                 (iter (for (path macro) in-hashtable hash)
                       (collect macro)))))))))

(defun all-macros (length actions *domain* *problem*)
  "Generate all macro actions of length LENGTH"
  (let ((count 0) acc)
    (tformat t "Number of instantiated ground actions: ~a" (length actions))
    (tformat t "Generating all macro actions")
    (labels ((rec (length macro list)
               (if (zerop length)
                   (progn
                     (push (nullary-macro-action (nreverse (coerce list 'vector))) acc)
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
    acc))

(defun get-all-ground-actions (domain problem)
  (with-temp (dir "dump")
    (let* ((pp (write-pddl problem "problem.pddl" dir))
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
                  :path (first plans))))))


(defun init-macros (length quantity actions *domain* *problem*)
  ""
  (let ((hash (make-hash-table :test #'equal)))
    (tformat t "Number of instantiated ground actions: ~a" (length actions))
    (tformat t "Sampling macro actions applicable to the initial state")
    (labels ((rec (length state list)
               (if (zerop length)
                   list
                   (let* ((len (length actions))
                          (flags (make-array len :element-type 'bit :initial-element 0)))
                     (iter (generate count below len)
                           (for i = (random len))
                           (if (zerop (aref flags i))
                               (setf (aref flags i) 1)
                               (next-iteration))
                           (next count)
                           (for a = (aref actions i))
                           (when (applicable state a)
                             (return
                               (rec (1- length)
                                    (apply-ground-action a state)
                                    (cons a list))))))
                   #+slow
                   (let ((candidates (remove-if-not (curry #'applicable state) actions)))
                     (unless (zerop (length candidates))
                       (let ((a (random-elt candidates)))
                         (rec (1- length)
                              (apply-ground-action a state)
                              (cons a list))))))))
      (iter (generate count from 1 below quantity)
            (for path = (rec length (init *problem*) nil))
            (when path
              (unless (gethash path hash)
                (setf (gethash path hash) (nullary-macro-action (nreverse (coerce path 'vector))))
                (next count)))
            (finally
             (return
               (iter (for (path macro) in-hashtable hash)
                     (collect macro))))))))

(defun all-init-macros (length quantity actions *domain* *problem*)
  ""
  (let ((acc nil) (count 0))
    (tformat t "Number of instantiated ground actions: ~a" (length actions))
    (tformat t "Generating ALL macro actions applicable to the initial state")
    (labels ((rec (length state list)
               (if (zerop length)
                   (progn
                     (push (nullary-macro-action (nreverse (coerce list 'vector))) acc)
                     (incf count)
                     (when (< quantity count)
                       (signal 'minimum-requirement)))
                   (let ((candidates (remove-if-not (curry #'applicable state) actions)))
                     (map nil (lambda (a)
                                (rec (1- length)
                                     (apply-ground-action a state)
                                     (cons a list)))
                          candidates)))))
      (rec length (init *problem*) nil))
    acc))


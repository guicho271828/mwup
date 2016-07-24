(in-package :mwup)

(defmethod solve :around ((mode (eql :eval-macro)) dpath ppath)
  (handler-case
      (call-next-method)
    (no-macro ()
      (solve :plain dpath ppath))))

(defmethod solve ((mode (eql :eval-macro)) dpath ppath)
  (multiple-value-bind (dname domain) (suppress (parse-file dpath nil t))
    (multiple-value-bind (pname problem) (suppress (parse-file ppath nil t))
      (print dname) (print domain) (print pname) (print problem)
      (finalize-plans-macros
       dpath ppath
       (solve-problem-enhancing (lambda ()
                                  (funcall (apply #'multiple-value-compose
                                                  (append *transformers*
                                                          (list #'filter-trivial-macros
                                                                #'basic-enhance)))
                                           problem domain
                                           (eval-macros problem domain)))
                                :time-limit 1 ; satisficing
                                :name *search*
                                :options *options*
                                :verbose *verbose*
                                :iterated *iterated*)))))

(defun eval-macros (problem domain)
  (handler-bind ((warning #'muffle-warning))
    (let ((actions (get-all-ground-actions domain problem)))
      (tformat t "Number of instantiated ground actions: ~a" (length actions))
      (ematch *junk*
        ((list length :infinity)
         (check-type length (integer 2))
         (tformat t "Adding all eval macros: length: ~a quantity: ~A" length :infinity)
         (eval-macros/quantity actions length MOST-POSITIVE-FIXNUM))
        ((list length param)
         (check-type length (integer 2))
         (let* ((prim-len (length actions))
                (quantity (ceiling (* prim-len param 1/100))))
           (check-type quantity (integer 0))
           (tformat t "Adding eval macros: length: ~a quantity: ~A -- ~a% relative to ~A"
                    length quantity param prim-len)
           (eval-macros/quantity actions length quantity)))))))

(defun cancelled-effects (ga1 ga2)
  (ematch* (ga1 ga2)
    (((pddl-ground-action :delete-list d1)
      (pddl-ground-action :add-list a2))
     (intersection d1 a2 :test #'eqstate))))

(defun pop-least-priority (queue n)
  (iter (with least = (pqueue-from-key queue))
        (repeat n)
        (while (= least (pqueue-from-key queue)))
        (pqueue-pop queue))
  queue)



(defun eval-macros/quantity (actions length quantity)
  (tformat t "Generating eval macro using Reservoir Sampling")
  (let ((count 0)
        (size 0)
        (queue (make-pqueue #'< :key-type 'fixnum)))
    (labels ((rec (length macro cancelled list)
               (if (zerop length)
                   (progn
                     (if (< size quantity) 
                         (when (< 0 cancelled)
                           (pqueue-push list cancelled queue)
                           (incf size))
                         (when (< (pqueue-front-key queue) cancelled)
                           (pqueue-pop queue)
                           (pqueue-push list cancelled queue)))
                     (incf count))
                   (map nil
                        (lambda (a)
                          (unless (conflict macro a)
                            (let ((dc (length (cancelled-effects macro a))))
                              (when (plusp dc)
                                (rec (1- length)
                                     (merge-ground-actions macro a)
                                     (+ cancelled dc)
                                     (cons a list))))))
                        actions))))
      (map nil (lambda (a) (write-char #\.) (rec (1- length) a 0 (list a))) actions)
      (tformat t "Total possible junk macros: ~a" count)
      (let ((hist (make-hash-table)))
        (prog1
          (iter (until (pqueue-empty-p queue))
                (for (values actions cancelled) = (pqueue-pop queue))
                (assert (plusp cancelled))
                (when (listp actions)
                  (ensure-gethash cancelled hist 0)
                  (incf (gethash cancelled hist))
                  (collect
                      (nullary-macro-action (nreverse (coerce actions 'vector))))))
          (iter (for (cancelled count) in-hashtable hist)
                (tformat t "(#cancelled #macro): ~a ~a" cancelled count)))))))



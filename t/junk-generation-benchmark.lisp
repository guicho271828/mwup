
(in-package :mwup.test)

(defun rel (path)
  (asdf:system-relative-pathname :mwup path))

(test benchmark
  ;; expect >4000 macros
  (let ((*package* (find-package :mwup)))
    (mwup::suppress
      (multiple-value-bind (dname domain) (mwup::parse-file (rel "t/test1/domain.pddl") nil t)
        (declare (ignorable dname))
        (multiple-value-bind (pname problem) (mwup::parse-file (rel "t/test1/p01.pddl") nil t)
          (declare (ignorable pname))
          (let ((length 3)
                (quantity 100))
            (time (let ((mwup::*start* (get-universal-time)))
                    (mwup::junk-macros-reservoir length quantity (mwup::get-all-ground-actions domain problem) domain problem)))
            (time (let ((mwup::*start* (get-universal-time)))
                    (mwup::junk-macros-greedy length quantity (mwup::get-all-ground-actions domain problem) domain problem)))))))))


(benchmark)


(test benchmark2
  ;; expect >20000 macros
  (let ((*package* (find-package :mwup))
        (mwup::*start* (get-universal-time)))
    (mwup::suppress
      (multiple-value-bind (dname domain) (mwup::parse-file (rel "t/openstacks/p11-domain.pddl") nil t)
        (declare (ignorable dname))
        (multiple-value-bind (pname problem) (mwup::parse-file (rel "t/openstacks/p11.pddl") nil t)
          (declare (ignorable pname))
          (let ((actions (mwup::get-all-ground-actions domain problem)))
            (time
             (mwup::junk-macros-greedy 2 (ceiling (* (length actions) 1 1/100)) actions domain problem))
            (mwup::tformat t "finished")))))))

(benchmark2)

;; initial :
;; Evaluation took:
;;   20.306 seconds of real time
;;   20.312000 seconds of total run time (20.284000 user, 0.028000 system)
;;   [ Run times consist of 0.052 seconds GC time, and 20.260 seconds non-GC time. ]
;;   100.03% CPU
;;   60,923,340,215 processor cycles
;;   2 page faults
;;   153,273,376 bytes consed

;; after opt:
;; Evaluation took:
;;   0.058 seconds of real time
;;   0.056000 seconds of total run time (0.056000 user, 0.000000 system)
;;   96.55% CPU
;;   172,322,658 processor cycles
;;   9,219,296 bytes consed

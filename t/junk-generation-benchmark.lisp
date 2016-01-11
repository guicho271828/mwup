
(in-package :mwup.test)

(def-suite :mwup.benchmark)
(in-suite :mwup.benchmark)

(test benchmark
      ;; expect >4000 macros
  (multiple-value-bind (dname domain) (suppress (parse-file "t/test1/domain.pddl" nil t))
    (multiple-value-bind (pname problem) (suppress (parse-file "t/test1/p01.pddl" nil t))
      (let ((length 3)
            (quantity 100))
        (time (junk-macros length quantity (get-all-ground-actions domain problem) domain problem))
        ;; (junk-macros length
        ;;              quantity
        ;;              (get-all-ground-actions domain problem)
        ;;              domain problem)
        ;; (junk-macros length
        ;;              quantity
        ;;              (get-all-ground-actions domain problem)
        ;;              domain problem)
        ))))

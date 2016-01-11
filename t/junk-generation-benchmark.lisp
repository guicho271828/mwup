
(in-package :mwup.test)

(def-suite :mwup.benchmark)
(in-suite :mwup.benchmark)

(defun rel (path)
  (asdf:system-relative-pathname :mwup path))

(test benchmark
      ;; expect >4000 macros
  (multiple-value-bind (dname domain) (mwup::suppress (parse-file (rel "t/test1/domain.pddl") nil t))
    (multiple-value-bind (pname problem) (mwup::suppress (parse-file (rel "t/test1/p01.pddl") nil t))
      (let ((length 3)
            (quantity 100))
        (time (let ((mwup::*start* (get-universal-time)))
                (mwup::suppress
                  (mwup::junk-macros length quantity (mwup::get-all-ground-actions domain problem) domain problem))))
        ;; (junk-macros length
        ;;              quantity
        ;;              (get-all-ground-actions domain problem)
        ;;              domain problem)
        ;; (junk-macros length
        ;;              quantity
        ;;              (get-all-ground-actions domain problem)
        ;;              domain problem)
        ))))

#+nil
(5am:run! 'mwup.test::benchmark)

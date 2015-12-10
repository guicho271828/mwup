(in-package :mwup)

(defun filter-trivial-macros (domain problem macros)
  (values domain
          problem
          (remove-single-macros
           (remove-null-macros macros))))

(defun remove-null-macros (macros)
  (format t "~&~40@<Filtering null macros.~>")
  (let ((filtered (remove-if (lambda-match ((macro-action actions)
                                            (= 0 (length actions))))
                             macros)))
    (format t "... ~a remaining." (length filtered))
    filtered))

(defun remove-single-macros (macros)
  (format t "~&~40@<Filtering macros with length 1.~>")
  (let ((filtered (remove-if (lambda-match ((macro-action actions)
                                            (= 1 (length actions))))
                             macros)))
    (format t "... ~a remaining." (length filtered))
    filtered))


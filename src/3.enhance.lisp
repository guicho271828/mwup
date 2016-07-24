(in-package :mwup)

(defun basic-enhance (problem domain macros)
  "Applies a basic transformation to the original domain/problem.
Adds the macros to the domain and replaces problem's pointer to the domain"
  (tformat t "Enhancing problem ~a and domain ~a with ~a macros" problem domain (length macros))
  (ematch domain
    ((pddl-domain name actions constants predicates)
     (let* ((edomain (shallow-copy domain
                                   :name (symbolicate name '-enhanced)
                                   :actions (append actions macros)
                                   :constants (append constants (objects/const problem))
                                   :predicates
                                   (if *lift*
                                       (cons (pddl-predicate
                                              :domain domain
                                              :name 'equal
                                              :parameters (list (pddl-variable :domain domain :name '?p1)
                                                                (pddl-variable :domain domain :name '?p2)))
                                             predicates)
                                       predicates)))
            (eproblem (shallow-copy problem
                                    :name (symbolicate (name problem) '-enhanced)
                                    :domain edomain
                                    :objects nil
                                    :init
                                    (if *lift*
                                        (append (mapcar (lambda (o)
                                                          (pddl-atomic-state
                                                           :domain domain
                                                           :problem problem
                                                           :name 'equal
                                                           :parameters (list o o)))
                                                        (objects problem))
                                                (init problem))
                                        (init problem)))))
       (tformat t "Max_number_of_parameters: ~a"
               (reduce #'max (mapcar (lambda (a) (length (parameters a)))
                                     (actions edomain))))
       (values eproblem edomain macros)))))






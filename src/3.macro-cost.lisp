(in-package :mwup)

(defun add-macro-cost (domain problem macros)
  "Add the action costs to the domain if it is a unit-cost domain.
Primitive actions are given a cost of 1. Macro actions are given a cost same as its length."
  (let ((*domain* (add-costs domain))
        (*problem* (add-costs problem)))
    (values *domain* *problem*
            ;; FIXME: due to caching, evaluation order is important... 
            (mapcar #'add-costs macros))))

(defun remove-cost (domain problem macros)
  (let ((*domain* (remove-costs domain))
        (*problem* (remove-costs problem)))
    (values *domain* *problem*
            ;; FIXME: due to caching, evaluation order is important... 
            (mapcar #'remove-costs macros))))

#+nil
(defvar *action-cost-plusone* nil
  "Count the cost of each action in a macro plus one. This is a similar
  behavior to that of FD's strategy.")

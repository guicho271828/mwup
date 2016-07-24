(in-package :mwup)

(defun append-junk-tag (macro)
  (ematch macro
    ((pddl-action :name (place name))
     (setf name (symbolicate *macro-prefix* name))
     macro)))

(defun junk-tag-wrapper (domain problem macros)
  (values domain problem
          (mapcar #'append-junk-tag
                  macros)))

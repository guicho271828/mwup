(in-package :mwup)

(defun mangle-wrapper (edomain eproblem macros)
  (values (mangle edomain) (mangle eproblem) (map 'vector #'mangle macros)))

(defgeneric mangle (object))
(defgeneric demangle (object))

(defvar *mangle-table* (make-hash-table))
(defvar *demangle-table* (make-hash-table))

(defmethod mangle :around (object)
  (let ((result (or (gethash object *mangle-table*)
                    (call-next-method))))
    (setf (gethash result *demangle-table*) object
          (gethash object *mangle-table*) result)
    result))

(defmethod mangle ((domain pddl-domain))
  (let ((*domain* (shallow-copy domain)))
    (setf (actions *domain*)
          (mapcar #'mangle (shuffle (actions *domain*))))
    *domain*))

(defmethod mangle ((a pddl-action))
  (shallow-copy a
                :name (gensym "MANGLED")
                :domain *domain*))

(defmethod mangle ((problem pddl-problem))
  (shallow-copy problem :domain (mangle (domain problem))))

(defmethod demangle (object)
  (gethash object *demangle-table* object))

(defmethod demangle ((ga pddl-ground-action))
  (shallow-copy ga
                :name (name (demangle (action (domain ga) (name ga))))
                :domain (demangle (domain ga))
                :problem (demangle (problem ga))))

#|
  This file is a part of mwup project.
  Copyright (c) 2015 Masataro Asai (guicho2.71828@gmail.com)
|#

#|
  Macro without Utility Problem

  Author: Masataro Asai (guicho2.71828@gmail.com)
|#



(in-package :cl-user)
(defpackage mwup-asd
  (:use :cl :asdf))
(in-package :mwup-asd)


(defsystem mwup
  :version "0.1"
  :author "Masataro Asai"
  :mailto "guicho2.71828@gmail.com"
  :license "WTFPL"
  :depends-on (:pddl.planner-scripts :pddl.macro-action :pddl :alexandria :iterate :trivia
                                     :priority-queue)
  :components ((:module :src
                :components
                ((:file :0.package)
                 (:file :1.solve)
                 (:file :2.plan-plain)
                 (:file :2.plan-macro)
                 (:file :2.plan-eval-macro)
                 (:file :3.enhance)
                 (:file :3.macro-cost)
                 (:file :3.filter)
                 (:file :3.junk)
                 (:file :3.junk-tag)
                 (:file :3.mangle))))
  :description "Macro without Utility Problem"
  :in-order-to ((test-op (test-op :mwup.test))))

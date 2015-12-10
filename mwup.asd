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
  :depends-on (:pddl.planner-scripts :pddl.macro-action :pddl :alexandria :iterate :trivia)
  :components ((:module "src"
                :components
                ((:file :0.package)
                 (:file :1.solve))))
  :description "Macro without Utility Problem"
  :in-order-to ((test-op (load-op :mwup.test))))

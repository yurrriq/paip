;;;; paip.asd

(asdf:defsystem #:paip
  :description "Paradigms of Artificial Intelligence Programming exercises"
  :author "Eric Bailey <eric@ericb.me>"
  :license "BSD-3"
  :depends-on (#:lisp-unit)
  :serial t
  :components ((:module "src"
                :serial t
                :components
                ((:file "intro")
                 (:file "simple")
                 (:file "gps")
                 (:file "eliza")
                 (:file "tools")))))

(defpackage #:paip
  (:use #:cl))
(in-package #:paip)

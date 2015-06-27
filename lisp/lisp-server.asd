;;;; lisp-server.asd

(in-package #:asdf-user)

(defsystem "lisp-server"
  :description "Local Lisp server for running Lisp software"
  :version "0.0.0"
  :author "Stanislav Kondratyev <kondratjevsk@gmail.com>"
  :licence "CC0"
  :depends-on nil
  :serial t
  :components ((:file "package")
               (:file "trivial-sbcl-server")
               (:file "lisp-server")))


(defpackage #:trivial-sbcl-server
  (:use #:cl)
  (:export #:make-server
           #:start-server
           #:stop-server
           #:server-name))

(defpackage #:lisp-server-dev
  (:use #:cl #:trivial-sbcl-server)
  (:export #:make
           #:start
           #:stop
           #:init
           #:*servers*
           #:*user-directory*
           #:*lisp-server-home*
           #:socket-file
           #:rc-file
           #:dependency-file
           #:shell-wrapper))

(defpackage #:lisp-server-user
  (:nicknames #:lisp-server)
  (:use #:cl #:lisp-server-dev)
  (:export #:make
           #:start
           #:stop
           #:init
           #:*servers*
           #:*user-directory*
           #:*lisp-server-home*
           #:socket-file
           #:rc-file
           #:dependency-file
           #:shell-wrapper))

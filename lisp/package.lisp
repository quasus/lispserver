(defpackage #:trivial-sbcl-server
  (:use #:cl)
  (:export #:make-server
           #:start-server
           #:stop-server
           #:server-name))

(defpackage #:lisp-server-dev
  (:use #:cl #:trivial-sbcl-server)
  (:export #:make-server
           #:start-server
           #:stop-server
           #:init
           #:*servers*
           #:*default-user-directory*))

(defpackage #:lisp-server-user
  (:nicknames #:lisp-server)
  (:use #:cl #:trivial-sbcl-server)
  (:export #:make-server
           #:start-server))

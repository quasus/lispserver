(in-package #:lisp-server-dev)

(defvar *servers* nil)

(defparameter *default-user-directory*
  (merge-pathnames #p".lispserver/" (user-homedir-pathname)))

(defparameter *default-bindir*
  (merge-pathnames #p"bin/" *default-user-directory*))

(defun socket-directory (user-directory)
  (merge-pathnames #p"sockets/" user-directory))

(defun socket-file (user-directory)
  (merge-pathnames #p"io-socket" (socket-directory user-directory)))

(defun rc-file (user-directory)
  (merge-pathnames #p"rc.lisp" user-directory))

(defun script-function (function)
  (lambda (args
            &key
            (standard-input *standard-input*)
            (standard-output *standard-output*)
            (error-output *error-output*)
            (query-io *query-io*)
            (terminal-io *terminal-io*))
    (let ((*standard-input* standard-input)
          (*standard-output* standard-output)
          (*error-output* error-output)
          (*query-io* query-io)
          (*terminal-io* terminal-io))
      (funcall function args))))

(defun lispserver-handler (args)
  (let ((cmd (first args))
        (*default-pathname-defaults* (if (second args)
                                         (pathname (second args))
                                         *default-pathname-defaults*))
        (argv0 (third args))
        (uiop:*command-line-arguments* (nthcdr 3 args)))
    (eval (with-standard-io-syntax
            `(flet ((uiop:argv0 ()
                      ,argv0))
               ,(read-from-string cmd))))))

(defun init (&optional (user-directory *default-user-directory*))
  (let ((socket-file (socket-file user-directory))
        (name (namestring user-directory)))
    (let ((server (find name *servers* :key #'server-name :test #'equal)))
      (when server
        (stop-server server)
        (setf *servers* (remove server *servers*))))
    (ensure-directories-exist (socket-file user-directory))
    (handler-case
      (delete-file socket-file)
      (file-error ()))
    (load (rc-file user-directory) :if-does-not-exist nil)
    (push (trivial-sbcl-server:make-server name socket-file) *servers*)))

(defun start (&optional (name (namestring *default-user-directory*)))
  (let ((server (or (find name *servers* :key #'server-name :test #'equal)
                    (error "Server ~A does not exist." name))))
    (start-server server (script-function #'lispserver-handler))))

(defun stop (&optional (name (namestring *default-user-directory*)))
  (let ((server (or (find name *servers* :key #'server-name :test #'equal)
                    (error "Server ~A does not exist." name))))
    (stop-server server)))

(defun make (&key name
                  libs
                  (entry (intern "MAIN" (find-package (or (string-upcase name)
                                                          (string-upcase (first libs))))))
                  (form `(funcall ',entry uiop:*command-line-arguments*)))
  (with-open-file (o name
                     :direction :output
                     :if-does-not-exist :create
                     :if-exists :supersede)
    (with-standard-io-syntax
      (format o "#!/bin/sh
lispctl eval ~A `pwd` \"$0\" \"$@\"
"
              (uiop/run-program:escape-sh-token (write-to-string form)))))
  (uiop/run-program:run-program `("chmod" "+x" ,name)))

(defun install (name &optional (bindir *default-bindir*))
  (unless (probe-file name)
    (error "File ~A does not exist." name))
  (let ((target (merge-pathnames name bindir)))
    (ensure-directories-exist target)
    (uiop/filesystem:rename-file-overwriting-target name target)))

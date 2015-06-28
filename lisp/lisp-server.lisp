(in-package #:lisp-server-dev)

(defvar *servers* nil)

(defparameter *default-user-directory*
  (merge-pathnames #p".lispserver/" (user-homedir-pathname)))

(defun default-bindir (user-directory)
  (merge-pathnames #p"bin/" user-directory))

(defun socket-directory (user-directory)
  (merge-pathnames #p"sockets/" user-directory))

(defun socket-file (user-directory)
  (merge-pathnames #p"io-socket" (socket-directory user-directory)))

(defun rc-file (user-directory)
  (merge-pathnames #p"rc.lisp" user-directory))

(defun software-file (user-directory)
  (merge-pathnames #p"etc/software.lisp" user-directory))

(defun dependencies-file (user-directory)
  (merge-pathnames #p"etc/dependencies.lisp" user-directory))

(defun read-software-list (user-directory)
  (with-open-file (in (software-file user-directory)
                      :direction :input
                      :if-does-not-exist nil)
    (when in
      (with-standard-io-syntax
        (read in)))))

(defun dump-software-list (software user-directory)
  (with-open-file (out (ensure-directories-exist (software-file user-directory))
                       :direction :output
                       :if-exists :supersede)
    (with-standard-io-syntax
      (print software out))))

(defun software-dependencies (software)
  (multiple-value-bind (explicit implicit) (loop :for s :in software
                                                 :append (rest (assoc :depends-on s)) :into explicit
                                                 :append (loop :for d :in (rest (assoc :depends-on s))
                                                               :append (asdf:system-depends-on (asdf:find-system d))) :into implicit
                                                 :finally (return (values explicit implicit)))
    (remove-duplicates (set-difference explicit implicit :test #'equal) :test #'equal)))

(defun read-dependencies (user-directory)
  (with-open-file (in (dependencies-file user-directory)
                      :direction :input
                      :if-does-not-exist nil)
    (when in
      (with-standard-io-syntax
        (read in)))))

(defun dump-dependencies (list user-directory)
  (with-open-file (out (ensure-directories-exist (dependencies-file user-directory))
                       :direction :output
                       :if-exists :supersede)
    (with-standard-io-syntax
      (write-line "#+(or)  \"This file has been automatically generated.\"" out)
      (print list out))))

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
    (mapc #'asdf:require-system (read-dependencies user-directory))
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

(defun make (name &key
                  (entry (intern "MAIN" (find-package (string-upcase name))))
                  (form `(funcall ',entry uiop:*command-line-arguments*)))
  (with-open-file (o name
                     :direction :output
                     :if-does-not-exist :create
                     :if-exists :supersede)
    (with-standard-io-syntax
      (format o "#!/bin/sh
lispctl eval ~S `pwd` \"$0\" \"$@\"
"
              (uiop/run-program:escape-sh-token (if (stringp form)
                                                    form
                                                    (write-to-string form))))))
  (uiop/run-program:run-program `("chmod" "+x" ,name)))

(defun server-user-directory (server)
  (server-name server))

(defun install (software server &optional (bindir (default-bindir (server-user-directory server))))
  (let* ((soft (read-software-list (server-user-directory server)))
         (name (second (assoc :name software)))
         (file (merge-pathnames name)))
    (when (member name soft
                  :key (lambda (s)
                         (second (assoc :name s)))
                  :test #'equal)
      (error "Installation error: ~A has already been installed." name))
    (unless (probe-file file)
      (error "Installation error: file ~A does not exist." file))
    (let ((target (merge-pathnames name bindir)))
      (when (probe-file target)
        (error "Installation error: file ~A already exists." target))
      (ensure-directories-exist target)
      (rename-file name target)
      (let ((new-soft `(((:file ,target)
                         (:time ,(file-write-date target))
                         ,@software) ,@soft)))
        (dump-dependencies (software-dependencies new-soft) (server-user-directory server))
        (dump-software-list new-soft (server-user-directory server))))))

(defun uninstall (name server)
  (let* ((soft (read-software-list (server-user-directory server)))
         (software (find name soft
                         :key (lambda (s)
                                (second (assoc :name s)))
                         :test #'equal)))
    (unless software
      (error "Deinstallation error: ~A has not been installed." name))
    (setf soft (delete name soft
                       :key (lambda (s)
                              (second (assoc :name s)))
                       :test #'equal))
    (dump-software-list soft (server-user-directory server))
    (dump-dependencies (software-dependencies soft) (server-user-directory server))
    (let* ((file (second (assoc :file software)))
           (time (second (assoc :time software)))
           (actual-time (if file
                            (file-write-date file)
                            nil)))
      (cond ((and file
                  (not (probe-file file)))
             (warn "File ~A does not exist." file))
            ((not (and time actual-time))
             (warn "Cannot check if ~A has been modified since installation.  You can remove it manually." file))
            ((/= time actual-time) (warn "File ~A has been modified since installation.  You can remove it manually." file))
            (t (delete-file file))))))

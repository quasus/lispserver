(in-package #:lisp-server-dev)

(defvar *servers* nil)

(defparameter *user-directory*
  (merge-pathnames #p".lispserver/" (user-homedir-pathname)))

(defparameter *lisp-server-home*
  #p"/usr/local/share/lispserver/"
  "Global directory.")

(defun socket-directory (directory)
  (merge-pathnames #p"sockets/" directory))

(defun socket-file (directory)
  (merge-pathnames #p"io-socket" (socket-directory directory)))

(defun rc-file (directory)
  (merge-pathnames #p"rc.lisp" directory))

#|

Dependency management

|#

(defstruct dependency lib names)

(defun parse-dependency-line (string)
  "Given a string, return correspondent dependency or NIL if STRING is empty or starts with a space."
  (if (or (zerop (length string))
          (eql (char string 0) #\Space))
      nil
      ;; basically, split by spaces
      (let ((name-deps (loop :for c :across string
                             :for end :from 0
                             :with start = 0
                             :when (and (eql c #\Space)
                                        (> end start))
                             :collect (subseq string start end)
                             :when (and (= end (1- (length string)))
                                        (not (eql c #\Space)))
                             :collect (subseq string start)
                             :when (eql c #\Space)
                             :do (setf start (1+ end)))))
        (make-dependency :lib (first name-deps) :names (rest name-deps)))))

(defun parse-dependency-file (file)
  "Return list of dependencies, T if the file exists and NIL, NIL if it does not.  Can signal ERRORs if something goes wrong with the system."
  (with-open-file (in file :direction :input
                      :if-does-not-exist nil)
    (if in
        (values (loop :for line = (read-line in nil)
                      :while line
                      :when (parse-dependency-line line)
                      :collect it)
                t)
        (values nil nil))))

(defun load-lib (lib &key reload quicklisp)
  "Load the ASDF system.  :QUICKLISP should be one of NIL, T, or :ASK (interactively ask the user). If the system has been loaded, return generalized truth.  If ASDF could not find it or it is not available in Quicklisp, return NIL.  If something goes wrong with ASDF or Quicklisp, they signal their errors.  It is an error to actually try to use Quicklisp if it is not supported."
  (let ((system (asdf:find-system lib nil)))
    (if system
        (if reload
            (asdf:load-system system)
            (asdf:require-system system))
        #+quicklisp
        (if (or (eq quicklisp t)
                (and (eq quicklisp :ask)
                     (y-or-n-p "Download ~S using quicklisp?" lib)))
            (handler-case
              (ql:quickload lib)
              (ql:system-not-found () nil))
            nil)
        #-quicklisp
        (when quicklisp
          (error "Quicklisp is not supported."))))) 

;;; Dependencies: auxiliary functions for the server

(defun load-dependencies (dependencies &key reload quicklisp)
  "Load the list of dependencies.  There should be no errors."
  (dolist (d dependencies)
    (handler-case
      (or (load-lib (dependency-lib d) :reload reload :quicklisp quicklisp)
          (if quicklisp
              (apply #'warn "The library ~A needed by~#[ none~; ~A~; ~A and ~A~:;~@{~#[~; and~] ~A~^,~}~] could not be found by ASDF or downloaded by Quicklisp."
                     (dependency-lib d) (dependency-names d))
              (apply #'warn "The library ~A needed by~#[ none~; ~A~; ~A and ~A~:;~@{~#[~; and~] ~A~^,~}~] could not be found by ASDF." (dependency-lib d) (dependency-names d))))
      (error (e) (apply #'warn "~A~%Error loading library ~A needed by~#[ none~; ~A~; ~A and ~A~:;~@{~#[~; and~] ~A~^,~}~]." e (dependency-lib d) (dependency-names d))))))

(defun load-libs-safely (libs &key reload quicklisp)
  "Load the list of dependencies.  There should be no errors."
  (dolist (l libs)
    (handler-case
      (or (load-lib l :reload reload :quicklisp quicklisp)
          (if quicklisp
              (warn "The library ~S could not be found by ASDF or downloaded by Quicklisp." l)
              (warn "The library ~S could not be found by ASDF." l)))
      (error (e) (warn "~A~%Error loading library ~S." e l)))))

(defun dependency-file (directory)
  (merge-pathnames #p"dependencies.conf" directory))

;;; Setting up the server

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

;;;; Interface

(defun init (&key (user-directory *user-directory*)
                  (lisp-server-home *lisp-server-home*))
  (let ((socket-file (socket-file user-directory))
        (name (namestring user-directory)))
    (let ((server (find name *servers* :key #'server-name :test #'equal)))
      (when server
        (stop-server server)
        (setf *servers* (remove server *servers*))))
    (dolist (dir (list user-directory lisp-server-home))
      (handler-case
        (load-dependencies (parse-dependency-file (dependency-file dir)))
        ;; there may be unlikely errors reading the file
        (error (e) (warn "~A" e))))
    (ensure-directories-exist (socket-file user-directory))
    (handler-case
      (delete-file socket-file)
      (file-error ()))
    (load (rc-file user-directory) :if-does-not-exist nil)
    (push (trivial-sbcl-server:make-server name socket-file) *servers*)))

(defun start (&optional (name (namestring *user-directory*)))
  (let ((server (or (find name *servers* :key #'server-name :test #'equal)
                    (error "Server ~A does not exist." name))))
    (start-server server (script-function #'lispserver-handler))))

(defun stop (&optional (name (namestring *user-directory*)))
  (let ((server (or (find name *servers* :key #'server-name :test #'equal)
                    (error "Server ~A does not exist." name))))
    (stop-server server)))

(defun shell-wrapper (name &key
                           (entry (concatenate 'string "'" (string-upcase name) "::MAIN"))
                           (form (format nil "(funcall ~A uiop:*command-line-arguments*)" entry)))
  (with-open-file (o (merge-pathnames name)
                     :direction :output
                     :if-does-not-exist :create
                     :if-exists :supersede)
    (with-standard-io-syntax
      (format o "#!/bin/sh
lispctl eval ~A `pwd`/ \"$0\" \"$@\"
"
              (uiop/run-program:escape-sh-token (if (stringp form)
                                                    form
                                                    (write-to-string form))))))
  (uiop/run-program:run-program `("chmod" "+x" ,(namestring (merge-pathnames name)))))

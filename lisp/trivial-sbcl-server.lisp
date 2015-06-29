(in-package #:trivial-sbcl-server)

(defparameter *server-stream-buffer-size* 1024)

(defparameter *log-stream* t)

(defclass server-stream (sb-gray:fundamental-character-output-stream)
  ((stream :accessor server-stream-stream
           :initarg :stream)
   (code :accessor server-stream-code
         :initarg :code)
   (buffer :reader server-stream-buffer
           :initform (make-array *server-stream-buffer-size*
                                 :element-type 'character
                                 :fill-pointer 0))))

(defun write-buffer (buf stream code)
  (let ((byte-length (length (sb-ext:string-to-octets buf))))
    (format stream "~A~6,'0X" code byte-length)
    (write-string buf stream)
    (finish-output stream)
    (setf (fill-pointer buf) 0)))

(defmethod sb-gray:stream-write-string ((stream server-stream) string &optional (start 0) end)
  (let ((buf (server-stream-buffer stream)))
    (loop :for i fixnum :from start :to (or end (1- (length string)))
          :when (= (length buf) (array-dimension buf 0))
          :do (write-buffer buf
                            (server-stream-stream stream)
                            (server-stream-code stream))
          :do (vector-push (char string i) buf))))
  
(defmethod sb-gray:stream-write-char ((stream server-stream) character)
  (let ((buf (server-stream-buffer stream)))
    (when (= (length buf) (array-dimension buf 0))
      (write-buffer buf (server-stream-stream stream) (server-stream-code stream)))
    (vector-push-extend character buf)))

(defmethod sb-gray:stream-force-output ((stream server-stream))
  (write-buffer (server-stream-buffer stream)
                (server-stream-stream stream)
                (server-stream-code stream))
  (force-output (server-stream-stream stream)))

(defmethod sb-gray:stream-finish-output ((stream server-stream))
  (write-buffer (server-stream-buffer stream)
                (server-stream-stream stream)
                (server-stream-code stream))
  (finish-output (server-stream-stream stream)))

(defclass server ()
  ((name :accessor server-name :initarg :name)
   (address :accessor server-address :initarg :address)
   (backlog :accessor server-backlog :initarg :backlog :initform 5)
   (runningp :accessor server-running-p :initform nil)
   (threads :accessor server-threads :initform '())))

(defun worker (server sock handler)
  (unwind-protect
    (let ((io (sb-bsd-sockets:socket-make-stream sock :input t :output t)))
      (let ((out (make-instance 'server-stream :stream io :code #\o))
            (err (make-instance 'server-stream :stream io :code #\e))) 
        (let* ((argc (let ((str (make-string 6)))
                       (read-sequence str io)
                       (parse-integer str :radix 16)))
               (argv (loop :repeat argc
                           :with len-str = (make-string 6)
                           :do (read-sequence len-str io)
                           :collect (let* ((len (parse-integer len-str :radix 16))
                                           (msg (make-string len)))
                                      (read-sequence msg io)
                                      msg))))
          (let ((res (ignore-errors
                       (handler-case
                         (funcall handler argv
                                  :standard-input io
                                  :standard-output out
                                  :error-output err
                                  :query-io (make-two-way-stream io out) 
                                  :terminal-io (make-two-way-stream io out))
                         ;; in case of errors, tell the client
                         ;; in case there are connection errors, never mind
                         (simple-error (e) (format err "~A~%" e) (finish-output err) nil)
                         (sb-int:simple-file-error (e) (format err "~A~%" e) (finish-output err) nil )
                         (file-error (e) (format err "Error with file ~A~%" (file-error-pathname e)) (finish-output err) nil)    
                         (error (e) (format err "Lisp error of type ~A.~%" (type-of e)) (finish-output err) nil)))))
            (finish-output out)
            (finish-output err)
            (format io "r~A" (if res (code-char 0) (code-char 1)))
            (finish-output io)))))
    (ignore-errors
      (sb-bsd-sockets:socket-close sock))
    (remove sb-thread:*current-thread* (server-threads server))))

(defun listener (server handler)
  (let ((task-id 0)
        (address (server-address server))
        (backlog (server-backlog server))
        (listen-sock (make-instance 'sb-bsd-sockets:local-socket
                                    :type :stream)))
    (sb-bsd-sockets:socket-bind listen-sock address)
    (sb-bsd-sockets:socket-listen listen-sock backlog)
    (format *log-stream* "Entering the listening loop on ~A.~%" address)
    (unwind-protect
      (loop
        (unless (server-running-p server)
          (return))
        (let ((sock (sb-bsd-sockets:socket-accept listen-sock)))
          (push (sb-thread:make-thread (lambda () (worker server sock handler))
                                       :name (format nil "server-~A-worker-thread-~A"
                                                     (server-name server)
                                                     (incf task-id)))
                (server-threads server))
          (format *log-stream* "Connection to ~A, task ID ~A.~%" (server-address server) task-id)))
      (sb-bsd-sockets:socket-close listen-sock)
      (sb-posix:unlink address)
      (remove sb-thread:*current-thread* (server-threads server)))))

(defun make-server (name address &optional (backlog 5))
  (make-instance 'server :name name :address (namestring address) :backlog backlog))

(defun start-server (server handler)
  (format *log-stream* "Starting server ~A.~%" (server-name server))
  (setf (server-running-p server) t)
  (push (sb-thread:make-thread (lambda ()
                                 (listener server handler))
                               :name (format nil "server-~A-listener-thread"
                                             (server-name server)))
        (server-threads server))
  (format *log-stream* "Server ~A has started.~%" (server-name server))
  server)

(defun server-cleanup (server)
  (setf (server-running-p server) nil)
  (dolist (thread (server-threads server))
    (when (sb-thread:thread-alive-p thread)
      (sb-thread:terminate-thread thread))))

(defun stop-server (server)
  (server-cleanup server))

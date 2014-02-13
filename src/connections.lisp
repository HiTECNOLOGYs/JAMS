(in-package :jams)

(defvar *connections* (make-hash-table :test 'equal))
(defvar *next-connection-id* 0)

;;; Waiting for a minute before closing connection
(define-constant +max-no-keep-alive-time+ (* 1 60)
  :test #'=)

(defun gen-connection-id ()
  (prog1 *next-connection-id*
    (incf *next-connection-id*)))

(defclass Connection ()
  ((id :initform (gen-connection-id)
       :reader connection-id)
   (remote-address :initarg :remote-address
                   :reader connection-remote-address)
   (remote-port :initarg :remote-port
                :reader connection-remote-port)
   (data-queue :initform (lparallel.queue:make-queue)
               :accessor connection-data-queue)
   (socket :initarg :socket
           :reader connection-socket)
   (status :initform :opening
           :accessor connection-status)
   (client :accessor connection-client)
   (keep-alive-received? :initform nil
                         :accessor connection-keep-alive-received-p)
   (last-keep-alive-id :initform 0
                       :accessor connection-last-keep-alive-id)
   (last-keep-alive-time :initform (get-universal-time)
                         :accessor connection-last-keep-alive-time)
   (data-handler :initarg :data-handler
                 :accessor connection-data-handler)))

(define-condition Close-connection ()
  ((connection :initarg :connection)
   (reason :initarg :reason)))

(defgeneric dispatch-connection (connection received-data)
  (:method ((connection Connection) (received-data vector))
    (case (connection-status connection)
      (:opened
       ;; (setf (connection-status connection) :running)
       )
      (:running
       (when (< +max-no-keep-alive-time+
                (- (connection-last-keep-alive-time connection)
                   (get-universal-time)))
         (terminate-connection connection))))
    (when (slot-boundp connection 'data-handler)
      (funcall (connection-data-handler connection)
               connection received-data))))

(defgeneric connection-closed-p (connection)
  (:method ((connection Connection))
    (eql (connection-status connection) :closed)))

(defgeneric connection-running-p (connection)
  (:method ((connection Connection))
    (not (connection-closed-p connection))))

(defgeneric terminate-connection (connection)
  (:method ((connection Connection))
    (setf (connection-status connection) :closed)))

(defgeneric delete-connection (connection)
  (:method ((connection Connection))
    (remhash (cons (connection-remote-address connection)
                   (connection-remote-port connection))
             *connections*)))

(defgeneric send-data (data connection)
  (:method ((data vector) (connection Connection))
    (let ((queue (connection-data-queue connection)))
      (lparallel.queue:with-locked-queue queue
        (lparallel.queue:push-queue/no-lock data queue)))))

(defun get-connection (remote-address remote-port)
  (gethash (cons remote-address remote-port) *connections*))

(defun (setf get-connection) (new-value remote-address remote-port)
  (setf (gethash (cons remote-address remote-port)
                 *connections*)
        new-value))

(defun open-connection (remote-address remote-port socket data-handler)
  (setf (get-connection remote-address remote-port)
        (make-instance 'Connection
                       :remote-address remote-address
                       :remote-port remote-port
                       :data-handler data-handler
                       :socket socket)))

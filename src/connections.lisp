(in-package :jams)

(defvar *connections* (make-hash-table :test 'equal))
(defvar *next-connection-id* 0)

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
   (dispatcher :initarg :dispatcher
               :initform #'dispatch-connection
               :accessor connection-dispatcher)))

(defgeneric dispatch-connection (connection)
  (:method ((connection Connection))
    (handler-case
      (case (connection-status connection)
        (:opened
         ;; Init connection here
         )
        (:running
         ;; Process player actions here
         )))))

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

(defun get-connection (remote-address remote-port)
  (gethash (cons remote-address remote-port) *connections*))

(defun (setf get-connection) (new-value remote-address remote-port)
  (setf (gethash (cons remote-address remote-port)
                 *connections*)
        new-value))

(defun open-connection (remote-address remote-port socket)
  (setf (get-connection remote-address remote-port)
        (make-instance 'Connection
                       :remote-address remote-address
                       :remote-port remote-port
                       :socket socket)))

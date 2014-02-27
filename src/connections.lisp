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
                 :accessor connection-data-handler)
   (termination-handler :accessor connection-termination-handler)))

(define-condition Close-connection ()
  ((connection :initarg :connection)
   (reason :initarg :reason)))

(defgeneric dispatch-connection (connection received-data)
  (:method ((connection Connection) (received-data vector))
    (flet
       ((call-data-handler ()
          (when (and (not (zerop (length received-data)))
                     (slot-boundp connection 'data-handler))
            (funcall (connection-data-handler connection)
                     connection received-data))))
      (case (connection-status connection)
        (:opening
         (unless (zerop (length received-data))
           (call-data-handler)))
        (:running
         (unless (zerop (length received-data))
           (call-data-handler))
         (when (< +max-no-keep-alive-time+
                  (- (connection-last-keep-alive-time connection)
                     (get-universal-time)))
           (terminate-connection connection "Didn't receive keep alive.")))
        (:closed
         (when (slot-boundp connection 'termination-handler)
           (funcall (connection-termination-handler connection)
                    connection)))))))

(defun get-connection (connection-id)
  (gethash connection-id *connections*))

(defun (setf get-connection) (new-value connection-id)
  (setf (gethash connection-id *connections*)
        new-value))

(defun connection-closed-p (connection)
  (eql (connection-status connection) :closed))

(defun connection-running-p (connection)
  (not (connection-closed-p connection)))

(defun terminate-connection (connection reason)
  (error 'Close-connection
         :connection connection
         :reason reason))

(defun close-connection (connection)
  (setf (connection-status connection) :closed))

(defun delete-connection (connection)
  (remhash (connection-id connection) *connections*))

(defgeneric send-data (data connection)
  (:method ((data vector) (connection Connection))
    (let ((queue (connection-data-queue connection)))
      (lparallel.queue:with-locked-queue queue
        (lparallel.queue:push-queue/no-lock data queue)))))

(defun open-connection (remote-address remote-port socket data-handler)
  (let ((connection (make-instance 'Connection
                                   :remote-address remote-address
                                   :remote-port remote-port
                                   :data-handler data-handler
                                   :socket socket)))
    (setf (get-connection (connection-id connection)) connection)
    connection))

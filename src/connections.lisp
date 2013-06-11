(in-package :jams)

(defparameter *connection-statuses*
  '((:connecting  . process-new-client)
    (:established . process-connected-client)))

(define-condition Change-connection-status ()
  ((socket :initarg :socket
           :reader socket)
   (new-status :initarg :new-status
               :reader new-status)))

(define-condition Drop-connection ()
  ((message :initarg :message
            :reader message)
   (socket :initarg :socket
           :reader socket)))

(defun get-connection-status-processor (status)
  (cdr (assoc status *connection-statuses*)))

(defstruct Connection
  id
  (status :connecting))

(let ((connections (make-hash-table))
      sockets)
  
  (defun clear-connections ()
    (clrhash connections))
  
  (defun clear-sockets ()
    (mapc #'socket-close sockets)
    (setf sockets nil))
  
  (defun get-connections ()
    connections)
  
  (defun get-sockets ()
    sockets)
  
  (defun (setf get-connections) (value)
    (setf connections value))
  
  (defun (setf get-sockets) (value)
    (setf sockets value))
  
  (defun get-connection (socket)
    (gethash socket connections))
  
  (defun (setf get-connection) (value socket)
    (setf (gethash socket connections) value))
  
  (defun add-socket (socket)
    (push socket sockets))
  
  (defun remove-socket (socket)
    (setf sockets (remove socket sockets)))
  
  (defun add-connection (socket connection)
    (setf (get-connection socket) connection))

  (defun remove-connection (socket)
    (unwind-protect (socket-close socket)
      (remhash socket connections))
    (values)))

(defun drop-connection (socket)
  (remove-socket socket)
  (remove-connection socket))

(let ((connection-id-counter 0))
  (defun establish-connection (socket)
    (let ((connection (make-connection :id (incf connection-id-counter))))
      (add-socket socket)
      (add-connection socket connection))))

(defun drop-connection-handler (condition)
  (let ((socket (socket condition))
        (message (message condition)))
    (princ message)
    (terpri)
    (force-output)
    (drop-connection socket)))

(defun invalid-packet-handler (condition)
  (let ((socket (socket condition))
        (message (message condition))
        (data (data condition)))
    (format t "~A: ~A~%" message data)
    (force-output)
    (drop-connection socket)))

(defun change-connection-status-handler (condition)
  (let ((connection (get-connection (socket condition)))
        (new-status (new-status condition)))
    (format t "Changing connection [~D] status to ~A~%" (connection-id connection) new-status)
    (setf (connection-status connection)
          new-status)))

(defun process-connection (socket)
  (handler-case
    (let ((connection (get-connection socket)))
      (handler-bind
          ((change-connection-status #'change-connection-status-handler))
        (funcall (get-connection-status-processor (connection-status connection))
                 socket
                 connection)
        (finish-output (socket-stream socket)))
      t)
    
    (end-of-file ()
      (drop-connection-handler (make-condition 'Drop-connection
                                               :socket socket
                                               :message "Client dropped conenction")))

    (drop-connection (condition)
      (drop-connection-handler condition))

    (invalid-packet (condition)
      (invalid-packet-handler condition))))

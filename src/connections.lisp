(in-package :jams)

(defparameter *connection-statuses*
  '((:connecting  . process-new-client)
    (:established . send-data-to-client)
    (:running     . process-client)))

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


(in-package :jams)

(defun receive-field (stream type-definition)
  (read-typedef stream type-definition))

(defun receive-packet (stream)
  (let ((packet-id (read-byte stream)))
    (cons packet-id
          (mapcar (curry #'receive-field stream)
                  (packet-definition-structure (get-packet-definition packet-id))))))

(defun receive-and-process-packet (socket)
  (let ((stream (socket-stream socket)))
    (process-packet socket (receive-packet stream))))

(defun send-data-to-client (socket connection)
  (format t "Sending data to client #~D~%" (connection-id connection))
  (write-sequence (encode-packet 'login-request
                                 '((:integer 228)
                                   "default"
                                   0
                                   0
                                   0
                                   0
                                   16))
                  (socket-stream socket))
  (write-sequence (encode-packet 'spawn-position
                                 '((:integer 0)
                                   (:integer 0)
                                   (:integer 0)))
                  (socket-stream socket))
  (write-sequence (encode-packet 'player-position-and-look
                                 '((:double 0.0)
                                   (:double 0.0)
                                   (:double 2.0)
                                   (:double 0.0)
                                   0.0
                                   0.0
                                   t))
                  (socket-stream socket)))

(defun process-new-client (socket connection)
  (format t "Connecting client with ID: ~D~%" (connection-id connection))
  (receive-and-process-packet socket)
  (send-data-to-client socket connection)
  (signal 'Change-connection-status :socket socket :new-status :established))

(defun keep-alive-client (socket)
  (handler-case (progn (write-sequence (make-keep-alive-packet)
                                       (socket-stream socket))
                       (finish-output))
    (sb-int:simple-stream-error ()
      (communication-error-handler socket))))

(defun keep-alive-everybody ()
  (dolist (socket (get-sockets))
    (let ((connection (get-connection socket)))
      (when (eql (connection-status connection) :established)
        (keep-alive-client socket)))))

(defun process-connected-client (socket connection)
  (declare (ignore connection))
  (keep-alive-client socket)
  (receive-and-process-packet socket))

(defun network-listener-thread ()
  (let ((port (receive-message :network-listener-thread)))
    (with-socket-listener (socket *wildcard-host* port
                                  :element-type '(unsigned-byte 8) :reuseaddress t)
      (format t "Starting listener on port ~D~%" port)
      (unwind-protect
        (loop
           for message = (unless (message-queue-empty-p :network-listener-thread)
                           (receive-message :network-listener-thread))
           if (eql message :stop)
           do (format t "Stopping listener on port ~D~%" port)
             (return nil)
           else
           do (handler-case
                  (dolist (ready-socket (wait-for-input (cons socket (get-sockets))
                                                        :ready-only t
                                                        :timeout 5))
                    (if (eql ready-socket socket)
                        (add-task-to-queue #'establish-connection (socket-accept ready-socket))
                        (add-task-to-queue #'process-connection ready-socket)))
                (sb-int:simple-stream-error ()
                  ;; TODO rewrite entire network listener to make it check sockets state first
                  nil))
             (run-tasks-queue))
        (progn (clear-sockets)
               (clear-connections))))))

(defun server (port)
  (setf *kernel* (make-kernel +max-number-of-threads+))
  (run-separate-thread #'server-main-thread :main-thread)
  (run-separate-thread #'network-listener-thread :network-listener-thread)
  (send-message-to-thread :network-listener-thread port)
  (values))

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
                                   (:string "default")
                                   (:byte 0)
                                   (:byte 0)
                                   (:byte 0)
                                   (:byte 0)
                                   (:byte 16)))
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
                                   (:float 0.0)
                                   (:float 0.0)
                                   (:bool 1)))
                  (socket-stream socket)))

(defun process-new-client (socket connection)
  (format t "Connecting client with ID: ~D~%" (connection-id connection))
  (receive-and-process-packet socket)
  (send-data-to-client socket connection)
  (signal 'Change-connection-status :socket socket :new-status :established))

(defun process-connected-client (socket connection)
  (declare (ignore connection))
  (receive-and-process-packet socket))

(defun server (port)
  (setf *kernel* (make-kernel +max-number-of-threads+))
  (with-socket-listener (socket *wildcard-host* port :element-type '(unsigned-byte 8) :reuse-address t)
    (add-socket socket)
    (unwind-protect
         (loop
            (handler-case
                (dolist (ready-socket (wait-for-input (get-sockets) :ready-only t))
                  (if (eql ready-socket socket)
                      (add-to-queue #'establish-connection (socket-accept ready-socket))
                      (add-to-queue #'process-connection ready-socket)))
              
              (sb-int:simple-stream-error ()
                nil)

              (sb-sys:interactive-interrupt ()
                (return-from server (values))))
            (run-queue))
      (progn (clear-sockets)
             (clear-connections))))
  (values))

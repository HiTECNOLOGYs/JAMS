(in-package :jams)

(defun receive-data (stream)
  (loop for byte = (read-byte stream nil nil)
        while byte
        collecting byte))

(defun listener (socket)
  (handler-case
      (let* ((stream (socket-stream socket))
             (responses (process-packet (receive-data stream))))
        (dolist (response responses)
          (destructuring-bind (packet-id . data) response
            (when (and packet-id data)
              (write-sequence (encode-packet packet-id data)
                              stream)
              (force-output stream)))))
    (invalid-packet ()
      (write-sequence (encode-packet +kick-packet-id+
                                     '((:string "Something went terribly wrong. We're working on it.")))
                      (socket-stream socket)))))

(defun server (port)
  (with-socket-listener (socket "127.0.0.1" port :element-type '(unsigned-byte 8) :reuse-address t)
    (loop (with-connected-socket (connected-socket (socket-accept socket))
            (listener connected-socket)))))

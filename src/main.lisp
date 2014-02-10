(in-package :jams)

#+nil
(defun send-some-data (socket connection)
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

(defun server (port)
  (setf *kernel* (make-kernel +max-number-of-threads+))

  (fork #'server-main-thread :main-thread)

  (init-network)
  (start-network-listener port)
  t)

(defun main ()
  ;; Returning exit codes in case somebody runs this from shell.
  ;; Otherwise, cosider using SERVER directly.
  (if (server 25565)
    0
    1))

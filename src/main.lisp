(in-package :jams)

(defun server (port)
  (usocket:socket-server "127.0.0.1" port #'listener
                         nil :element-type '(unsigned-byte 8)))

(defun main ()
  (server 25565))

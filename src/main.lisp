(in-package :jams)

(defun server (port)
  (setf *kernel* (make-kernel +max-number-of-threads+))

  (fork #'server-main-thread :main-thread)

  (start-network-listener port)
  t)

(defun main ()
  ;; Returning exit codes in case somebody runs this from shell.
  ;; Otherwise, cosider using SERVER directly.
  (if (server 25565)
    0
    1))

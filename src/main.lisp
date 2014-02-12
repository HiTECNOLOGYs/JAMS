(in-package :jams)

(defun start-server (port)
  (setf *kernel* (make-kernel +max-number-of-threads+))

  (unwind-protect
       (progn
         ;; Starting main thread that processes all game events
         (start-thread 'main-thread )

         ;; Starting network listener thread that accepts new connections
         ;; and manages clients
         (start-thread 'network-listener port))

    ;; Stopping threads
    (stop-thread 'main-thread)
    (stop-thread 'network-listener))
  t)

(defun stop-server ()
  (and (stop-thread 'main-thread)
       (stop-thread 'network-listener)))

(defun main ()
  ;; Returning exit codes in case somebody runs this from shell.
  ;; Otherwise, cosider using START-SERVER/STOP-SERVER directly.
  (if (start-server 25565)
    0
    1))

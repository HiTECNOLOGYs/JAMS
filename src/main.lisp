(in-package :jams)

(defun start-server (port)
  (setf lparallel:*kernel* (lparallel:make-kernel +max-number-of-threads+))

  ;; Starting main thread that processes all game events
  (start-thread 'main-thread )

  ;; Starting network listener thread that accepts new connections
  ;; and manages clients
  (start-thread 'network-listener port)

  ;; Starting timer that keeps clients alive
  (schedule-timer "keep-clients-alive"
                  'network-listener
                  #'keep-alive-everybody
                  1
                  :repeat-interval 1)
  t)

(defun stop-server ()
  (stop-thread 'main-thread)
  (stop-thread 'network-listener)
  t)

(defun main ()
  ;; Returning exit codes in case somebody runs this from shell.
  ;; Otherwise, cosider using START-SERVER/STOP-SERVER directly.
  (let ((server-return (start-server 25565)))
    ;; Start interactive UI here
    (if server-return
      0
      1)))

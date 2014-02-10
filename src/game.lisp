(in-package :jams)

(define-constant +ticks-per-second+ 20)

(defun server-main-thread ()
  #+jams-print-debug-messages
    (format t "Startng main thread~%")
  (loop for message = (unless (message-queue-empty-p :main-thread)
                        (receive-message :main-thread))
     if (eql message :stop)
       do #+jams-print-debug-messages
            (format t "Stopping main thread~%")
          (return nil)
     else
     do (with-delay ((/ 1 +ticks-per-second+))
          (server-tick))))

(defun server-tick ()
  ;; Keeping alive clients
  ;; (keep-alive-everybody)
  ;; Stuff goes here
  )

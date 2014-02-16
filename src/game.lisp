(in-package :jams)

(define-constant +ticks-per-second+ 20
  :test #'=)

(defthread main-thread (&aux (stopped? nil))
    (setf stopped? t)
  (iter (until stopped?)
    (with-delay (/ 1 +ticks-per-second+)
      (server-tick))))

(defmacro doclients ((remote-address-var connection-var) &body body)
  `(iter (for (,remote-address-var ,connection-var) in-hashtable *connections*)
     (after-each ,@body)))

(defun increment-time ()
  (setf (world-age *world*)  (1+ (world-age *world*))
        (world-time *world*) (mod (1+ (world-time *world*))
                                  +ticks-per-game-day+)))

(defun server-tick ()
  (increment-time)
  (doclients (remote-address connection)
    (keep-alive-client connection)
    (send-packet 'time-update
                 connection
                 `((:long ,(world-age *world*))
                   (:long ,(world-time *world*))))))

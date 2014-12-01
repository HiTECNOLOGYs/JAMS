(in-package :jams)

(defvar *world*
  (make-instance 'World
                 :name "Main"
                 :description "Main world."
                 :spawn-point (list 0.0 0.0 0.0))
  "Currently server supports only one world. This variables stores it.")

(defthread main-thread (&aux (stopped? nil))
    (setf stopped? t)
  (iter (until stopped?)
    (with-delay (/ 1 *ticks-per-second*)
      (server-tick))))

(defmacro doclients ((remote-address-var connection-var) &body body)
  `(iter (for (,remote-address-var ,connection-var) in-hashtable *connections*)
     (after-each ,@body)))

(defun increment-time ()
  (setf (world-age *world*)         (1+ (world-age *world*))
        (world-time-of-day *world*) (mod (1+ (world-time-of-day *world*))
                                         *ticks-per-game-day*)))

(defun server-tick ()
  (increment-time)
  (doclients (remote-address connection)
    #+nil
    (send-packet 'time-update
                 connection
                 `((:long ,(world-age *world*))
                   (:long ,(world-time-of-day *world*))))))

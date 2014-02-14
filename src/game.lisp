(in-package :jams)

(define-constant +ticks-per-second+ 20
  :test #'=)

(defthread main-thread (&aux (stopped? nil))
    (setf stopped? t)
  (iter (until stopped?)
    (with-delay (/ 1 +ticks-per-second+)
      (server-tick))))

(defun keep-alive-everybody ()
  (iter (for (remote-address connection) in-hashtable *connections*)
    (keep-alive-client connection)))

(defun server-tick ()
  ;; Stuff goes here
  )

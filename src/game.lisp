(in-package :jams)

(define-constant +ticks-per-second+ 20
  :test #'=)

(defun server-main-thread ()
  (loop
    (with-delay (/ 1 +ticks-per-second+)
      (server-tick))))

(defun server-tick ()
  ;; Keeping alive clients
  ;; (keep-alive-everybody)
  ;; Stuff goes here
  )

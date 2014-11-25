(in-package :jams)

(defmacro doarray ((array counter element &key size increment) &body body)
  `(iter
     (for ,counter below (1- (or ,size (array-total-size ,array)))
          by (or ,increment 1))
     (for ,element next (row-major-aref ,array ,counter))
     (after-each ,@body)))

(defun read-bytes (stream count)
  (iter
    (for i from 0 below count)
    (with result = (make-array count))
    (setf (svref result i) (read-byte stream))
    (finally (return result))))

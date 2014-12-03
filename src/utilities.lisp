(in-package :jams)

(defmacro doarray ((array counter element &key size (increment 1)) &body body)
  `(iter
     (for ,counter below (1- (or ,size (array-total-size ,array))) by ,increment)
     (for ,element next (row-major-aref ,array ,counter))
     (after-each ,@body)))

(defun read-bytes (count stream)
  (iter
    (for i from 0 below count)
    (with result = (make-array count
                               :initial-element 0
                               :fill-pointer 0
                               :element-type '(unsigned-byte 8)))
    (after-each
      (vector-push (read-byte stream) result))
    (finally (return result))))

(defun write-bytes (vector stream)
  (iter
    (for byte in-vector vector)
    (after-each
      (write-byte byte stream))))

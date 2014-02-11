(in-package :jams)

(defmacro doarray ((array counter element &optional size increment) &body body)
  `(loop for ,counter upto ,(1- (if size (symbol-value size) (array-total-size array)))
         by ,(if increment increment 1)
         for ,element = (row-major-aref ,array ,counter)
         do ,@body))

(defun log-message (type message &rest arguments)
  (format t
          (case type
            (:info     "[INFO]  ~?~%")
            (:warning  "[WARN]  ~?~%")
            (:error    "[ERR]   ~?~%")
            (:critical "[!ERR!] ~?~%")
            (otherwise (error "Unknown log message type: ~S" type)))
          message
          arguments))

(in-package :jams)

(defmacro doarray ((array counter element &optional size increment) &body body)
  `(iter
     (for ,counter to ,(1- (if size (symbol-value size) (array-total-size array)))
          by ,(if increment increment 1))
     (for ,element next (row-major-aref ,array ,counter))
     (after-each ,@body)))

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

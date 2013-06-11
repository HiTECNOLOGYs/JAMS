(in-package :jams)

(defun curry (function &rest args)
  #'(lambda (&rest more-args)
      (apply function (append args
                              more-args))))

(defun compose (function-1 function-2)
  #'(lambda (&rest arguments)
      (funcall function-1 (apply function-2 arguments))))

(defmacro define-constant (name value &optional doc)
  `(defconstant ,name (if (boundp ',name) (symbol-value ',name) ,value)
     ,@(when doc (list doc))))

(defmacro aif (condition if-true &optional if-false)
  `(let ((it ,condition))
     (if it
         ,if-true
         ,if-false)))

(defmacro awhen (condition &body body)
  `(aif ,condition
        (progn ,@body)))

(defun read-bytes (stream byte-count)
  (loop repeat byte-count
        with result = (make-array (list 0) :adjustable t :fill-pointer 0 :element-type '(unsigned-byte 8))
        do (vector-push-extend (read-byte stream)
                               result)
        finally (return (reverse result))))

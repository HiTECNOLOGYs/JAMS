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

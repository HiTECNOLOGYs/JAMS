(in-package :jams)

(defun curry (function &rest args)
  #'(lambda (&rest more-args)
      (apply function (append args
                              more-args))))
(defmacro define-constant (name value &optional doc)
  `(defconstant ,name (if (boundp ',name) (symbol-value ',name) ,value)
     ,@(when doc (list doc))))

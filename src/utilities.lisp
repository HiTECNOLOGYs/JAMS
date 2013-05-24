(in-package :jams)

(defun curry (function &rest args)
  #'(lambda (&rest more-args)
      (apply function (append args
                              more-args))))

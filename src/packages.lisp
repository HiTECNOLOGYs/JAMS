(defpackage :jams
  (:use :closer-common-lisp
        :flexi-streams
        :ieee-floats
        :cl-store
        :bordeaux-threads
        :alexandria
        :anaphora
        :iterate)
  (:shadow :float
           :character
           :boolean
           :position)
  (:export :main))

(defpackage :jams
  (:use :cl
        :flexi-streams
        :ieee-floats
        :cl-store
        :bordeaux-threads
        :alexandria
        :anaphora
        :iterate)
  (:shadow :float
           :character
           :boolean)
  (:export :main))

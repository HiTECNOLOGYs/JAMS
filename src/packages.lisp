(defpackage :jams
  (:use :cl
        :flexi-streams
        :ieee-floats
        :lparallel
        :cl-store
        :bordeaux-threads
        :alexandria
        :anaphora
        :iterate)
  (:export :main))

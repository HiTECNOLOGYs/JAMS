(defpackage :jams
  (:use :closer-common-lisp
        :flexi-streams
        :ieee-floats
        :cl-store
        :bordeaux-threads
        :alexandria
        :iterate)
  (:shadow :float
           :character
           :string
           :boolean
           :position)
  (:export :main))

;; Send to client
(defpackage :jams.packets.client
  (:use))

;; Handled by server
(defpackage :jams.packets.server
  (:use))

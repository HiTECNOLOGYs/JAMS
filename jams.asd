(defpackage :jams-system
  (:use :cl
        :asdf))

(in-package :jams-system)

(defsystem :jams
  :author "Mark Fedurin <hitecnologys@gmail.com>"
  :description "Simple Minecraft server."
  :defsystem-depends-on (flexi-streams
                         usocket
                         ieee-floats
                         lparallel
                         bordeaux-threads
                         salza2
                         cl-store)
  :components ((:module :src
                        :serial t
                        :components ((:file "packages")
                                     (:file "utilities")
                                     (:file "types")
                                     (:file "packets")
                                     (:file "protocol")
                                     (:file "threading")
                                     (:file "connections")
                                     (:file "game")
                                     (:file "networking")
                                     (:file "main")))))

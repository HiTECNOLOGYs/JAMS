(defsystem :jams
  :author "Mark Fedurin <hitecnologys@gmail.com>"
  :description "Simple Minecraft server."
  :depends-on (:flexi-streams
               :iolib
               :ieee-floats
               :lparallel
               :bordeaux-threads
               :salza2
               :cl-store
               :alexandria
               :anaphora
               :trivial-garbage
               :iterate)
  :pathname "src/"
  :serial t
  :components ((:file "packages")
               (:file "utilities")
               (:file "types")
               (:file "packets")
               (:file "protocol")
               (:file "threading")
               (:file "connections")
               (:file "objects")
               (:file "entities")
               (:file "map")
               (:file "game")
               (:file "networking")
               (:file "main")))

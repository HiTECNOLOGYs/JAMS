(defsystem :jams
  :author "Mark Fedurin <hitecnologys@gmail.com>"
  :description "Simple Minecraft server."
  :depends-on (:flexi-streams
               :iolib
               :ieee-floats
               :lparallel
               :bordeaux-threads
               :salza2
               :cl-store)
  :pathname "src/"
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
               (:file "main")))

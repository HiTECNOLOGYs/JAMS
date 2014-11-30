(defsystem :jams
  :author "Mark Fedurin <hitecnologys@gmail.com>"
  :description "Simple Minecraft server."
  :version (:read-file-form "version.lisp-expr")
  :depends-on (:flexi-streams
               :iolib
               :ieee-floats
               :lparallel
               :bordeaux-threads
               :salza2
               :cl-store
               :alexandria
               :iterate
               :closer-mop
               :log4cl)
  :pathname "src/"
  :serial t
  :components ((:file "packages")
               (:file "config")
               (:file "utilities")
               (:module "foundation"
                        :pathname "foundation/"
                        :serial t
                        :components ((:file "threading")
                                     (:file "types")
                                     (:file "sockets")
                                     (:file "packets")
                                     (:file "protocol")))
               (:module "gameplay"
                        :pathname "gameplay/"
                        :serial t
                        :components ((:file "objects")
                                     (:file "entities")
                                     (:file "map")
                                     (:file "actions")
                                     (:file "game")))
               (:file "main")))

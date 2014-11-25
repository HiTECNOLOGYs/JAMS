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
               :anaphora
               :trivial-garbage
               :iterate
               :closer-mop
               :log4cl)
  :pathname "src/"
  :serial t
  :components ((:file "packages")
               (:file "config")
               (:file "utilities")
               (:file "types")
               (:file "packets")
               (:file "protocol")
               (:file "threading")
               (:file "connections")
               (:file "objects")
               (:file "entities")
               (:file "map")
               (:file "actions")
               (:file "game")
               (:file "networking")
               (:file "main")))

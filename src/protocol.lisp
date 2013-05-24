(in-package :jams)

(defpacket #x02 ((prot-id :byte) (nick :string) (address :string) (port :integer))
  (format t "***HANDSHAKE***~%Protocol ID: ~D~%Nickname: ~A~%Address: ~A~%Port: ~A~%"
          prot-id
          nick
          address
          port)
  (force-output)
  '((#x01
     (:integer 1298)
     (:byte #b110)
     (:byte 0)
     (:byte 3)
     (:byte 0)
     (:byte 100)))))

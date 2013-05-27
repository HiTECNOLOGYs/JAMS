(in-package :jams)

(define-constant +kick-packet-id+ #xFF)

(defpacket #x02 ((prot-id :byte) (nick :string) (address :string) (port :integer))
  (format t "***HANDSHAKE***~%Protocol ID: ~D~%Nickname: ~A~%Address: ~A~%Port: ~A~%"
          prot-id
          nick
          address
          port)
  '((#x01
     (:int 314)
     (:string "default")
     (:byte 0)
     (:byte 0)
     (:byte 3)
     (:byte 0)
     (:byte 8))))

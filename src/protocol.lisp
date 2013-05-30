(in-package :jams)

(define-constant +kick-packet-id+ #xFF)

(defpacket (login-request #x01) ((:integer entity-id)
                                 (:string level-type)
                                 (:byte game-mode)
                                 (:byte dimension)
                                 (:byte difficulty)
                                 (:byte nothing) ; was used by vanilla server to set world height
                                 (:byte max-players)))

(defpacket (handshake #x02) ((:byte prot-id) (:string nick) (:string address) (:integer port))
  (format t "***HANDSHAKE***~%Protocol ID: ~D~%Nickname: ~A~%Address: ~A~%Port: ~A~%"
          prot-id
          nick
          address
          port)
  (encode-packet 'login-request
                 '((:integer 228)
                   (:string "default")
                   (:byte 0)
                   (:byte 0)
                   (:byte 1)
                   (:byte 0)
                   (:byte 16))))

(defpacket (ping #xFE) ((:byte magic))
  (declare (ignore magic))
  (packet-attach-id 'kick
                    (encode-ping-response "61" "1.5.2" "MAMKU EBAL" "8" "32")))

(defpacket (kick #xFF) ((:string message)))

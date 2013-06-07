(in-package :jams)

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
  (write-sequence (encode-packet 'login-request
                                 '((:integer 228)
                                   (:string "default")
                                   (:byte 0)
                                   (:byte 0)
                                   (:byte 0)
                                   (:byte 0)
                                   (:byte 16)))
                  (socket-stream socket)))

(defpacket (ping #xFE) ((:byte magic))
  (declare (ignore magic))
  (write-sequence (make-packet 'kick
                               (encode-ping-response "61" "1.5.2" "MAMKU EBAL" "8" "32"))
                  (socket-stream socket))
  (error 'drop-connection
         :socket socket
         :message "Kicking client after ping request"))

(defpacket (kick #xFF) ((:string message)))

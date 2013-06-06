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
  (write-sequence (encode-packet 'kick
                                 '((:string "SASAI LALKA")))
                  (socket-stream socket)))

(defpacket (encryption-key-request #xFD) ())

(defpacket (ping #xFE) ((:byte magic))
  (declare (ignore magic))
  (write-sequence (packet-attach-id 'kick
                                    (encode-ping-response "61" "1.5.2" "MAMKU EBAL" "8" "32"))
                  (socket-stream socket))
  (error 'drop-connection
         :socket socket
         :message "Kicking client after ping request."))

(defpacket (kick #xFF) ((:string message)))

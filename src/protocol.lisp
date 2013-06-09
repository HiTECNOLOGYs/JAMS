(in-package :jams)

(defpacket (keep-alive #x00) ((:integer id))
  (format t "Keep alive: ~D~%" id)
  (write-sequence (encode-packet 'keep-alive
                                 `((:integer ,(1- (ash (random most-positive-fixnum) -1)))))
                  (socket-stream socket)))

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
                  (socket-stream socket))
  (write-sequence (encode-packet 'spawn-position
                                 '((:integer 0)
                                   (:integer 0)
                                   (:integer 0)))
                  (socket-stream socket))
  (write-sequence (encode-packet 'player-position-and-look
                                 '((:double 0.0)
                                   (:double 0.0)
                                   (:double 2.0)
                                   (:double 0.0)
                                   (:float 0.0)
                                   (:float 0.0)
                                   (:bool 1)))
                  (socket-stream socket)))

(defpacket (spawn-position #x06) ((:integer x) (:integer y) (:integer z)))

(defpacket (player-position #x0B) ((:double x) (:double y) (:double stance) (:double z) (:bool on-ground?))
  (format t "***PLAYER POSITION***~%X: ~D~%Y: ~D~%Stance: ~D~%Z: ~D~%On ground: ~A~%"
          x y stance z on-ground?))

(defpacket (player-position-and-look #x0D) ((:double x)
                                            (:double y)
                                            (:double stance)
                                            (:double z)
                                            (:float yaw)
                                            (:float pitch)
                                            (:bool on-ground?))
  (format t "***PLAYER-POSITION-AND-LOOK***~%X: ~8$~%Y: ~8$~%Stance: ~8$~%Z: ~8$~%Yaw: ~4$~%Pitch: ~4$~%On-gound: ~A~%"
          x y stance z yaw pitch on-ground?))

(defpacket (client-statuses #xCD) ((:byte payload))
  (format t "***CLIENT STATUSES***Payload: ~D~%"
          payload))

(defpacket (ping #xFE) ((:byte magic))
  (declare (ignore magic)) ; assuming magic is always 1
  (write-sequence (make-packet 'kick
                               (encode-ping-response "61" "1.5.2" "MAMKU EBAL" "8" "32"))
                  (socket-stream socket))
  (error 'drop-connection
         :socket socket
         :message "Kicking client after ping request"))

(defpacket (client-settings #xCC) ((:string locale)
                                   (:byte view-distance)
                                   (:byte chat-settings)
                                   (:byte difficulty)
                                   (:bool show-cape))
  (format t "***CLIENT SETTINGS***~%Locale: ~A~%View distance: ~D~%Chat-settings: ~D~%Difficulty: ~D~%Show-cape: ~D~%"
          locale view-distance chat-settings difficulty show-cape))

(defpacket (plugin-message #xFA) ((:string channel) (:byte-array data))
  (format t "***PLUGIN MESSAGE***~%Channel: ~A~%Data: ~A~%"
          channel data))

(defpacket (kick #xFF) ((:string message)))

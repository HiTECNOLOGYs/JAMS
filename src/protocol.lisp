(in-package :jams)

(defun keep-alive-client (connection)
  (when (connection-keep-alive-received-p connection)
    (multiple-value-bind (packet id)
        (make-keep-alive-packet)
      (setf (connection-last-keep-alive-id connection) id
            (connection-keep-alive-received-p connection) nil)
      (send-data packet connection))))

(defpacket (keep-alive #x00) ((:integer id))
  (when (= (connection-last-keep-alive-id connection) id)
    (setf (connection-last-keep-alive-time connection) (get-universal-time)
          (connection-keep-alive-received-p connection) t)))

(defpacket (login-request #x01) ((:integer entity-id)
                                 (:string level-type)
                                 (:byte game-mode)
                                 (:byte dimension)
                                 (:byte difficulty)
                                 (:byte nothing) ; was used by vanilla server to set world height
                                 (:byte max-players)))

(defun send-login-packets (connection)
  "Sends packets required to log in."
  #+jams-debug (log-message :info "Sending data to client #~D"
                            (connection-id connection))
  (send-data (encode-packet 'login-request
                            '((:integer 228)
                              "default"
                              0
                              0
                              0
                              0
                              16))
             connection)
  (send-data (encode-packet 'spawn-position
                            '((:integer 0)
                              (:integer 0)
                              (:integer 0)))
             connection)
  (send-data (encode-packet 'player-position-and-look
                            '((:double 0.0)
                              (:double 0.0)
                              (:double 2.0)
                              (:double 0.0)
                              0.0
                              0.0
                              t))
             connection))

(defpacket (handshake #x02) ((:byte prot-id) (:string nick) (:string address) (:integer port))
  #+jams-debug
  (format t "***HANDSHAKE***~%Protocol ID: ~D~%Nickname: ~A~%Address: ~A~%Port: ~A~%"
          prot-id
          nick
          address
          port)
  (send-login-packets connection))

(defpacket (spawn-position #x06) ((:integer x) (:integer y) (:integer z)))

(defpacket (player #x0A) ((:bool on-ground?))
  #+jams-debug
  (format t "***PLAYER***~%On ground: ~A~%"
          on-ground?))

(defpacket (player-position #x0B) ((:double x) (:double y) (:double stance) (:double z) (:bool on-ground?))
  #+jams-debug
  (format t "***PLAYER POSITION***~%X: ~D~%Y: ~D~%Stance: ~D~%Z: ~D~%On ground: ~A~%"
          x y stance z on-ground?))

(defpacket (player-look #x0C) ((:float yaw) (:float pitch) (:bool on-ground?))
  #+jams-debug
  (format t "***PLAYER LOOK***~%Yaw: ~8$~%Pitch: ~8$~%On ground: ~A~%"
          yaw pitch on-ground?))

(defpacket (player-position-and-look #x0D) ((:double x)
                                            (:double y)
                                            (:double stance)
                                            (:double z)
                                            (:float yaw)
                                            (:float pitch)
                                            (:bool on-ground?))
  #+jams-debug
  (format t "***PLAYER POSITION AND LOOK***~%X: ~8$~%Y: ~8$~%Stance: ~8$~%Z: ~8$~%Yaw: ~4$~%Pitch: ~4$~%On gound: ~A~%"
          x y stance z yaw pitch on-ground?))

(defpacket (chunk-data #x33) ((:integer x) (:integer z) (:bool ground-up-continuous) ((:unsigned :short) )))

(defpacket (client-statuses #xCD) ((:byte payload))
  #+jams-debug
  (format t "***CLIENT STATUSES***Payload: ~D~%"
          payload))

(defpacket (ping #xFE) ((:byte magic))
  (declare (ignore magic)) ; assuming magic is always 1
  (send-data (make-packet 'kick
                          (encode-ping-response "61" "1.5.2" "MAMKU EBAL" "100" "32"))
             connection)
  (error 'Close-connection
         :connection connection
         :reason "Kicking client after ping request"))

(defpacket (client-settings #xCC) ((:string locale)
                                   (:byte view-distance)
                                   (:byte chat-settings)
                                   (:byte difficulty)
                                   (:bool show-cape))
  #+jams-debug
  (format t "***CLIENT SETTINGS***~%Locale: ~A~%View distance: ~D~%Chat-settings: ~D~%Difficulty: ~D~%Show-cape: ~D~%"
          locale view-distance chat-settings difficulty show-cape))

(defpacket (plugin-message #xFA) ((:string channel) ((:array :byte) data))
  #+jams-debug
  (format t "***PLUGIN MESSAGE***~%Channel: ~A~%Data: ~A~%"
          channel data))

(defpacket (kick #xFF) ((:string message))
  #+jams-debug (log-message :info "Terminating connection #~D. (~A)"
                            (connection-id connection) message)
  (terminate-connection connection))

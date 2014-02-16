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

(defpacket (handshake #x02) ((:byte prot-id)
                             (:string nick)
                             (:string address)
                             (:integer port))
  #+jams-debug (log-message :info "Player connected: ~A (protocol:~A) (~A:~D)"
                            nick prot-id address port)
  (send-data (encode-packet 'login-request
                            '((:integer 228)
                              "default"
                              0
                              0
                              0
                              0
                              16))
             connection)
  (setf (connection-client connection) (add-player *world* nick)
        (connection-termination-handler connection) #'(lambda (connection)
                                                        (declare (ignore connection))
                                                        (delete-player *world* nick)))
  (send-login-packets connection))

(defpacket (spawn-position #x06) ((:integer x) (:integer y) (:integer z)))

(defpacket (player #x0A) ((:bool on-ground?)))

(defpacket (player-position #x0B) ((:double x)
                                   (:double y)
                                   (:double stance)
                                   (:double z)
                                   (:bool on-ground?)))

(defpacket (player-look #x0C) ((:float yaw) (:float pitch) (:bool on-ground?)))

(defpacket (player-position-and-look #x0D) ((:double x)
                                            (:double y)
                                            (:double stance)
                                            (:double z)
                                            (:float yaw)
                                            (:float pitch)
                                            (:bool on-ground?)))

(defpacket (chunk-data #x33) ((:integer x)
                              (:integer z)
                              (:bool ground-up-continuous)
                              (:unsigned :short)))

(defpacket (client-statuses #xCD) ((:byte payload)))

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
                                   (:bool show-cape)))

(defpacket (plugin-message #xFA) ((:string channel) ((:array :byte) data)))

(defpacket (kick #xFF) ((:string message))
  #+jams-debug (log-message :info "Terminating connection #~D. (~A)"
                            (connection-id connection) message)
  (terminate-connection connection))

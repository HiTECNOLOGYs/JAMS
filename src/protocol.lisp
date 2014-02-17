(in-package :jams)

;;; Packets definitions

(defpacket (keep-alive #x00) ((:integer id)))

(defpacket (login-request #x01) ((:integer entity-id)
                                 (:string level-type)
                                 (:byte game-mode)
                                 (:byte dimension)
                                 (:byte difficulty)
                                 (:byte nothing) ; used to be world height
                                 (:byte max-players)))

(defpacket (handshake #x02) ((:byte protocol-id)
                             (:string nick)
                             (:string address)
                             (:integer port)))

(defpacket (time-update #x04) ((:long age-of-the-world)
                               (:long time-of-day)))

(defpacket (spawn-position #x06) ((:integer x)
                                  (:integer y)
                                  (:integer z)))

(defpacket (player #x0A) ((:bool on-ground?)))

(defpacket (player-position #x0B) ((:double x)
                                   (:double y)
                                   (:double stance)
                                   (:double z)
                                   (:bool on-ground?)))

(defpacket (player-look #x0C) ((:float yaw)
                               (:float pitch)
                               (:bool on-ground?)))

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
                              ((:unsigned :short) primary-bit-map)
                              ((:unsigned :short) add-bit-map)
                              (:integer compressed-size)
                              ((:array :byte) compressed-data)))

(defpacket (client-statuses #xCD) ((:byte payload)))

(defpacket (ping #xFE) ((:byte magic)))

(defpacket (client-settings #xCC) ((:string locale)
                                   (:byte view-distance)
                                   (:byte chat-settings)
                                   (:byte difficulty)
                                   (:bool show-cape)))

(defpacket (plugin-message #xFA) ((:string channel)
                                  ((:array :byte) data)))

(defpacket (kick #xFF) ((:string message)))


;;; Packets handlers

(defun get-players-count (world)
  (hash-table-count (world-players world)))

(defun keep-alive-client (connection)
  (when (connection-keep-alive-received-p connection)
    (multiple-value-bind (packet id)
        (make-keep-alive-packet)
      (setf (connection-last-keep-alive-id connection)    id
            (connection-keep-alive-received-p connection) nil)
      (send-data packet connection))))

(defun keep-alive (connection id)
  (when (= (connection-last-keep-alive-id connection) id)
    (setf (connection-last-keep-alive-time connection)  (get-universal-time)
          (connection-keep-alive-received-p connection) t)))

(defun player-position  (connection x y stance z on-ground?)
  (let ((player (connection-client connection)))
    (setf (x player)           x
          (y player)           y
          (z player)           z
          (stance player)      stance
          (on-ground-p player) on-ground?)))

(defun player-look (connection yaw pitch on-ground?)
  (let ((player (connection-client connection)))
    (setf (yaw player)         yaw
          (pitch player)       pitch
          (on-ground-p player) on-ground?)))

(defun handshake (connection protocol-id nick address port)
  #+jams-debug (log-message :info "Player connected: ~A (protocol:~A) (~A:~D)"
                            nick protocol-id address port)
  (when (<= +server-max-players+ (get-players-count *world*))
    (send-packet 'kick
                 connection
                 '("No slots available."))
    (error 'Close-connection
           :connection connection
           :reason "No slots available."))
  (send-data (encode-packet 'login-request
                            '((:integer 228)
                              "default"
                              0
                              0
                              0
                              0
                              16))
             connection)
  (let ((player (add-player *world* connection nick)))
    (setf (connection-status connection) :running
          (connection-client connection) player)
    (spawn-entity *world* player)))

(defun client-statuses (connection payload)
  (switch (payload :test #'=)
    (0 (spawn-entity *world* (connection-client connection)))
    (1 (respawn-entity *world* (connection-client connection)))))

(defun ping (connection magic)
  (declare (ignore magic)) ; assuming magic is always 1
  (let ((number-of-players (get-players-count *world*)))
    (send-data (make-packet 'kick
                            (encode-ping-response (write-to-string
                                                   +server-supported-protocol+)
                                                  +server-version+
                                                  +server-description+
                                                  (write-to-string number-of-players)
                                                  (write-to-string +server-max-players+)))
               connection))
  (error 'Close-connection
         :connection connection
         :reason "Kicking client after ping request"))

(defun kick (connection message)
  #+jams-debug (log-message :info "Terminating connection #~D. (~A)"
                            (connection-id connection) message)
  (error 'Close-connection
         :connection connection
         :reason message))

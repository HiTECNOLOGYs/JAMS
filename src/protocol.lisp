(in-package :jams)

;;; Packets definitions

(defpacket (keep-alive #x00)
           ((:integer id)))

(defpacket (login-request #x01)
           ((:integer entity-id)
            ((:exclude :length-prefix) level-type-length)
            (((:repeat level-type-length) :character) level-type)
            (:byte game-mode)
            (:byte dimension)
            (:byte difficulty)
            (:byte nothing) ; used to be world height
            (:byte max-players)))

(defpacket (handshake #x02)
           ((:byte protocol-id)
            ((:exclude :length-prefix) nick-length)
            (((:repeat nick-length) :character) nick)
            ((:exclude :length-prefix) address-length)
            (((:repeat address-length) :character) address)
            (:integer port)))

(defpacket (chat-message #x03)
           (((:exclude :prefix-length) message-length)
            (((:repeat message-length) :character) message)))

(defpacket (time-update #x04)
           ((:long age-of-the-world)
            (:long time-of-day)))

(defpacket (entity-equipment #x05)
           ((:int entity-id)
            (:short slot)
            (:slot item)))

(defpacket (spawn-position #x06)
           ((:integer x)
            (:integer y)
            (:integer z)))

(defpacket (use-entity #x07)
           ((:integer user)
            (:integer targer)
            (:bool mouse-button)))

(defpacket (update-health #x08)
           ((:short health)
            (:short food)
            (:float food-saturation)))

(defpacket (respawn #x09)
           ((:integer dimension)
            (:byte difficulty)
            (:byte game-mode)
            (:short world-height)
            ((:exclude :prefix-length) level-type-length)
            (((:repeat level-type-length) :character) level-type)))

(defpacket (player #x0A)
           ((:bool on-ground?)))

(defpacket (player-position #x0B)
           ((:double x)
            (:double y)
            (:double stance)
            (:double z)
            (:bool on-ground?)))

(defpacket (player-look #x0C)
           ((:float yaw)
            (:float pitch)
            (:bool on-ground?)))

(defpacket (player-position-and-look #x0D)
           ((:double x)
            (:double y)
            (:double stance)
            (:double z)
            (:float yaw)
            (:float pitch)
            (:bool on-ground?)))

(defpacket (player-digging #x0E)
           ((:byte status)
            (:integer x)
            (:byte y)
            (:integer z)
            (:byte face)))

(defpacket (player-block-placement #x0F)
           ((:integer x)
            ((:unsigned :byte) y)
            (:integer z)
            (:byte direction)
            (:slot held-item)
            (:byte cursor-x)
            (:byte cursor-y)
            (:byte cursor-z)))

(defpacket (held-item-change #x10)
           ((:short slot)))

(defpacket (use-bed #x11)
           ((:integer entity-id)
            (:byte foo) ; Unknown. Protocol description says that only 0 was observed.
            (:integer x)
            (:byte y)
            (:integer z)))

(defpacket (animation #x12)
           ((:integer entity-id)
            (:byte animation-id)))

(defpacket (entity-action #x13)
           ((:integer entity-id)
            (:byte action-id)))

(defpacket (spawn-named-entity #x14)
           ((:integer entity-id)
            ((:exclude :length-prefix) name-length)
            (((:repeat name-length) :character) name)
            (:integer x)
            (:integer y)
            (:integer z)
            (:byte yaw)
            (:byte pitch)
            (:short current-item)
            (:entity-metadata entity-metadata)))

(defpacket (collect-item #x16)
           ((:integer collector-entity-id)
            (:integer collected-entity-id)))

(defpacket (spawn-object #x17)
           ((:integer entity-id)
            (:byte type)
            (:integer x)
            (:integer y)
            (:integer z)
            (:byte pitch)
            (:byte yaw)
            (:object-data object-data)))

(defpacket (spawn-mob #x18)
           ((:integer entity-id)
            (:byte type)
            (:integer x)
            (:integer y)
            (:integer z)
            (:byte pitch)
            (:byte head-pitch)
            (:byte yaw)
            (:short velocity-x)
            (:short velocity-y)
            (:short velocity-z)
            (:entity-metadata metadata)))

(defpacket (spawn-painting #x19)
           ((:integer entity-id)
            ((:exclude :length-prefix) title-length)
            (:integer x)
            (:integer y)
            (:integer z)
            (:integer direction)))

(defpacket (spawn-experience-orb #x1A)
           ((:integer entity-id)
            (:integer x)
            (:integer y)
            (:integer z)
            (:short count)))

(defpacket (entity-velocity #x1C)
           ((:integer entity-id)
            (:short velocity-x)
            (:short velocity-y)
            (:short velocity-z)))

(defpacket (destroy-entity #x1D)
           (((:exclude :byte) count)
            (((:repeat count) :integer) entity-ids)))

(defpacket (entity #x1E)
           ((:integer entity-id)))

(defpacket (entity-relative-move #x1F)
           ((:integer entity-id)
            (:byte dx)
            (:byte dy)
            (:byte dz)))

(defpacket (entity-look #x20)
           ((:integer entity-id)
            (:byte yaw)
            (:byte pitch)))

(defpacket (entity-look-and-relative-move #x21)
           ((:integer entity-id)
            (:byte dx)
            (:byte dy)
            (:byte dz)
            (:byte yaw)
            (:byte pitch)))

(defpacket (entity-teleport #x22)
           ((:integer entity-id)
            (:integer x)
            (:integer y)
            (:integer z)
            (:byte yaw)
            (:byte pitch)))

(defpacket (entity-head-look #x23)
           ((:integer entity-id)
            (:byte yaw)))

(defpacket (entity-status #x26)
           ((:integer entity-id)
            (:byte status)))

(defpacket (attach-entity #x27)
           ((:integer entity-id)
            (:integer vehicle-id)))

(defpacket (entity-metadata #x28)
           ((:integer entity-id)
            (:entity-metadata metadata)))

(defpacket (entity-effect #x29)
           ((:integer entity-id)
            (:byte effect-id)
            (:byte amplifier)
            (:short duration)))

(defpacket (remove-entity-effect #x2A)
           ((:integer entity-id)
            (:byte effect-id)))

(defpacket (set-experience #x2B)
           ((:float experience-bar)
            (:short level)
            (:short total-experience)))

(defpacket (chunk-data #x33)
           ((:integer x)
            (:integer z)
            (:bool ground-up-continuous)
            ((:unsigned :short) primary-bit-map)
            ((:unsigned :short) add-bit-map)
            ((:exclude :integer) compressed-size)
            (((:repeat compressed-size) :byte) compressed-data)))

(defpacket (multi-block-change #x34)
           ((:integer chunk-x)
            (:integer chunk-z)
            ((:exclude :short) record-count)
            ((:exclude :integer) data-size)
            (((:repeat record-count) :block-change-record) changes)))

(defpacket (block-change #x35)
           ((:integer x)
            (:byte y)
            (:integer z)
            (:short block-type)
            (:byte block-metadata)))

(defpacket (block-action #x36)
           ((:integer x)
            (:short y)
            (:integer z)
            (:byte byte-1)
            (:byte byte-2)
            (:short block-id)))

(defpacket (block-break-animation #x37)
           ((:integer breaking-entity-id)
            (:integer x)
            (:integer y)
            (:integer z)
            (:byte destroy-stage)))

(defpacket (chunk-bulk #x38)
           ((:short chunks-column-count)
            ((:exclude :integer) data-length)
            (:bool sky-lit?)
            (((:repeat data-length) :byte) data)
            (((:repeat data-length) :chunk-bulk-metadata) metadata)))

(defpacket (explosion #x3C)
           ((:double x)
            (:double y)
            (:double z)
            (:float radius)
            ((:exclude :integer) record-count)
            (((:repeat record-count) :explosion-damage-record) records)
            (:float player-motion-x)
            (:float player-motion-y)
            (:float player-motion-z)))

(defpacket (sound-of-particle-effect #x3D)
           ((:integer effect-id)
            (:integer x)
            (:byte y)
            (:integer z)
            (:integer data)
            (:bool disable-relative-volume?)))

(defpacket (named-sound-effect #x3E)
           (((:exclude :length-prefix) sound-name-length)
            (((:repeat effect-name-length) :character) sound-name)
            (:integer x)
            (:integer y)
            (:integer z)
            (:float volume)
            (:byte pitch)))

(defpacket (particle #x3F)
           (((:exclude :length-prefix) particle-name-length)
            (((:repeat particle-name-length) :character) particle-name)
            (:float x)
            (:float y)
            (:float z)
            (:float offset-x)
            (:float offset-y)
            (:float offset-z)
            (:float particle-speed)
            (:integer number-of-particles)))

(defpacket (change-game-state #x46)
           ((:byte reason)
            (:byte game-mode)))

(defpacket (spawn-global-entity #x47)
           ((:integer entity-id)
            (:byte type)
            (:integer x)
            (:integer y)
            (:integer z)))

(defpacket (open-window #x64)
           ((:byte window-id)
            (:byte inventory-type)
            ((:exclude :length-prefix) window-title-length)
            (((:repeat window-title-length) :character) window-title)
            (:byte number-of-slots)
            (:bool use-provided-window-title?)))

(defpacket (close-window #x65)
           ((:byte window-id)))

(defpacket (click-window #x66)
           ((:byte window-id)
            (:short slot)
            (:byte button)
            (:short action-number)
            (:byte mode)
            (:slot clicked-item)))

(defpacket (set-slot #x67)
           ((:byte window-id)
            (:short slot)
            (:slot data)))

(defpacket (set-window-items #x68)
           ((:byte window-id)
            ((:exclude :short) count)
            (((:repeat count) :slot) slots)))

(defpacket (update-window-property #x69)
           ((:byte window-id)
            (:short property)
            (:short value)))

(defpacket (confirm-transaction #x6A)
           ((:byte window-id)
            (:short action-number)
            (:bool accepted?)))

(defpacket (creative-inventory-action #x6B)
           ((:short slot)
            (:slot clicked-item)))

(defpacket (ehchant-item #x6C)
           ((:byte window-id)
            (:byte enchantment)))

(defpacket (update-sign #x82)
           ((:integer x)
            (:short y)
            (:integer z)
            ((:exclude :length-prefix) line-1-length)
            (((:repeat line-1-length) :character) line-1)
            ((:exclude :length-prefix) line-2-length)
            (((:repeat line-1-length) :character) line-2)
            ((:exclude :length-prefix) line-3-length)
            (((:repeat line-1-length) :character) line-3)
            ((:exclude :length-prefix) line-4-length)
            (((:repeat line-1-length) :character) line-4)))

(defpacket (item-data #x83)
           ((:short item-type)
            (:short item-id)
            ((:exclude :short) text-length)
            (((:repeat text-length) :byte) text)))

(defpacket (update-title-entity #x84)
           ((:integer x)
            (:short y)
            (:integer z)
            (:byte action)
            ((:exclude :short) data-length)
            (((:repeat data-length) :byte) nbt-data)))

(defpacket (increment-statistic #xC8)
           ((:integer statistic-id)
            (:byte amount)))

(defpacket (player-list-item #xC9)
           (((:exclude :length-prefix) player-name-length)
            (((:repeat player-name-length) :character) player-name)
            (:bool online?)
            (:short ping)))

(defpacket (player-abilities #xCA)
           ((:byte flags)
            (:byte flying-speed)
            (:byte walking-speed)))

(defpacket (tab-complete #xCB)
           (((:exclude :length-prefix) text-length)
            (((:repeat text-length) :character) text)))

(defpacket (client-settings #xCC)
           (((:exclude :length-prefix) locale-length)
            (((:repeat locale-length) :character) locale)
            (:byte view-distance)
            (:byte chat-settings)
            (:byte difficulty)
            (:bool show-cape)))

(defpacket (client-statuses #xCD)
           ((:byte payload)))

(defpacket (scoreboard-objective #xCE)
           (((:exclude :length-prefix) objective-id-length)
            (((:repeat objective-id-length) :character) objective-id)
            ((:exclude :length-prefix) objective-display-name-length)
            (((:repeat objective-display-name-length) :character) objective-display-name)
            (:byte action)))

(defpacket (update-score #xCF)
           (((:exclude :length-prefix) item-name-length)
            (((:repeat item-name-length) :character) item-name)
            (:byte action)
            ((:exclude :length-prefix) objective-id-length)
            (((:repeat objective-id-length) :character) objective-id)
            (:integer value)))

(defpacket (display-scoreboard #xD0)
           ((:byte position)
            ((:exclude :length-prefix) scoreboard-id-length)
            (((:repeat scoreboard-id-length) :character) scoreboard-id)))

(defpacket (teams #xD1)
           (((:exclude :length-prefix) team-id-length)
            (((:repeat team-id-length) :character) team-id)
            (:byte mode)
            ((:exclude :length-prefix) team-display-name-length)
            (((:repeat team-display-name-length) :character) team-display-name)
            ((:exclude :length-prefix) team-prefix-length)
            (((:repeat team-prefix-length) :character) team-prefix)
            ((:exclude :length-prefix) team-suffix-length)
            (((:repeat team-suffix-length) :character) team-suffix)
            (:byte friendly-fire)
            ((:exclude :short) player-count)
            (((:repeat player-count) :string) player)))

(defpacket (plugin-message #xFA)
           (((:exclude :length-prefix) channel-name-length)
            (((:repeat channel-name-length) :character) channel)
            ((:exclude :length-prefix) data-length)
            (((:repeat data-length) :byte) data)))

(defpacket (encryption-key-response #xFC)
           (((:exclude :short) shared-secret-length)
            (((:repeat shared-secret-length) :byte) shared-secret)
            ((:exclude :short) verify-token-length)
            (((:repeat verify-token-length) :byte) verify-token-response)))

(defpacket (encryption-key-request #xFD)
           (((:exclude :length-prefix) server-id-length)
            (((:repeat server-id-length) :character) server-id)
            ((:exclude :short) public-key-length)
            (((:repeat public-key-length) :byte) public-key)
            ((:exclude :short) verify-token-length)
            (((:repeat verify-token-length) :byte) verify-token)))

(defpacket (ping #xFE)
           ((:byte magic)))

(defpacket (kick #xFF)
           (((:exclude :length-prefix) message-length)
            (((:repeat message-length) :character) message)))


;;; Packets handlers

(defun get-players-count (world)
  (iter (for (nickname player) in-hashtable (world-players world))
    (when (connection-running-p (player-connection player))
      (summing 1))))

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
  (when (<= *server-max-players* (get-players-count *world*))
    (send-packet 'kick
                 connection
                 '("No slots available.")))
  (let ((player (add-player *world* connection nick)))
    (send-packet 'login-request
                 connection
                 `((:integer ,(id player))
                   "default"
                   0
                   0
                   0
                   0
                   ,*server-max-players*))
    (setf (connection-status connection) :running
          (connection-client connection) player)
    (spawn-entity *world* player)))

(defun client-statuses (connection payload)
  (switch (payload :test #'=)
    (0 (spawn-entity *world* (connection-client connection)))
    (1 (respawn-entity *world* (connection-client connection)))))

(defun ping (connection magic)
  (declare (ignore magic)) ; assuming magic is always 1
  (let* ((number-of-players (get-players-count *world*))
         (packet (encode-ping-response (write-to-string *server-supported-protocol*)
                                       *server-version*
                                       *server-description*
                                       (write-to-string number-of-players)
                                       (write-to-string *server-max-players*))))
    (send-packet 'kick connection packet)))

(defun kick (connection message)
  #+jams-debug (log-message :info "Client kicked us. Terminating connection #~D. (~A)"
                            (connection-id connection) message)
  (terminate-connection connection message))

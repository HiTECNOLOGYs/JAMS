(in-package :jams)

(defgeneric use (world entity target)
  (:documentation "Called when entity uses world object."))
(defgeneric attack (world entity target)
  (:documentation "Called when entity tries to punch another entity or world object."))

(defgeneric right-mouse-button (world entity target))
(defgeneric left-mouse-button (world entity target))

(defgeneric move (world entity))


(defgeneric spawn-entity (world entity))
(defgeneric respawn-entity (world entity))

(defmethod spawn-entity ((world World) (player Player))
  (let* ((connection (player-connection player))
         (spawn-point (get-spawn-point *world*)))
    ;; TODO Send chunks here
    ;; TODO Send entities here
    (send-packet 'spawn-position
                 connection
                 (getf spawn-point :x)
                 (getf spawn-point :y)
                 (getf spawn-point :z))
    ;; TDOD Send inventory here
    (send-packet 'player-position-and-look
                 connection
                 (x player)
                 (y player)
                 (+ 2.0 (x player))
                 (z player)
                 (yaw player)
                 (pitch player)
                 t)
    ;; TODO Receive look+position packet sent from client here
    ;;      in order to verify spawn position.
    )
  (setf (spawned-p player) t))

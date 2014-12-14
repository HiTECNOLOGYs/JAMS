(in-package :jams)

(defgeneric use-object (world entity target)
  (:documentation "Called when entity uses world object."))

(defgeneric attack-object (world entity target)
  (:documentation "Called when entity tries to punch another entity or world object."))

(defmethod spawn-entity ((world World) (player Player))
  (with-slots (spawned? connection x y z yaw pitch on-ground?) player
    ;; TODO Send chunks here
    ;; TODO Send entities here
    (respond connection 'jams.packets.client:spawn-position
             :location (get-spawn-point *world*))
    ;; TDOD Send inventory here
    (respond connection 'jams.packets.client:player-position-and-look
             :x x
             :feet-y y
             :z z
             :yaw yaw
             :pitch pitch
             :on-ground? on-ground?)
    ;; TODO Receive look+position packet sent from client here
    ;;      in order to verify spawn position.
    (setf spawned? t)))

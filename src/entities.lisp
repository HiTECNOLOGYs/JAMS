(in-package :jams)

(defvar *next-entity-id* 0)

(defun gen-entity-id ()
  (prog1 *next-connection-id*
    (incf *next-connection-id*)))

(defclass Entity ()
  ((id :initform (gen-entity-id)
       :reader id)
   (x :initarg :x
      :accessor x)
   (y :initarg :y
      :accessor y)
   (z :initarg :z
      :accessor z)
   (yaw :initarg :yaw
        :initform 0.0
        :accessor yaw)
   (pitch :initarg :pitch
          :initform 0.0
          :accessor pitch)
   (on-ground? :initform t
               :accessor on-ground-p)
   (helath :initarg :health
           :initform 20
           :accessor health)
   (armor :initarg :armor
          :initform 0
          :accessor armor)
   (hunger :initarg :hunger
           :initform 20
           :accessor hunger)
   (spawned? :initform nil
             :accessor spawned-p)))

(defclass Zombie (Entity) ())

(defclass Skeleton (Entity) ())

(defclass Creeper (Entity) ())

(defclass Player (Entity)
  ((nickname :initarg :nickname
             :reader player-nickname)
   (connection :initarg :connection
               :accessor player-connection)
   (hands :initform 0
          :accessor player-hands)
   (inventory :initarg :inventory
              :initform nil
              :accessor inventory)))

(defmethod initialize-instance :after ((instance Player) &rest initargs)
  (declare (ignore initargs))
  (unless (notany (curry #'slot-boundp instance)
                  '(x y z))
    (let ((point (get-spawn-point *world*)))
      (setf (x instance) (getf point :x)
            (y instance) (getf point :y)
            (z instance) (getf point :z)))))

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
                 `((:integer ,(getf spawn-point :x))
                   (:integer ,(getf spawn-point :y))
                   (:integer ,(getf spawn-point :z))))
    ;; TDOD Send inventory here
    (send-packet 'player-position-and-look
                 connection
                 `((:double ,(x player))
                   (:double ,(y player))
                   (:double ,(+ 2.0 (x player)))
                   (:double ,(z player))
                   ,(yaw player)
                   ,(pitch player)
                   t))
    ;; TODO Receive look+position packet sent from client here
    ;;      in order to verify spawn position.
    )
  (setf (spawned-p player) t))

(in-package :jams)

(defclass Entity ()
  ((x :initarg :x
      :accessor x)
   (y :initarg :y
      :accessor y)
   (z :initarg :z
      :accessor z)
   (yaw :initarg :yaw
        :accessor pitch)
   (pitch :initarg :pitch
          :accessor pitch)
   (helath :initarg :health
           :initform 20
           :accessor health)
   (armor :initarg :armor
          :initform 0
          :accessor armor)
   (hunger :initarg :hunger
           :initform 20
           :accessor hunger)))

(defclass Zombie (Entity) ())

(defclass Skeleton (Entity) ())

(defclass Creeper (Entity) ())

(defclass Player (Entity)
  ((nickname :initarg :nickname
             :reader player-nickname)
   (hands :initform 0
          :accessor player-hands)
   (inventory :initarg :inventory
              :initform nil
              :accessor inventory)))

(defgeneric use (world entity target)
  (:documentation "Called when entity uses world object."))
(defgeneric attack (world entity target)
  (:documentation "Called when entity tries to punch another entity or world object."))

(defgeneric right-mouse-button (world entity target))
(defgeneric left-mouse-button (world entity target))

(defgeneric move (world entity))

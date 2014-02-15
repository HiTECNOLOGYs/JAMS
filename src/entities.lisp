(in-package :jams)

(defclass Entity ()
  ((x :initarg :x
      :accessor x)
   (y :initarg :y
      :accessor y)
   (z :initarg :z
      :accessor z)
   (helath :initarg :health
           :initform 20
           :accessor health)
   (armor :initarg :armor
          :initform 0
          :accessor armor)
   (hunger :initarg :hunger
           :initform 20
           :accessor hunger)
   (inventory :initarg :inventory
              :initform nil
              :accessor inventory)))

(defclass Zombie (Entity) ())

(defclass Skeleton (Entity) ())

(defclass Creeper (Entity) ())

(defclass Player (Entity) ())

(defgeneric use (world entity target)
  (:documentation "Called when entity uses world object."))
(defgeneric attack (world entity target)
  (:documentation "Called when entity tries to punch another entity or world object."))

(defgeneric right-mouse-button (world entity target))
(defgeneric left-mouse-button (world entity target))

(defgeneric move (world entity))

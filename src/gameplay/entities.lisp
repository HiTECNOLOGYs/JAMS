(in-package :jams)

(defvar *next-entity-id* 0)

(defun gen-entity-id ()
  (prog1 *next-entity-id*
    (incf *next-entity-id*)))

(defclass Entity ()
  ((id :initform (gen-entity-id)
       :reader id)
   (x :initarg :x
      :accessor x)
   (y :initarg :y
      :accessor y)
   (z :initarg :z
      :accessor z)
   (stance :initarg :stance
           :accessor stance)
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
  (when (notany (curry #'slot-boundp instance) '(x y z))
    (let ((point (get-spawn-point *world*)))
      (setf (x instance) (getf point :x)
            (y instance) (getf point :y)
            (z instance) (getf point :z)))))

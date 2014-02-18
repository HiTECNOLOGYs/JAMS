(in-package :jams)

(defclass Object ()
  ((id :initarg :id
       :initform 0
       :accessor object-id)
   (add-id :initarg :add-id
           :initform 0
           :accessor object-add-id)
   (damage-value :initarg :damage-value
                 :initform 0
                 :accessor object-damage-value)
   (metadata :initarg :metadata
             :accessor metadata))
  (:documentation "To be specific, I need to say that `damage-value' is what is usually called metadata in some implementations and `metadata' is things like inventory or something. Just keep this in mind."))

(defclass World-object (Object)
  ((light-level :initarg :light-level
                :initform 15
                :accessor world-object-light-level)
   (sky-light :initarg :sky-light
              :initform 15
              :accessor world-object-sky-light)))

(defclass Item (Object)
  ((x :initarg :x
      :accessor item-x)
   (y :initarg :y
      :accessor item-y)
   (z :initarg :z
      :accessor item-z)))

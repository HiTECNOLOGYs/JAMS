(in-package :jams)

(define-constant +chunk-size+ (* 16 16 16))
(define-constant +biomes-grid-size+ (* 16 16))

(defparameter *biomes*
  '((:taiga .    0)
    (:woodland . 1)))

(defun get-biome-value (biome-id)
  (cdr (assoc biome-id *biomes*)))

(defun store-data (data place)
  (store data
         place))

(defun restore-data (place)
  (restore place))

(defclass Object ()
  ((id :initarg :id
       :initform 0
       :accessor id)
   (add-id :initarg :add-id
           :initform 0
           :accessor add-id)
   (damage-value :initarg :damage-value
                 :initform 0
                 :accessor damage-value)
   (metadata :initarg :metadata
             :accessor metadata))
  (:documentation "To be specific, I need to say that `damage-value' is what is usually called metadata in some implementations and `metadata' is things like inventory or something. Just keep this in mind."))

(defclass Cube (Object)
  ((light-level :initarg :light-level
                :initform 15
                :accessor light-level)
   (sky-light :initarg :sky-light
              :initform 15
              :accessor sky-light)))

(defclass Item (Object)
  ((x :initarg :x
      :accessor x)
   (y :initarg :y
      :accessor y)
   (z :initarg :z
      :accessor z)))

(defclass Entity (Object)
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


(defclass Chunk ()
  ((x :initarg :x
      :accessor x)
   (y :initarg :y
      :accessor y)
   (bitmask :accessor bitmask
            :type '(unsigned-byte 16))
   (sky-lit? :initarg :sky-lit
             :initform t
             :accessor sky-lit-p)
   (items :initarg :items
          :initform nil
          :type '(list Iterm)
          :accessor items)
   (blocks :initarg blocks
           :initform (make-array (list 16 16 16)
                                 :element-type 'Cube
                                 :initial-element (make-instance 'Cube))
           :accessor blocks)
   (biomes :initarg :biomes
           :initform (make-array (list 16 16)
                                 :initial-element :taiga)
           :accessor biomes)))

(defclass Chunks-column ()
  ((id :initarg :id
       :accessor id)
   (z :initarg :z
      :accessor z)
   (x :initarg :x
      :accessor x)
   (chunks :initarg :chunks
           :initform (make-array (list 16)
                                 :initial-element (make-instance 'Chunk))
           :accessor chunks)))

(defun chunk-layer-empty-p (chunk layer)
  (let ((blocks (blocks chunk)))
    (dotimes (z 16)
      (dotimes (x 16)
        (when (aref blocks layer x z)
          (return-from chunk-layer-empty-p)))))
  t)

(defun calculate-bitmask (chunk)
  (iter (for layer below 16)
    (summing
     (if (chunk-layer-empty-p chunk layer)
       (ash 1 layer)
       0))))

(defmethod initialize-instance :after ((instance Chunk) &rest initargs)
  (declare (ignore initargs))
  (unless (slot-boundp instance 'bitmask)
    (setf (bitmask instance) (calculate-bitmask instance))))

(defun mapchunk (chunk chunk-slot function &optional step)
  (let ((blocks (slot-value chunk chunk-slot))
        (result (make-array 0
                            :adjustable t
                            :fill-pointer 0
                            :element-type '(unsigned-byte 8))))
    (doarray (blocks counter block :increment step)
      (vector-push-extend (funcall function blocks block counter)
                          result))
    result))

(defun get-chunk-blocks-id (chunk)
  (mapchunk chunk 'blocks
            #'(lambda (blocks block counter)
                (declare (ignore blocks counter))
                (ldb (byte 8 0) (id block)))))

(defun get-chunk-blocks-damage-value (chunk)
  (mapchunk chunk 'blocks
            #'(lambda (blocks block counter)
                (logior (ldb (byte 4 0)
                             (damage-value block))
                        (ldb (byte 4 4)
                             (damage-value (row-major-aref blocks (1+ counter))))))))

(defun get-chunk-blocks-light-level (chunk)
  (mapchunk chunk 'blocks
            #'(lambda (blocks block counter)
                (declare (ignore blocks counter))
                (light-level block))))

(defun get-chunk-blocks-sky-light (chunk)
  (mapchunk chunk 'blocks
            #'(lambda (blocks block counter)
                (declare (ignore blocks counter))
                (if (sky-light block)
                  1
                  0))))

(defun get-chunk-blocks-add-id (chunk)
  (mapchunk chunk 'blocks
            #'(lambda (blocks block counter)
                (logior (ldb (byte 4 8) (id block))
                        (ash (ldb (byte 4 8)
                                  (id (row-major-aref blocks (1+ counter))))
                             4)))))

(defun get-chunk-biomes (chunk)
  (mapchunk chunk 'biomes
            #'(lambda (biomes biome counter)
                (declare (ignore biomes counter))
                (get-biome-value biome))))

(defgeneric pack (object))

(defmethod pack ((chunk Chunk))
  (concatenate '(simple-array (unsigned-byte 8) (*))
               (get-chunk-blocks-id chunk)
               (get-chunk-blocks-damage-value chunk)
               (get-chunk-blocks-light-level chunk)
               (if (sky-lit-p chunk)
                 (get-chunk-blocks-sky-light chunk)
                 #())
               (get-chunk-blocks-add-id chunk)
               (get-chunk-biomes chunk)))

(defmethod pack :around ((chunk Chunk))
  (salza2:compress-data (call-next-method)
                        'salza2:deflate-compressor))

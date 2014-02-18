(in-package :jams)

;;; Vars

(defparameter *biomes*
  '((:taiga .    0)
    (:woodland . 1)))


;;; Calsses definitions

(defclass Chunk ()
  ((x :initarg :x
      :accessor x)
   (z :initarg :z
      :accessor z)
   (id :initarg :id
       :accessor id)
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
                                 :element-type 'World-object
                                 :initial-element (make-instance 'World-object))
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
           :accessor chunks)))

(defmethod initialize-instance ((instance Chunks-column) &rest initargs)
  (declare (ignore initargs))
  (unless (slot-boundp instance 'chunks)
    (setf (chunks instance)
          (make-array 16
                      :element-type 'Chunk
                      :initial-element (make-instance 'Chunk
                                                      :x (x instance)
                                                      :z (z instance))))))

(defclass World ()
  ((name :initarg :name
         :accessor world-name)
   (description :initarg :description
                :accessor world-description)
   (age :initarg :age
        :initform 0
        :accessor world-age)
   (time-of-day :initarg :time-of-day
                :initform 0
                :accessor world-time-of-day)
   (map :initarg :map
        :initform (make-hash-table :test 'equal)
        :accessor world-map)
   (spawn-point :initarg :spawn-point
                :documentation "(X Y Z)"
                :accessor world-spawn-point)
   (players :initform (make-hash-table :test 'equal)
            :accessor world-players)
   (entities :initarg :entities
             :initform (make-hash-table :test 'equal)
             :accessor world-entities)))

(defmethod initialize-instance :after ((instance Chunk) &rest initargs)
  (declare (ignore initargs))
  (unless (slot-boundp instance 'bitmask)
    (setf (bitmask instance) (calculate-bitmask instance))))


;;; Biomes

(defun get-biome-value (biome-id)
  (cdr (assoc biome-id *biomes*)))


;;; Data (re)storing

(defun store-data (data place)
  (store data
         place))

(defun restore-data (place)
  (restore place))


;;; Chunks

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


;;; World

(defun world-chunks-column (world x z)
  (gethash (list x z) (world-map world)))

(defun (setf world-chunks-column) (new-value world x z)
  (setf (gethash (list x z) (world-map world)) new-value))

(defun world-player (world player-nickname)
  (gethash player-nickname (world-players world)))

(defun (setf world-player) (new-value world player-nickname)
  (setf (gethash player-nickname (world-players world))
        new-value))

(defun add-player (world connection player-nickname)
  (multiple-value-bind (player exists?)
      (world-player world player-nickname)
    (if exists?
      (setf (player-connection player)
            connection)
      (setf (world-player world player-nickname)
            (make-instance 'Player
                           :nickname player-nickname
                           :connection connection)))))

(defun delete-player (world player-nickname)
  (remhash player-nickname (world-players world)))

(defun get-spawn-point (world)
  (let ((point (world-spawn-point world)))
    (list :x (first point)
          :y (second point)
          :z (third point))))

(defun get-region (world x1 z1 x2 z2)
  (iter (for x from x1 to x2)
    (appending (iter (for z from z1 to z2)
                 (collecting (cons (list x z) (world-chunks-column world x z)))))))


;;; Packing data for sending it over wires

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

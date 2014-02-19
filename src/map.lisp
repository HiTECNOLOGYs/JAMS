(in-package :jams)

;;; Vars

(defparameter *biomes*
  '((:taiga .    0)
    (:woodland . 1)))


;;; Calsses definitions

(defclass Chunk ()
  ((x :initarg :x
      :accessor chunk-x)
   (z :initarg :z
      :accessor chunk-z)
   (id :initarg :id
       :accessor chunk-id)
   (sky-lit? :initarg :sky-lit
             :initform t
             :accessor chunk-sky-lit-p)
   (items :initarg :items
          :initform nil
          :accessor chunk-items)
   (blocks :initarg blocks
           :initform (make-array (list 16 16 16)
                                 :element-type 'World-object
                                 :initial-element (make-instance 'World-object))
           :accessor chunk-blocks)
   (biomes :initarg :biomes
           :initform (make-array (list 16 16)
                                 :initial-element :taiga)
           :accessor chunk-biomes)))

(defclass Chunks-column ()
  ((id :initarg :id
       :accessor chunks-column-id)
   (bitmask :accessor chunks-column-bitmask
            :type '(unsigned-byte 16))
   (z :initarg :z
      :accessor chunks-column-z)
   (x :initarg :x
      :accessor chunks-column-x)
   (chunks :initarg :chunks
           :accessor chunks-column-chunks)))

(defmethod initialize-instance :after ((instance Chunks-column) &rest initargs)
  (declare (ignore initargs))
  (unless (slot-boundp instance 'bitmask)
    (setf (chunks-column-bitmask instance) (calculate-bitmask instance)))
  (unless (slot-boundp instance 'chunks)
    (setf (chunks-column-chunks instance)
          (make-array 16
                      :element-type 'Chunk
                      :initial-element (make-instance 'Chunk
                                                      :x (chunks-column-x instance)
                                                      :z (chunks-column-z instance))))))

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

(defclass Region ()
  ((x1 :initarg :x1
       :accessor region-x1)
   (z1 :initarg :z1
       :accessor region-z1)
   (x2 :initarg :x2
       :accessor region-x2)
   (z2 :initarg :z2
       :accessor region-z2)
   (chunks-columns :initargs :chunks-columns
                   :accessor region-chunks-columns)))

(defun get-region (world x1 z1 x2 z2)
  (iter (for x from x1 to x2)
    (appending (iter (for z from z1 to z2)
                 (collecting (cons (list x z) (world-chunks-column world x z)))))))

(defmethod initialize-instance :after ((instance Region) &key world)
  (unless (slot-boundp instance 'chunks-columns)
    (setf (region-chunks-columns instance)
          (get-region world
                      (region-x1 instance) (region-z1 instance)
                      (region-x2 instance) (region-z2 instance)))))


;;; Biomes

(defun get-biome-value (biome-id)
  (cdr (assoc biome-id *biomes*)))


;;; Data (re)storing

(defun store-data (data place)
  (store data place))

(defun restore-data (place)
  (restore place))


;;; Chunks column

(defun chunk-empty-p (chunk)
  (let ((blocks (chunk-blocks chunk)))
    (dotimes (i (array-total-size blocks))
      (when (row-major-aref blocks i)
        (return-from chunk-empty-p))))
  t)

(defun calculate-bitmask (chunks-column)
  (iter (for chunk in-vector (chunks-column-chunks chunks-column))
    (for i from 0)
    (summing (if (chunk-empty-p chunk)
               (ash 1 i)
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

(defun view-distance->region (world center-x center-z)
  (make-instance 'Region
                 :world world
                 :x1 (- center-x *view-distance*) :z1 (- center-z *view-distance*)
                 :x2 (+ center-z *view-distance*) :z2 (+ center-z *view-distance*)))


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
                (ldb (byte 8 0) (object-id block)))))

(defun get-chunk-blocks-damage-value (chunk)
  (mapchunk chunk 'blocks
            #'(lambda (blocks block counter)
                (logior (ldb (byte 4 0)
                             (object-damage-value block))
                        (ldb (byte 4 4)
                             (object-damage-value (row-major-aref blocks (1+ counter))))))))

(defun get-chunk-blocks-light-level (chunk)
  (mapchunk chunk 'blocks
            #'(lambda (blocks block counter)
                (declare (ignore blocks counter))
                (world-object-light-level block))))

(defun get-chunk-blocks-sky-light (chunk)
  (mapchunk chunk 'blocks
            #'(lambda (blocks block counter)
                (declare (ignore blocks counter))
                (world-object-sky-light block))))

(defun get-chunk-blocks-add-id (chunk)
  (mapchunk chunk 'blocks
            #'(lambda (blocks block counter)
                (logior (ldb (byte 4 8) (object-id block))
                        (ash (ldb (byte 4 8)
                                  (object-id (row-major-aref blocks (1+ counter))))
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
               (if (chunk-sky-lit-p chunk)
                 (get-chunk-blocks-sky-light chunk)
                 #())
               (get-chunk-blocks-add-id chunk)
               (get-chunk-biomes chunk)))

(defmethod pack ((column Chunks-column))
  (map 'list #'pack (chunks-column-chunks column)))

(defmethod pack ((region Region))
  (iter (for (position . chunks-column) in (region-chunks-columns region))
    (when chunks-column
      (collecting (pack chunks-column)))))

(defmethod pack :around ((chunk Chunk))
  (salza2:compress-data (call-next-method)
                        'salza2:deflate-compressor))

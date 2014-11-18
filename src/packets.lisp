(in-package :jams)

;;; **************************************************************************
;;;  Basics
;;; **************************************************************************

;; ----------------
;; ID -> Class name

(defparameter *packets* (make-hash-table)
  "Stores (ID -> Class name) table.")

(defun get-packet-name (id)
  (gethash id *packets*))

(defun (setf get-packet-name) (new-value id)
  (setf (gethash id *packets*) new-value))

;; ----------------
;; MOP

(defclass Packet (standard-class)
  ((id :initarg :id
       :reader packet-id)
   (name :initarg :name
         :reader packet-name)
   (stage :initarg :stage
          :reader packet-stage)
   (size :initarg :size
         :initform nil
         :documentation "Some packets have fixed size. In order to reduce amount of work, this size can be provided here. Otherwise, if NIL is stored in this slot, size is computated dynamically (when reading packet). DEFPACKET macro defines if packet size is static automatically, so not extra care is needed."
         :reader packet-size)
   ;; Isn't necessary but can be useful for debugging and statistics.
   (description :initarg :description
                :reader packet-description)
   (category :initarg :category
             :reader packet-category)
   (bound-to :initarg :bound-to
             :reader packet-bound-to)))

(defmethod validate-superclass ((class Packet) (superclass standard-class))
  t)

(defmethod shared-initialize :after ((class Packet) slot-names
                                     &key id stage description category bound-to)
  (setf (slot-value class 'id)          (first id)
        (slot-value class 'name)        (class-name class)
        (slot-value class 'stage)       (first stage)
        (slot-value class 'description) (first description)
        (slot-value class 'category)    (first category)
        (slot-value class 'bound-to)    (first bound-to)))

(defclass packet-field (standard-direct-slot-definition)
  ((type :initarg :type
         :reader packet-field-type)
   (id :initarg :id
       :reader packet-field-id)
   ;; Not necessary but, again, useful for debugging and not forgetting what the hell is that.
   (documentation :initarg :documentation
                  :reader packet-field-documentation)))

(defmethod shared-initialize :after ((slot packet-field) slot-names
                              &key type id documentation &allow-other-keys)
  (setf (slot-value slot 'type)          type
        (slot-value slot 'id)            id
        (slot-value slot 'documentation) documentation))

(defmethod direct-slot-definition-class ((class Packet) &key type id &allow-other-keys)
  "Slots that have both :ID and :TYPE attributes are considered to be packet fields
(or slots, whichever you like better; I'll stick to slots for the sake of simplicity,
except for cases when slot means class's slot, then I'll use field)."
  (if (and type id)
    (find-class 'packet-field)
    (call-next-method)))

(defparameter *direct-packet-slot* nil
  "Bound to direct slot when slot is packet field.")

(defclass effective-packet-slot (standard-effective-slot-definition)
  ((direct-slot :initform *direct-packet-slot* :reader packet-slot-direct-slot)))

(defmethod compute-effective-slot-definition ((class Packet) name direct-slot-definitions)
  (flet
    ((field-p (slot)
       (typep slot 'packet-field)))
    (let ((*direct-packet-slot* (find-if #'field-p direct-slot-definitions)))
      (when (and *direct-packet-slot*
                 (not (every #'field-p direct-slot-definitions)))
        (error "Slot ~S (that belongs to class ~S) can't possibly be packet field and class slot at the same time." name class))
      (call-next-method))))

(defmethod effective-slot-definition-class ((class Packet) &rest initargs)
  (declare (ignore initargs))
  (if *direct-packet-slot*
    (find-class 'effective-packet-slot)
    (call-next-method)))

(defun make-packet (class &rest fields)
  (let ((instance (make-instance class)))
    (loop for slot in (class-direct-slots (find-class class))
          for field in fields
          doing (setf (slot-value instance (slot-definition-name slot)) field))
    instance))

;; ----------------
;; Macros

(defmacro defpacket ((id name stage) &body body)
  "Structure is a list of (type name)."
  (let (metaclass-args fields)
    (loop for (expr . rest) on body
          while (and (listp expr) (keywordp (first expr)))
          doing (push expr metaclass-args)
          finally (setf fields rest))
    (unless metaclass-args
      (setf fields body))
    `(progn
       (setf (get-packet-name ,id) ',name)
       (defclass ,name ()
         (,@fields)
         (:metaclass Packet)
         (:id ,id)
         (:stage ,stage)
         ,@metaclass-args)
       ',name)))

;;; **************************************************************************
;;;  Packing data and building packets
;;; **************************************************************************

(defgeneric pack (object))

(defun encode-ping-response (protocol-version server-version motd player-count max-players)
  (concatenate 'vector
               (encode-value (+ 3 ; header (0xA7 0x31) + null byte
                                4 ; separators (null bytes)
                                (length protocol-version)
                                (length server-version)
                                (length motd)
                                (length player-count)
                                (length max-players))
                             :short)
               #(#x00 #xA7 #x00 #x31 #x00 #x00)
               (encode-data protocol-version :raw)
               #(#x00 #x00)
               (encode-data server-version :raw)
               #(#x00 #x00)
               (encode-data motd :raw)
               #(#x00 #x00)
               (encode-data player-count :raw)
               #(#x00 #x00)
               (encode-data max-players :raw)))

(defun encode-packet-data (name data)
  (iter
    (with result = #())
    (for (field-type field-name) in (packet-definition-structure name))
    (for field-data in data)
    (setf result
          (concatenate 'vector result (encode-value field-data field-type)))
    (finally (return result))))

(defun make-packet (name data)
  (let ((packet-id (packet-definition-id name)))
    (concatenate 'vector
                 (vector packet-id)
                 data)))

(defun encode-packet (name &rest data)
  (make-packet name (encode-packet-data name data)))

(defun make-keep-alive-packet ()
  (let ((id (random (1- (ash 2 (1- (* 8 (binary-type-size (get-type 'u4)))))))))
    (values (encode-packet 'keep-alive id)
            id)))

(defun send-packet (name connection &rest data)
  (send-data (apply #'encode-packet name data)
             connection))

;;; **************************************************************************
;;;  Packets reader
;;; **************************************************************************

;; Field readers

(defgeneric read-field (bindings data-stream type))

(defmethod read-field (bindings data-stream (type Basic-type))
  (decode-data type (read-bytes data-stream (binary-type-size type))))

(defmethod read-field (bindings data-stream (type Composite-type))
  (iter (for field in (composite-type-structure type))
        (for field-length next (binary-type-size field))
        (collecting (read-field bindings data-stream (get-type field)))))

(defmethod read-field (bindings data-stream (type String))
  (let* ((prefix-var (getf (binary-type-modifiers type) :length))
         (prefix (if prefix-var
                   (assoc prefix-var bindings)
                   (read-field bindings data-stream (get-type 'length-prefix)))))
    (iter (repeat prefix)
      (collecting (read-field bindings data-stream (get-type 'char))))))

;; High-level wrappers

(defun read-typedef (bindings data-stream type)
  (let ((exclude-from-result? (getf (binary-type-modifiers type) :exclude)))
    (values (read-field bindings data-stream type)
            (when exclude-from-result?
              t))))

(defun read-packet (vector)
  (let* ((packet-id (svref vector 0))
         (packet-data (subseq vector 1))
         (packet-structure (packet-definition-structure
                             (get-packet-name packet-id))))
    (with-input-from-sequence (data-stream packet-data)
      (iter
        (with bindings)
        (for (field name) in packet-structure)
        (for (data exclude-from-result?)
             next (multiple-value-list (read-typedef bindings data-stream field)))
        (push (cons name data) bindings)
        (unless exclude-from-result?
          (collect data into result))
        (finally (return (values (cons packet-id result)
                                 bindings)))))))

;;; **************************************************************************
;;;  Packets processing
;;; **************************************************************************

(defun process-packet (connection vector)
  (destructuring-bind (packet-id . packet-data)
      (read-packet vector)
    (let ((packet-processor (get-packet-name packet-id)))
      (if packet-processor
        (when (fboundp packet-processor)
          (apply packet-processor connection packet-data))
        (error 'Invalid-packet
               :message "Dunno what is this shit."
               :connection connection
               :data packet-id)))))

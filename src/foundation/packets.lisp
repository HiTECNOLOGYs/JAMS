(in-package :jams)

;;; **************************************************************************
;;;  Basics
;;; **************************************************************************

;; ----------------
;; ID -> Class name

(defparameter *packets* (make-hash-table :test 'equal)
  "Stores (ID + stage -> Class) table.")

(defun get-packet-class (id stage)
  (gethash (cons id stage) *packets*))

(defun (setf get-packet-class) (new-value id stage)
  (setf (gethash (cons id stage) *packets*) new-value))

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
  (declare (ignore class))
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
  (declare (ignore class initargs))
  (if *direct-packet-slot*
    (find-class 'effective-packet-slot)
    (call-next-method)))

(defun make-packet (class &rest fields)
  (let ((instance (make-instance class))
        (data (copy-list fields)))
    (dolist (slot (class-slots class) instance)
      (when (typep slot 'effective-packet-slot)
        (setf (slot-value instance (slot-definition-name slot)) (pop data))))))

(defun packet-structure (class)
  (iter (for slot in (class-direct-slots class))
        (collecting (list (packet-field-id slot) (packet-field-type slot)))))

;; ----------------
;; Some mixins

(defclass Encodable-packet () ())

;; ----------------
;; Macros

(defmacro defpacket ((id name stage) &body body)
  "Structure is a list of (type name)."
  (let (metaclass-args fields)
    (loop for (expr . rest) on body
          while (and (listp expr) (keywordp (first expr)))
          doing (push expr metaclass-args)
          finally (setf fields (cons expr rest)))
    (unless metaclass-args
      (setf fields body))
    `(progn
       (defclass ,name (Encodable-packet)
         (,@fields)
         (:metaclass Packet)
         (:id ,id)
         (:stage ,stage)
         ,@metaclass-args)
       (setf (get-packet-class ,id ,stage) (find-class ',name))
       ',name)))

;;; **************************************************************************
;;;  Packets decoding
;;; **************************************************************************

(define-condition Invalid-packet (error)
  ((message :initarg :message)
   (data :initarg :data
         :initform nil)
   (connection :initarg :connection)))

;;; **************************************************************************
;;;  Packets encoding
;;; **************************************************************************

(defun encode-packet (id &rest data)
  (with-output-to-sequence (output-stream)
    (when-let (class (get-packet-class id))
      (with-output-to-sequence (stream)
        (iter
          (for field in (class-direct-slots class))
          (for value in data)
          (write-binary-type (packet-field-type field) value output-stream))))))

(defun send-packet (connection id &rest data)
  (send-data (apply #'encode-packet id data)
             connection))

;;; **************************************************************************
;;;  Packets reader
;;; **************************************************************************

(defun read-packet (vector)
  (with-input-from-sequence (data-stream vector)
    (let* ((packet-class (get-packet-class (read-binary-type 'Var-Int data-stream)))
           (packet-structure (packet-structure packet-class)))
      (apply #'make-packet packet-class
             (mapcar #'(lambda (field)
                         (read-binary-type (packet-field-type field) data-stream))
                     packet-structure)))))

;;; **************************************************************************
;;;  Packets handling
;;; **************************************************************************

(defgeneric handle-packet (connection packet)
  (:documentation "Does packet processing. The result of this function call is sent to client. If more than one packets are needed to be sent, list should be returned."))

(defmacro define-packet-handler (name &body body)
  "Defines new handler for packet with given name. Also defines some handy local-bound functions to interact with connection and client and binds packet fields to appropriate symbols for easy access."
  `(defmethod handle-packet ((connection Connection) (packet ,name))
     (flet
       ((respond (id &rest data)
          ;; Sends data to client
          )
        (set-status (id &rest data)
          ;; Updates connection status
          )
        (drop ()
          ;; Drops connection immediately, aborts handler
          )
        (client ()
          ;; Returns instance of player who established this connection
          ))
       (with-slots ,(mapcar #'slot-definition-name
                      (class-direct-slots (find-class name)))
           packet
         ,@body))))

(defmethod handle-packet ((connection Connection) packet)
  (error 'Invalid-packet
         :message "Dunno what is this shit."
         :connection connection
         :data packet))

(defun process-packet (connection vector)
  (handle-packet connection (read-packet vector)))

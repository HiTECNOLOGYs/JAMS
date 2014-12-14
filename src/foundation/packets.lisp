(in-package :jams)

;;; **************************************************************************
;;;  Basics
;;; **************************************************************************

;; ----------------
;; ID -> Class name

(defparameter *packets* (make-hash-table :test 'equal)
  "Stores (ID + stage -> Class) table.")

(defun get-packet-class (id stage)
  (gethash (list id stage) *packets*))

(defun (setf get-packet-class) (new-value id stage)
  (setf (gethash (list id stage) *packets*) new-value))

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
             :reader packet-category)))

(defmethod validate-superclass ((class Packet) (superclass standard-class))
  t)

(defmethod shared-initialize :after ((class Packet) slot-names
                                     &key id stage description category)
  (setf (slot-value class 'id)          (first id)
        (slot-value class 'name)        (class-name class)
        (slot-value class 'stage)       (first stage)
        (slot-value class 'description) (first description)
        (slot-value class 'category)    (first category)))

(defclass packet-field (standard-direct-slot-definition)
  ((type :initarg :type
         :reader packet-field-type)
   ;; Not necessary but, again, useful for debugging and not forgetting what the hell is that.
   (documentation :initarg :documentation
                  :reader packet-field-documentation)))

(defmethod shared-initialize :after ((slot packet-field) slot-names
                              &key type documentation &allow-other-keys)
  (setf (slot-value slot 'type)          type
        (slot-value slot 'documentation) documentation))

(defmethod direct-slot-definition-class ((class Packet) &key type &allow-other-keys)
  "Slots that have both :ID and :TYPE attributes are considered to be packet fields
(or slots, whichever you like better; I'll stick to slots for the sake of simplicity,
except for cases when slot means class's slot, then I'll use field)."
  (declare (ignore class))
  (if type
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

(defun make-empty-packet (class)
  (make-instance class))

(defun make-packet (class &rest initargs)
  (apply #'make-instance class initargs))

(defmacro dopacket ((field-variable packet) &body body)
  `(dolist (,field-variable (class-direct-slots (class-of ,packet)) ,packet)
     ,@body))

;; ----------------
;; Macros

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun find-packet-package (bound-to)
    (case bound-to
      (:client (find-package :jams.packets.client))
      (:server (find-package :jams.packets.server))
      (otherwise (error "Don't know how to bind something to: ~S" bound-to)))))

(defmacro defpacket ((id name stage bound-to) &body body)
  "Structure is a list of (type name)."
  (let (metaclass-args fields
        (package (find-packet-package bound-to)))
    (loop for (expr . rest) on body
          while (and (listp expr) (keywordp (first expr)))
          doing (push expr metaclass-args)
          finally (setf fields (if (and (listp expr) (keywordp (first expr)))
                                 rest
                                 (cons expr rest))))
    (unless metaclass-args
      (setf fields body))
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (defclass ,#1=(intern (symbol-name name) package) ()
         (,@fields)
         (:metaclass Packet)
         (:id ,id)
         (:stage ,stage)
         ,@metaclass-args)
       (export ',#1# ,package)
       ,(when (eql bound-to :server)
          `(setf (get-packet-class ,id ,stage) (find-class ',#1#)))
       ',#1#)))

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

(defun encode-packet (packet)
  (let ((class (class-of packet)))
    (with-output-to-sequence (stream)
      (write-binary-type 'Var-Int (packet-id class) stream)
      (dolist (field (class-direct-slots class))
        (write-binary-type (packet-field-type field)
                           (slot-value packet (slot-definition-name field))
                           stream)))))

(defun send-packet (connection packet)
  (send-data (encode-packet packet) connection))

(defun respond (connection packet-id &rest data)
  (send-packet connection (apply #'make-packet (find-package packet-id) data)))

;;; **************************************************************************
;;;  Packets reader
;;; **************************************************************************

(defgeneric read-packet (class stream)
  (:method (packet stream)
   (dopacket (slot packet)
     (let ((value (read-binary-type (packet-field-type slot) stream)))
       (setf (slot-value packet (slot-definition-name slot)) value)))))

(defun parse-packet (stage data)
  (with-input-from-sequence (data-stream data)
    (let* ((id (read-binary-type 'Var-Int data-stream))
           (packet (make-empty-packet (get-packet-class id stage))))
      (read-packet packet data-stream))))

;;; **************************************************************************
;;;  Packets handling
;;; **************************************************************************

(defgeneric handle-packet (connection packet)
  (:documentation "Does packet processing. The result of this function call is sent to client. If more than one packets are needed to be sent, list should be returned."))

(defmacro define-packet-handler (name &body body)
  "Defines new handler for packet with given name. Also defines some handy local-bound functions to interact with connection and client and binds packet fields to appropriate symbols for easy access."
  `(defmethod handle-packet ((connection Connection)
                             (packet ,#1=(intern (symbol-name name)
                                                 (find-packet-package :server))))
     (flet
       ((respond (id &rest data)
          (apply #'respond connection id data))
        (set-status (new-status)
          (setf (connection-status connection) new-status))
        (drop (reason)
          (terminate-connection connection reason))
        (client ()
          (connection-client connection)))
       (with-slots ,(mapcar #'slot-definition-name
                      (class-direct-slots
                        (find-class #1#)))
         packet
         ,@body))))

(defmethod handle-packet ((connection Connection) packet)
  (error 'Invalid-packet
         :message "Dunno what is this shit."
         :connection connection
         :data packet))

(defun process-packet (connection data)
  (handle-packet connection (parse-packet (connection-stage connection) data)))

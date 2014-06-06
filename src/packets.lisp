(in-package :jams)

;;; **************************************************************************
;;;  Packets defintion
;;; **************************************************************************

(defparameter *packets* (make-hash-table))

(define-condition Invalid-packet (error)
  ((message :initarg :message)
   (data :initarg :data
         :initform nil)
   (connection :initarg :connection)))

(defmacro defpacket ((name id) structure &body body)
  "Structure format (type name-for-binding)."
  `(progn
     ,(when body
        `(defun ,name ,(cons 'connection (iter (for (typespec id) in structure)
                                           (unless (and (listp typespec)
                                                        (eql (first typespec)
                                                             :exclude))
                                             (collecting id))))
           (declare (ignorable connection))
           ,@body))
     (setf (gethash ,id *packets*) ',name
           (get ',name :structure) ',structure
           (get ',name :id)         ,id)))

(defun get-packet-name (id)
  (gethash id *packets*))

(defun packet-definition-id (name)
  (get name :id))

(defun packet-definition-structure (name)
  (get name :structure))

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

(in-package :jams)

;;; ***************************************************************************************
;;; Packets defintion
;;; ***************************************************************************************

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

;;; ***************************************************************************************
;;; Packing data and building packets
;;; ***************************************************************************************

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
  (let ((id (random (1- (ash 2 (1- (* 8 (get-type-size :integer))))))))
    (values (encode-packet 'keep-alive id)
            id)))

(defun send-packet (name connection &rest data)
  (send-data (apply #'encode-packet name data)
             connection))

;;; ***************************************************************************************
;;; Packets reader
;;; ***************************************************************************************


(defun subseq-shift (vector start end shift)
  (subseq vector
          (+ start shift)
          (+ end shift)))

(defgeneric read-field (bindings vector shift type count))

(defmethod read-field (bindings vector shift type (count integer))
  (let* ((length (* (get-type-size type) count)))
    (values (decode-data (subseq-shift vector 0 length shift)
                         type
                         (when (< 1 count)
                           :array))
            (+ shift length))))

(defmethod read-field ((bindings list) (vector vector) (shift integer) type
                       (count (eql nil)))
  (declare (ignore count))
  (read-field bindings vector shift type 1))

(defmethod read-field ((bindings list) (vector vector) (shift integer) type (count symbol))
  (read-field bindings vector shift type (cdr (assoc count bindings))))

(defun read-array (bindings vector shift element-type)
  (let ((prefix-size (get-type-size :length-prefix)))
    (read-field bindings
                vector
                (+ shift prefix-size)
                element-type
                (decode-data (subseq-shift vector 0 prefix-size shift)
                             :length-prefix
                             nil))))

(defmethod read-field ((bindings list) (vector vector) (shift integer) (type (eql :string))
                       (count (eql nil)))
  (read-array bindings vector shift :character))

(defmethod read-field ((bindings list) (vector vector) (shift integer)
                       (type (eql :byte-array)) (count (eql nil)))
  (read-array bindings vector shift :byte))

(defun read-typedef (bindings vector shift type-definition)
  (if (not (listp type-definition))
    (read-field bindings vector shift type-definition nil)
    (destructuring-bind (modifier type) type-definition
      (if (not (listp modifier))
        (if (eql modifier :exclude)
          (apply #'values
                 (append (multiple-value-list (read-field bindings vector shift type 1))
                         '(t)))
          (read-field bindings vector shift type 1))
        (when (and (eql (first modifier) :repeat)
                   (or (numberp (second modifier))
                       (symbolp (second modifier))))
          (read-field bindings vector shift type (second modifier)))))))

(defun read-packet-from-vector (vector)
  (let* ((packet-id (svref vector 0))
         (packet (subseq vector 1))
         (packet-structure (packet-definition-structure (get-packet-name packet-id))))
    (iter
      (with bindings)
      (for (field name) in packet-structure)
      (for shift first 0 then new-shift)
      (for (data new-shift exclude-from-result?) next
           (multiple-value-list (read-typedef bindings packet shift field)))
      (push (cons name data) bindings)
      (unless exclude-from-result?
        (collect data into result))
      (finally (return (values (cons packet-id result)
                               bindings))))))

;;; ***************************************************************************************
;;; Packets processing
;;; ***************************************************************************************

(defun process-packet (connection vector)
  (destructuring-bind (packet-id . packet-data)
      (read-packet-from-vector vector)
    (let ((packet-processor (get-packet-name packet-id)))
      (if packet-processor
        (when (fboundp packet-processor)
          (apply packet-processor connection packet-data))
        (error 'Invalid-packet
               :message "Dunno what is this shit."
               :connection connection
               :data packet-id)))))

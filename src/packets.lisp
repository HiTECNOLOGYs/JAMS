(in-package :jams)

(defparameter *packets* nil)

(define-condition Invalid-packet (error)
  ((message :initarg :message)
   (data :initarg :data
         :initform nil)
   (connection :initarg :connection)))

(defun make-packet-definition (id structure processor)
  (list id structure processor))

(defun packet-definition-id (packet-definition)
  (first packet-definition))

(defun packet-definition-structure (packet-definition)
  (second packet-definition))

(defun packet-definition-processor (packet-definition)
  (third packet-definition))

(defmacro defpacket ((name id) structure &body body)
  "Structure format (name-for-binding type)."
  `(progn
     ,(when body
        `(defun ,name ,(cons 'connection (mapcar #'second structure))
           (declare (ignorable connection))
           ,@body))
     (pushnew (make-packet-definition ,id
                                      ',(mapcar #'first structure)
                                      ',name)
              *packets*
              :key #'first
              :test #'=)))

(defun get-packet-definition (id)
  (assoc id *packets*
         :test #'=))

(defun get-packet-definition-by-name (name)
  (find name *packets*
        :key #'packet-definition-processor))

(defun encode-ping-response (protocol-version server-version motd player-count max-players)
  (concatenate 'vector
               (encode-value (list :short
                                   (+ 3 ; header (0xA7 0x31) + null byte
                                      4 ; separators (null bytes)
                                      (length protocol-version)
                                      (length server-version)
                                      (length motd)
                                      (length player-count)
                                      (length max-players))))
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

(defun encode-packet-data (data)
  (reduce (curry #'concatenate 'vector)
          data
          :key #'encode-value))

(defun make-packet (name data)
  (let ((packet-id (packet-definition-id (get-packet-definition-by-name name))))
    (concatenate 'vector
                 (vector packet-id)
                 data)))

(defun encode-packet (name data)
  (make-packet name
               (encode-packet-data data)))

(defun make-keep-alive-packet ()
  (encode-packet 'keep-alive `((:integer ,(random (1- (ash 2 15)))))))


;;; Packets processing

(defun subseq-shift (vector start end shift)
  (subseq vector
          (+ start shift)
          (+ end shift)))

(defun read-field (vector shift type modifier)
  (if (or (and (eql type :character)
               (eql modifier :array))
          (and (eql type :string)
               (null modifier)))
    (let* ((prefix-size (get-type-size :length-prefix))
           (prefix (subseq-shift vector 0 prefix-size shift))
           (length (decode-data prefix :short nil))
           (bytes-length (* (get-type-size :character) length)))
      (values (decode-data (subseq-shift vector
                                         prefix-size
                                         (+ bytes-length prefix-size)
                                         shift)
                           :string
                           nil)
              (+ prefix-size bytes-length)))
    (let ((size (get-type-size type)))
      (values (decode-data (subseq-shift vector
                                         0
                                         size
                                         shift)
                           type
                           modifier)
              size))))

(defun read-typedef (vector shift type-definition)
  (if (listp type-definition)
    (destructuring-bind (modifier type) type-definition
      (read-field vector shift type modifier))
    (read-field vector shift type-definition nil)))

(defun read-packet-from-vector (vector)
  (let* ((packet-id (svref vector 0))
         (packet (subseq vector 1))
         (packet-structure (packet-definition-structure (get-packet-definition packet-id))))
    (cons packet-id
          (iter (for field in packet-structure)
                (for shift first 0 then (+ shift length))
                (for (data length)
                     next (multiple-value-list (read-typedef packet shift field)))
                (collecting data)))))

(defun process-packet (connection vector)
  (destructuring-bind (packet-id . packet-data)
      (read-packet-from-vector vector)
    (let ((packet-processor (packet-definition-processor (get-packet-definition packet-id))))
      (if packet-processor
        (when (fboundp packet-processor)
          (apply packet-processor connection packet-data))
        (error 'Invalid-packet
               :message "Dunno what is this shit."
               :connection connection
               :data packet-id)))))

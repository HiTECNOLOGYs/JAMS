(in-package :jams)

(defparameter *packets* nil)

(define-condition Invalid-packet (error)
  ((message :initarg :message
            :reader message)
   (data :initarg :data
         :initform nil
         :reader data)
   (socket :initarg :socket
           :reader socket)))

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
        `(defun ,name ,(append '(socket)
                               (mapcar #'second structure))
           (declare (ignorable socket))
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

(defun process-packet (socket packet)
  (destructuring-bind (packet-id . packet-data) packet
    (let ((packet-processor (packet-definition-processor (get-packet-definition packet-id))))
      (if packet-processor
        (apply packet-processor socket packet-data)
        (error 'Invalid-packet
               :message "Dunno what is this shit"
               :socket socket
               :data packet-id)))))

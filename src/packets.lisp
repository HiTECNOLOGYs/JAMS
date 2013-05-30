(in-package :jams)

(defparameter *packets* nil)

(define-condition Invalid-packet ()
  ((message :initarg :message
            :reader message)
   (data :initarg :data
         :initform nil
         :reader data)))

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
        `(defun ,name ,(mapcar #'second structure)
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
                                   (+ 3 ; header (0xA7 0x31)
                                      4 ; separators (null bytes)
                                      (length protocol-version)
                                      (length server-version)
                                      (length motd)
                                      (length player-count)
                                      (length max-players))))
               #(#x00 #xA7 #x00 #x31 #x00 #x00)
               (encode-string-raw protocol-version)
               #(#x00 #x00)
               (encode-string-raw server-version)
               #(#x00 #x00)
               (encode-string-raw motd)
               #(#x00 #x00)
               (encode-string-raw player-count)
               #(#x00 #x00)
               (encode-string-raw player-count)))

(defun packet-attach-id (name data)
  (concatenate 'vector
               (vector (packet-definition-id (get-packet-definition-by-name name)))
               data))

(defun encode-packet (name data)
  (let ((packet-id (packet-definition-id (get-packet-definition-by-name name))))
    (concatenate 'vector
                 (vector packet-id)
                 (reduce (curry #'concatenate 'vector)
                         data
                         :key #'encode-value))))

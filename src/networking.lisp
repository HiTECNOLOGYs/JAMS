(in-package :jams)

(defun receive-bytes (stream byte-count)
  (loop repeat byte-count
        collecting (read-byte stream)))

(defun read-prefix (stream)
  (bytes->number (reverse (receive-bytes stream (get-type-size :short)))))

(defun receive-field (stream type)
  (convert type
           (let ((field-size (get-type-size type)))
             (if (not (or (eql field-size :prefix)
                          (eql field-size :prefix*2)
                          (eql field-size :metadata)))
                 (receive-bytes stream field-size)
                 (let ((prefix (read-prefix stream)))
                   (case field-size
                     (:prefix (receive-bytes stream prefix))
                     (:prefix*2 (receive-bytes stream (* 2 prefix)))
                     (:metadata "Not supported yet")))))))

(defun receive-packet (stream)
  (let ((packet-id (read-byte stream)))
    (cons packet-id
          (loop for type in (packet-definition-structure (get-packet-definition packet-id))
                collecting (receive-field stream type)))))

(defun process-packet (packet)
  (destructuring-bind (packet-id . packet-data) packet
    (let ((packet-processor (packet-definition-processor (get-packet-definition packet-id))))
      (if packet-processor
        (apply packet-processor packet-data)
        (error 'Invalid-packet
               :message "Dunno what is this shit."
               :data packet)))))

(defun listener (socket)
  (let ((stream (socket-stream socket)))
    (handler-case
        (let ((responses (process-packet (receive-packet stream))))
          (if (not (listp responses))
              (progn (write-sequence responses
                                     stream)
                     (finish-output stream))
              (dolist (response responses)
                (when response
                  (write-sequence response
                                  stream)
                  (finish-output stream)))))
      (invalid-packet ()
        (write-sequence (encode-packet +kick-packet-id+
                                       '((:string "Suck my dick.")))
                        socket)
        (finish-output socket)))))

(defun server (port)
  (with-socket-listener (socket *wildcard-host* port :element-type '(unsigned-byte 8) :reuse-address t)
    (loop (with-connected-socket (connected-socket (socket-accept socket))
            (listener connected-socket)))))

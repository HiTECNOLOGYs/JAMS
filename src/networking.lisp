(in-package :jams)

(define-constant +master-socket-id+ 0)


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

(defun receive-and-process-packet (connection-id stream)
  (process-packet connection-id stream (receive-packet stream)))

(defun process-new-client (connection-id stream connection)
  (format t "Connecting client with ID: ~D~%" (connection-id connection))
  (let ((responses (receive-and-process-packet connection-id stream)))
    (if (listp responses)
        (dolist (response responses)
          (write-sequence response stream))
        (write-sequence responses stream))
    (finish-output stream))
  nil)

(defun send-data-to-client (connection-id stream connection)
  nil)

(defun process-client (connection-id stream connection)
  nil)

(defun connection-drop-handler (condition)
  (let* ((id (target-id condition))
         (socket (get-socket-by-id id))
         (message (message condition)))
    (format t "~A~%" message)
    (force-output)
    (remove-connection socket)
    (remove-socket socket id)))

(let ((connection-id-counter 0))
  (defun establish-connection (socket)
    (task-handler-bind
        ((drop-connection #'connection-drop-handler)
         (end-of-file     #'connection-drop-handler))
      (let ((connection (make-connection :id (incf connection-id-counter))))
        (add-socket socket connection-id-counter)
        (add-connection socket connection)
        (process-connection socket)))))

(defun process-connection (socket)
  (task-handler-bind
      ((drop-connection #'connection-drop-handler)
       (end-of-file     #'connection-drop-handler))
    (let ((connection (get-connection socket))
          (stream (socket-stream socket)))
      (let ((new-status (funcall (get-connection-status-processor (connection-status connection))
                                 (connection-id connection)
                                 stream
                                 connection)))
        (when new-status
          (setf (connection-status connection)
                new-status)))
      t)))

(defun server (port)
  (setf *kernel* (make-kernel +max-number-of-threads+))
  (with-socket-listener (socket *wildcard-host* port :element-type '(unsigned-byte 8) :reuse-address t)
    (add-socket socket 0)
    (unwind-protect
         (loop
            (dolist (ready-socket (wait-for-input (get-sockets) :ready-only t))
              (if (eql ready-socket socket)
                  (add-to-queue #'establish-connection (socket-accept ready-socket))
                  (add-to-queue #'process-connection (get-connection ready-socket))))
            (print (run-queue)))
      (remove-socket socket 0)))
  (values))

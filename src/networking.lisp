(in-package :jams)

(defvar *event-base*)

;;; I/O buffer

(defun make-io-buffer (delegator connection disconnector
                       &key (max-size 16384))
  (let ((host (connection-remote-address connection))
        (port (connection-remote-port connection))
        (data-queue (connection-data-queue connection))
        (socket (connection-socket connection))
        (buffer (make-array max-size :element-type 'unsigned-byte))
        (read-handler-set? nil)
        (write-handler-set? nil)
        (read-index 0))
    (labels
        ((terminate ()
           (if (lparallel.queue:queue-empty-p data-queue)
             (funcall disconnector host port :close)
             (progn
               (funcall disconnector host port :read)
               (setf read-handler-set? nil))))

         (read-bytes (fd event exception)
           (declare (ignore event exception))
           (handler-case
               (progn
                 (multiple-value-bind (buf bytes-read)
                     (iolib:receive-from socket
                                         :buffer buffer
                                         :start read-index
                                         :end max-size)
                   (declare (ignore buf))
                   (when (zerop bytes-read)
                     (error 'end-of-file))

                   #+jams-debug (format t "[~A:~5D] Read ~D bytes.~%"
                                        host port bytes-read)
                   (incf read-index bytes-read))

                 (funcall delegator connection buffer)

                 (if (lparallel.queue:queue-empty-p data-queue)
                   (progn
                     (funcall disconnector host port :read)
                     (setf read-handler-set? nil))
                   (progn
                     (unless write-handler-set?
                       (iolib:set-io-handler *event-base*
                                             (iolib:socket-os-fd socket)
                                             :write
                                             #'write-bytes)
                       (setf write-handler-set? t))
                     (write-bytes fd :write nil)))

                 (when (connection-closed-p connection)
                   (terminate)))

             (iolib:socket-connection-reset-error ()
               ;; Connection was reseted by client. Closing socket.
               #+jams-debug (format t "[~A:~5D] Connection reset. Closing socket.~%"
                                    host port)
               (funcall disconnector host port :close))

             (end-of-file ()
               ;; Client sent EOF. Write all we have (if we have something)
               ;; and close socket.
               #+jams-debug (format t "[~A:~5D] End of file. Closing socket.~%"
                                    host port)
               (terminate))))

         (write-bytes (fd event exception)
           (declare (ignore fd event exception))
           (handler-case
               (progn
                 (lparallel.queue:with-locked-queue data-queue
                   (unless (lparallel.queue:queue-empty-p/no-lock data-queue)
                     (iter (until (lparallel.queue:queue-empty-p/no-lock data-queue))
                           (for data next (lparallel.queue:pop-queue/no-lock data-queue))
                           (after-each
                            (iolib:send-to socket data)
                            (format t "[~A:~5D] Send ~D bytes.~%"
                                    host port (length data)))))

                   (if (connection-closed-p connection)
                     (funcall disconnector host port :close)
                     (progn (funcall disconnector host port :write)
                            (setf write-handler-set? nil
                                  read-index 0)
                            (unless read-handler-set?
                              (iolib:set-io-handler *event-base*
                                                    (iolib:socket-os-fd socket)
                                                    :read
                                                    #'read-bytes)
                              (setf read-handler-set? t))))))

             (iolib:socket-connection-reset-error ()
               ;; Connection was reseted by client. Closing socket.
               #+jams-debug (format t "[~A:~5D] Connection reset. Closing socket.~%"
                                           host port)
               (funcall disconnector host port :close))

             (isys:ewouldblock ()
               ;; Say 'Oops' and ignore this.
               #+jams-debug (format t "[~A:~5D] OOPS~%"
                                           host port)
               nil)

             (isys:epipe ()
               ;; Client doesn't want to accept our data. Fuck you then, client.
               #+jams-debug (format t "[~A:~5D] End of pipe.~%"
                                           host port)
               (funcall disconnector host port :close)))))

      (lambda (message)
        (case message
          (:read
           (setf read-handler-set? t)
           #'read-bytes)
          (:write
           (setf write-handler-set? t)
           #'write-bytes)
          (otherwise
           (error "What? What the hell is: ~S? I know nothing about such type of IO buffer."
                  message)))))))


;;; Disconnection

(defun make-disconnectior (socket)
  "Returns closure that properly closes socket and removes IO handlers."
  (lambda (host port &rest events)
    (let ((fd (iolib:socket-os-fd socket)))
      (if (not (intersection '(:read :write :error) events))
          (iolib:remove-fd-handlers *event-base* fd :read t :write t :error t)
          (progn
            (when (member :read events)
              (iolib:remove-fd-handlers *event-base* fd :read t))
            (when (member :write events)
              (iolib:remove-fd-handlers *event-base* fd :write t))
            (when (member :error events)
              (iolib:remove-fd-handlers *event-base* fd :error t))))
      (when (member :close events)
        (close socket)
        (delete-connection (get-connection host port))))))


;;; Incoming connections handler

(defun make-listener-handler (delegator socket)
  "Returns closure that accepts and processes all incoming connections."
  (lambda (fd event exception)
    (declare (ignore fd event exception))
    (let ((client-socket (iolib:accept-connection socket :wait t)))
      (when client-socket
        (multiple-value-bind (address port) (iolib:remote-host client-socket)
          (let* ((connection (open-connection address port client-socket))
                 (io-buffer (make-io-buffer delegator
                                            connection
                                            (make-disconnectior client-socket))))
            (iolib:set-io-handler *event-base*
                                  (iolib:socket-os-fd client-socket)
                                  :read
                                  (funcall io-buffer :read))
            (iolib:set-io-handler *event-base*
                                  (iolib:socket-os-fd client-socket)
                                  :write
                                  (funcall io-buffer :write))))))))


;;; Listener

(defun start-listen (port data-handler)
  "Starts listening for incoming connections."
  (iolib:with-open-socket
      (server-socket :connect :passive
                     :address-family :internet
                     :type :stream
                     :ipv6 nil)
    #+jams-debug (format t "Created server socket ~A with FD=~D~%"
                         server-socket (iolib:socket-os-fd server-socket))

    (iolib:bind-address server-socket iolib:+ipv4-unspecified+
                        :port port
                        :reuse-addr t)
    #+jams-debug (format t "Bound server socket: ~A~%"
                         server-socket)

    #+jams-debug (format t "Starting listening on ~A:~D.~%"
                         (iolib:local-host server-socket)
                         (iolib:local-port server-socket))
    (iolib:listen-on server-socket :backlog 5)

    #+jams-debug (format t "Setting read handler.~%")
    (iolib:set-io-handler *event-base*
                          (iolib:socket-os-fd server-socket)
                          :read
                          (make-listener-handler data-handler
                                                 server-socket))

    #+jams-debug (format t "Starting dispatching requests.~%")
    (handler-case
        (iolib:event-dispatch *event-base*)

      (iolib:socket-connection-reset-error ()
        nil)

      (iolib:hangup ()
        nil)

      (end-of-file ()
        nil))))


;;; High-level wrappers

(defun close-all-connections ()
  (iter (for (address connection) in-hashtable *connections*)
    (close (connection-socket connection))
    (remhash address *connections*)))

(defun init-network ()
  (close-all-connections)
  (setf *event-base* (make-instance 'iolib:event-base)))

(defun start-network-listener (port)
  (start-listen port #'process-packet))

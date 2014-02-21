(in-package :jams)

(defvar *event-base*)

;;; I/O buffer

(defun make-io-buffer (connection disconnector &key (max-size 16384))
  (let ((host (connection-remote-address connection))
        (port (connection-remote-port connection))
        (data-queue (connection-data-queue connection))
        (socket (connection-socket connection))
        (buffer (make-array max-size :element-type 'unsigned-byte))
        (read-handler-set? nil)
        (write-handler-set? nil))
    (labels
        ((terminate ()
           (if (lparallel.queue:queue-empty-p data-queue)
             (funcall disconnector :close)
             (progn
               (funcall disconnector :read)
               (setf read-handler-set? nil)))
           (close-connection connection)
           (dispatch-connection connection #()))

         (read-bytes (fd event exception)
           (declare (ignore event exception))
           (handler-case
               (progn
                 (multiple-value-bind (buf bytes-read)
                     (iolib:receive-from socket
                                         :buffer buffer
                                         :end max-size)
                   (declare (ignore buf))
                   (when (zerop bytes-read)
                     (error 'end-of-file)))

                 (dispatch-connection connection buffer)

                 (unless (lparallel.queue:queue-empty-p data-queue)
                   (unless write-handler-set?
                     (iolib:set-io-handler *event-base*
                                           (iolib:socket-os-fd socket)
                                           :write
                                           #'write-bytes)
                     (setf write-handler-set? t))
                   (write-bytes fd :write nil))

                 (when (connection-closed-p connection)
                   (terminate)))

             (iolib:socket-connection-reset-error ()
               ;; Connection was reseted by client. Closing socket.
               #+jams-debug (log-message :info "[~A:~5D] Connection reset. Closing socket."
                                         host port)
               (funcall disconnector :close))

             (end-of-file ()
               ;; Client sent EOF. Write all we have (if we have something)
               ;; and close socket.
               #+jams-debug (log-message :info "[~A:~5D] End of file. Closing socket."
                                         host port)
               (terminate))

             (invalid-packet ()
               #+jams-debug (log-message :warning "Received invalid packet from #~D."
                                         (connection-id connection)))

             (close-connection (condition)
               ;; Server initiated connection closing. Executing.
               #+jams-debug
               (with-slots (connection reason) condition
                 (log-message :info "[~A:~5D] Closing connection #~D. (~A)"
                              host port
                              (connection-id connection)
                              reason))
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
                            (iolib:send-to socket data))))

                   (if (connection-closed-p connection)
                     (funcall disconnector :close)
                     (progn (funcall disconnector :write)
                            (setf write-handler-set? nil)
                            (unless read-handler-set?
                              (iolib:set-io-handler *event-base*
                                                    (iolib:socket-os-fd socket)
                                                    :read
                                                    #'read-bytes)
                              (setf read-handler-set? t))))))

             (iolib:socket-connection-reset-error ()
               ;; Connection was reseted by client. Closing socket.
               #+jams-debug (log-message :info "[~A:~5D] Connection reset. Closing socket."
                                         host port)
               (funcall disconnector :close))

             (isys:ewouldblock ()
               ;; Say 'Oops' and ignore this.
               #+jams-debug (log-message :error "[~A:~5D] OOPS."
                                         host port)
               nil)

             (isys:epipe ()
               ;; Client doesn't want to accept our data. Fuck you then, client.
               #+jams-debug (log-message :warning "[~A:~5D] End of pipe."
                                         host port)
               (funcall disconnector :close)))))

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
(defun make-disconnectior (connection)
  "Returns closure that properly closes socket and removes IO handlers."
  (lambda (&rest events)
    (let* ((socket (connection-socket connection))
           (fd (iolib:socket-os-fd socket)))
      (unless (null fd)
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
          (delete-connection connection))))))


;;; Incoming connections handler

(defun make-listener-handler (socket)
  "Returns closure that accepts and processes all incoming connections."
  (lambda (fd event exception)
    (declare (ignore fd event exception))
    (let ((client-socket (iolib:accept-connection socket :wait t)))
      (when client-socket
        (let ((address (iolib:remote-host client-socket))
              (port (iolib:remote-port client-socket)))
          #+jams-debug (log-message :info "[~A:~5D] Connected."
                                    address port)
          (let* ((connection (open-connection address port client-socket #'process-packet))
                 (io-buffer (make-io-buffer connection (make-disconnectior connection))))
            (iolib:set-io-handler *event-base*
                                  (iolib:socket-os-fd client-socket)
                                  :read
                                  (funcall io-buffer :read))
            (iolib:set-io-handler *event-base*
                                  (iolib:socket-os-fd client-socket)
                                  :write
                                  (funcall io-buffer :write))))))))

;;; Listener

(defun start-listen (port)
  "Starts listening for incoming connections."
  (iolib:with-open-socket
      (server-socket :connect :passive
                     :address-family :internet
                     :type :stream
                     :ipv6 nil)
    #+jams-debug (log-message :info "Created server socket ~A with FD=~D"
                              server-socket (iolib:socket-os-fd server-socket))

    (iolib:bind-address server-socket iolib:+ipv4-unspecified+
                        :port port
                        :reuse-addr t)
    #+jams-debug (log-message :info "Bound server socket: ~A"
                              server-socket)

    #+jams-debug (log-message :info "Starting listening on ~A:~D."
                              (iolib:local-host server-socket)
                              (iolib:local-port server-socket))
    (iolib:listen-on server-socket :backlog 5)

    #+jams-debug (log-message :info "Setting read handler.")
    (iolib:set-io-handler *event-base*
                          (iolib:socket-os-fd server-socket)
                          :read
                          (make-listener-handler server-socket))

    #+jams-debug (log-message :info "Starting dispatching requests.")
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
    (close (connection-socket connection) :abort t)
    (remhash address *connections*)))

(defun init-networking ()
  (setf *event-base* (make-instance 'iolib:event-base)))

(defun cleanup-networking ()
  (close-all-connections)
  (when *event-base*
    (close *event-base*)))

(defthread network-listener (port) ()
  (unwind-protect
       (handler-case
           (progn (init-networking)
                  (start-listen port))
         (thread-termination ()
           (log-message :info "Stopping network listener on port ~D."
                        port))
         (sb-sys:interactive-interrupt ()
           (log-message :info "Caught ^C. Exiting."))
         (simple-error (condition)
           (log-message :error "Unknown error: ~A. Exiting." condition)))
    (cleanup-networking)
    #+jams-debug (log-message :info "Stopped network listener on port ~D."
                              port)))

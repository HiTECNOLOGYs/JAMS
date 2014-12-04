(in-package :jams)

(defvar *event-base*)
(defvar *connections* (make-hash-table :test 'equal))

(define-constant +max-no-keep-alive-time+ (* 1 60) ; One minute (±1 s.)
  :test #'=)

;;; **************************************************************************
;;;  Connections
;;; **************************************************************************

;; ----------------
;; MOP

(defclass Connection ()
  ((id :initarg :id
       :reader connection-id)
   (remote-host :initarg :remote-host
                :reader connection-remote-host)
   (remote-port :initarg :remote-port
                :reader connection-remote-port)
   (data-queue :initform (lparallel.queue:make-queue)
               :accessor connection-data-queue)
   (socket :initarg :socket
           :reader connection-socket)
   (status :initform :opening
           :type (member :opening :running :closed)
           :accessor connection-status)
   (stage :initform :handshaking
          :type (member :handshaking :login :status :play)
          :accessor connection-stage)
   (keep-alive-received? :initform nil
                         :accessor connection-keep-alive-received-p)
   (last-keep-alive-id :accessor connection-last-keep-alive-id)
   (last-keep-alive-time :initform (get-universal-time)
                         :accessor connection-last-keep-alive-time)))

(defmethod print-object ((object Connection) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (with-slots (id remote-address remote-port stage status) object
      (format stream "#~D ~A:~D ~A (~A)" id remote-address remote-port stage status))))

(defmethod initialize-instance :after ((instance Connection) &rest initargs)
  (declare (ignore initargs))
  (cond
    ((not (slot-boundp instance 'socket))
     (error "Socket not supplied."))
    (t
     (with-slots (socket) instance
       (unless (slot-boundp instance 'id)
         (setf (slot-value instance 'id) (iolib:socket-os-fd socket)))
       (unless (slot-boundp instance 'remote-host)
         (setf (slot-value instance 'remote-host) (iolib:remote-host socket)))
       (unless (slot-boundp instance 'remote-port)
         (setf (slot-value instance 'remote-port) (iolib:remote-port socket)))))))

;; ----------------
;; Dispatch

(defgeneric keep-alive (connection &optional received-id)
  (:method ((connection Connection) &optional received-id)
   ;; This default method can only check if keep alive was right.
   (unless received-id
     (when (and (slot-boundp connection 'last-keep-alive-id)
                (= (connection-last-keep-alive-id connection) received-id))
       (setf (connection-last-keep-alive-time connection)  (get-universal-time)
             (connection-keep-alive-received-p connection) t)))))

(defgeneric dispatch-connection-data (connection data)
  (:method-combination list))

(defgeneric dispatch-connection-establishment (connection)
  (:method ((connection Connection))
   (setf (connection-status connection) :running)
   (iolib:set-io-handler *event-base*
                                (connection-id connection)
                                :read
                                #'dispatch-read-event)))

(defgeneric dispatch-connection-termination (connection)
  (:method ((connection Connection))
   (iolib:remove-fd-handlers *event-base* (connection-id connection)
                             :read t :write t :error t)
   (close (connection-socket connection) :abort t)
   (delete-connection connection)))

(defgeneric dispatch-connection (connection &optional data)
  (:method ((connection Connection) &optional data)
   (case (connection-status connection)
     (:opening
       (dispatch-connection-establishment connection))
     (:running
       (unless (zerop (length data))
         (dispatch-connection-data connection data))
       (when (< +max-no-keep-alive-time+
                (- (connection-last-keep-alive-time connection)
                   (get-universal-time)))
         (terminate-connection connection "Didn't receive keep alive.")))
     (:closed
       (dispatch-connection-termination connection)))
   connection))

;; ----------------
;; Condtions

(define-condition Close-connection ()
  ((connection :initarg :connection)
   (reason :initarg :reason)))

;; ----------------
;; Predicates

(defun connection-closed-p (connection)
  (eql (connection-status connection) :closed))

(defun connection-running-p (connection)
  (not (connection-closed-p connection)))

;; ----------------
;; Connections management

(defun get-connection (connection-id)
  (gethash connection-id *connections*))

(defun (setf get-connection) (new-value connection-id)
  (setf (gethash connection-id *connections*)
        new-value))

(defun terminate-connection (connection reason)
  (error 'Close-connection
         :connection connection
         :reason reason))

(defun open-connection (socket)
  (let ((connection (make-instance 'Connection
                                   :socket socket)))
    (setf (get-connection (connection-id connection)) connection)
    connection))

(defun close-connection (connection)
  (setf (connection-status connection) :closed)
  (dispatch-connection connection))

(defun delete-connection (connection)
  (remhash (connection-id connection) *connections*))

;;; **************************************************************************
;;;  Sockets
;;; **************************************************************************

;; ----------------
;; Data transmission

(defgeneric send-data (connection data)
  (:method ((connection Connection) (data vector))
   (with-slots (data-queue) connection
     (lparallel.queue:with-locked-queue data-queue
       (lparallel.queue:push-queue/no-lock data data-queue)))
   (iolib:set-io-handler *event-base*
                         (connection-id connection)
                         :write
                         #'dispatch-write-event)))

(defun flush-data-queue (connection)
  (with-slots (id data-queue socket) connection
    (lparallel.queue:with-locked-queue data-queue
      (unless (lparallel.queue:queue-empty-p/no-lock data-queue)
        (iter
          (until (lparallel.queue:queue-empty-p/no-lock data-queue))
          (for data next (lparallel.queue:pop-queue/no-lock data-queue))
          (after-each
            (iolib:send-to socket data)))))
    (iolib:remove-fd-handlers *event-base* id :write t)))

(defun receive-packet-length (socket)
  (let ((buffer (make-array 4
                            :element-type '(unsigned-byte 8)
                            :initial-element 0
                            :fill-pointer 0)))
    (iolib:receive-from socket
                        :buffer buffer
                        :start 0
                        :size 1)
    (iter (for i from 1 below 4)
          (while (logand #b10000000 (aref buffer i)))
          (after-each
            (iolib:receive-from socket
                                :buffer buffer
                                :start i
                                :size 1))
          (finally (return buffer)))))

;; ----------------
;; Event dispatchers

(defun dispatch-read-event (fd event exception)
  (declare (ignore event exception))
  (let ((connection (get-connection fd)))
    (handler-case
        (with-slots (socket) connection
          (multiple-value-bind (length bytes-read)
              (receive-packet-length socket)
            (when (zerop bytes-read)
              (error 'end-of-file))
            (multiple-value-bind (packet bytes-read)
                (iolib:receive-from socket :end length)
              (when (zerop bytes-read)
                (error 'end-of-file))
              (add-task-to-queue #'dispatch-connection connection packet))))
      ;; ----------------
      (iolib:socket-connection-reset-error ()
        (log:error connection "reset")
        (close-connection connection))
      ;; ----------------
      (end-of-file ()
        (log:error "End of file on" connection)
        (close-connection connection))
      ;; ----------------
      (invalid-packet ()
        (log:error "Received invalid packet on" connection))
      ;; ----------------
      (close-connection (condition)
        (log:info "Closing" connection (slot-value condition 'reason))
        (close-connection connection)))))

(defun dispatch-write-event (fd event exception)
  (declare (ignore event exception))
  (let ((connection (get-connection fd)))
    (handler-case
        (flush-data-queue connection)
      ;; ----------------
      (iolib:socket-connection-reset-error ()
        (log:error connection "reset")
        (close-connection connection))
      ;; ----------------
      (isys:ewouldblock ()
        ;; Say 'Oops' and ignore this.
        (log:error connection "OOPSed")
        nil)
      ;; ----------------
      (isys:epipe ()
        (log:error "End of pipe on" connection)
        (close-connection connection)))))

;; ----------------
;; Listener

(defun make-listen-handler (socket)
  "Returns closure that accepts and processes all incoming connections."
  (lambda (fd event exception)
    (declare (ignore fd event exception))
    (when-let (client-socket (iolib:accept-connection socket :wait t))
      (log:info "Got connection from ~A:~D"
                (iolib:remote-host client-socket)
                (iolib:remote-port client-socket))
      (dispatch-connection (open-connection client-socket)))))

;; ----------------
;; Start listener

(defun start-listen (port)
  "Starts listening for incoming connections."
  (iolib:with-open-socket
      (server-socket :connect :passive
                     :address-family :internet
                     :type :stream
                     :ipv6 nil)
    (log:debug "Created server socket ~A with FD=~D"
               server-socket (iolib:socket-os-fd server-socket))
    (iolib:bind-address server-socket iolib:+ipv4-unspecified+
                        :port port
                        :reuse-addr t)
    (log:debug "Bound server socket: ~A"
               server-socket)
    (log:debug "Starting listening on ~A:~D."
               (iolib:local-host server-socket)
               (iolib:local-port server-socket))
    (iolib:listen-on server-socket :backlog 5)
    (log:debug "Setting read handler.")
    (iolib:set-io-handler *event-base*
                          (iolib:socket-os-fd server-socket)
                          :read
                          (make-listen-handler server-socket))
    (log:debug "Starting dispatching requests.")
    (handler-case
        (iolib:event-dispatch *event-base*)
      ;; ----------------
      (iolib:socket-connection-reset-error ()
        nil)
      ;; ----------------
      (iolib:hangup ()
        nil)
      ;; ----------------
      (end-of-file ()
        nil))))

;;; **************************************************************************
;;;  Networking API
;;; **************************************************************************

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

;;; **************************************************************************
;;;  Thread
;;; **************************************************************************

(defthread network-listener (port) ()
  (unwind-protect
       (handler-case
           (progn (init-networking)
                  (start-listen port))
         (thread-termination ()
           (log:debu2 "Stopping network listener on" port))
         (sb-sys:interactive-interrupt ()
           (log:debug "Caught ^C. Exiting."))
         ;; (simple-error (condition)
         ;;   (log:error "Unknown error: ~A. Exiting." condition))
         )
    (cleanup-networking)
    (log:debu2 "Stopped network listener on" port)))

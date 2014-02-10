(in-package :jams)

(define-constant +max-number-of-threads+ 4)

(defparameter *threads-messages-buses* (make-hash-table))
(defparameter *tasks-queue*            (lparallel.queue:make-queue))

(defun clear-queue (queue)
  (lparallel.queue:with-locked-queue queue
    (iter (repeat (lparallel.queue:queue-count/no-lock queue))
      (lparallel.queue:pop-queue/no-lock queue)))
  (values))

(defun add-task-to-queue (function &rest arguments)
  (lparallel.queue:with-locked-queue *tasks-queue*
    (lparallel.queue:push-queue/no-lock (cons function arguments)
                                        *tasks-queue*))
  (values))

(defun run-tasks-queue ()
  (let ((channel (make-channel)))
    (lparallel.queue:with-locked-queue *tasks-queue*
      (let ((*task-category* 'connections)
            (task-count (lparallel.queue:queue-count/no-lock *tasks-queue*)))
        (iter (repeat task-count)
          (apply #'submit-task channel (lparallel.queue:pop-queue/no-lock *tasks-queue*)))
        (iter (repeat task-count)
          (collecting (receive-result channel)))))))

(defun get-thread-message-bus (thread-name)
  (gethash thread-name *threads-messages-buses*))

(defun (setf get-thread-message-bus) (new-value thread-name)
  (setf (gethash thread-name *threads-messages-buses*)
        new-value))

(defun send-message-to-thread (thread-name message)
  (let ((queue (get-thread-message-bus thread-name)))
    (lparallel.queue:with-locked-queue queue
      (lparallel.queue:push-queue/no-lock message
                                          queue))))

(defun receive-message (thread-name)
  (let ((queue (get-thread-message-bus thread-name)))
    (lparallel.queue:with-locked-queue queue
      (lparallel.queue:pop-queue/no-lock queue))))

(defun message-queue-empty-p (thread-name)
  (let ((queue (get-thread-message-bus thread-name)))
    (lparallel.queue:with-locked-queue queue
      (lparallel.queue:queue-empty-p/no-lock queue))))

(defun clear-message-queue (&optional thread-name)
  (if thread-name
      (clear-queue (get-thread-message-bus thread-name))
      (clrhash *threads-messages-buses*)))

(defun fork (function name)
  (setf (get-thread-message-bus name) (lparallel.queue:make-queue))
  (make-thread function :name (symbol-name name)))

(defmacro with-delay ((value) &body body)
  `(progn (sleep ,value)
          ,@body))

(defmacro with-message-queue ((message-var queue-name) &body body)
  `(let ((,message-var (unless (message-queue-empty-p ,queue-name)
                         (receive-message ,queue-name))))
     ,@body))

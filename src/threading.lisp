(in-package :jams)

(define-constant +max-number-of-threads+ 4)

(defparameter *threads-messages-buses* (make-hash-table))
(defparameter *tasks-queue*            (make-queue))

(defun clear-queue (queue)
  (with-locked-queue queue
    (loop repeat (queue-count/no-lock queue)
       do (pop-queue/no-lock queue)))
  (values))

(defun add-task-to-queue (function &rest arguments)
  (with-locked-queue *tasks-queue*
    (push-queue/no-lock (cons function arguments)
                        *tasks-queue*))
  (values))

(defun run-tasks-queue ()
  (let ((channel (make-channel)))
    (with-locked-queue *tasks-queue*
      (let ((*task-category* 'connections)
            (task-count (queue-count/no-lock *tasks-queue*)))
        (loop repeat task-count
           do (apply #'submit-task channel (pop-queue/no-lock *tasks-queue*)))
        (loop repeat task-count
           collecting (receive-result channel))))))

(defun get-thread-message-bus (thread-name)
  (gethash thread-name *threads-messages-buses*))

(defun (setf get-thread-message-bus) (new-value thread-name)
  (setf (gethash thread-name *threads-messages-buses*)
        new-value))

(defun send-message-to-thread (thread-name message)
  (let ((queue (get-thread-message-bus thread-name)))
    (with-locked-queue queue
      (push-queue/no-lock message
                          queue))))

(defun receive-message (thread-name)
  (let ((queue (get-thread-message-bus thread-name)))
    (with-locked-queue queue
      (pop-queue/no-lock queue))))

(defun message-queue-empty-p (thread-name)
  (let ((queue (get-thread-message-bus thread-name)))
    (with-locked-queue queue
      (queue-empty-p/no-lock queue))))

(defun clear-message-queue (&optional thread-name)
  (if thread-name
      (clear-queue (get-thread-message-bus thread-name))
      (clrhash *threads-messages-buses*)))

(defun run-separate-thread (function name)
  (setf (get-thread-message-bus name)
        (make-queue))
  (make-thread function :name (symbol-name name)))

(defmacro with-delay ((value) &body body)
  `(progn (sleep ,value)
          ,@body))

(in-package :jams)

(define-constant +max-number-of-threads+ 4)

;;; General stuff
(defmacro with-delay (value &body body)
  `(progn (sleep ,value)
          ,@body))


;;; Tasks queue

(defparameter *tasks-queue* (lparallel.queue:make-queue))

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


;;; Threads managemement

(define-condition Thread-termination () ())

(defmacro defforkable (name args cleanup &body body)
  `(setf (get ',name :function)
         #'(lambda ,args
             (handler-bind
                 ((Thread-termination
                    #'(lambda (condition)
                        (declare (ignore condition))
                        #+jams-debug (log-message :info
                                                  ,(format nil "Stopping thread \"~S\"."
                                                           name))
                        ,cleanup)))
               #+jams-debug ,(log-message :info (format nil "Starting thread \"~S\"." name))
               ,@body))))

(defun thread-running-p (name)
  (let ((thread (get name :thread)))
    (and (threadp thread)
            (thread-alive-p thread))))

(defun start-thread (name)
  (let* ((function (get name :function))
         (thread (make-thread function :name (symbol-name name))))
    (setf (get name :thread) thread)
    thread))

(defun stop-thread (name)
  (when (thread-running-p name)
    (interrupt-thread (get name :thread)
                      #'signal 'Thread-termination)))

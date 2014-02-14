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

(defun thread-function (name)
  (get name :function))

(defun (setf thread-function) (new-value name)
  (setf (get name :function) new-value))

(defun thread-thread (name)
  (get name :thread))

(defun (setf thread-thread) (new-value name)
  (setf (get name :thread) new-value))

(defmacro defthread (name args cleanup &body body)
  `(setf (thread-function ',name)
         #'(lambda ,args
             (handler-bind
                 ((Thread-termination
                    #'(lambda (condition)
                        (declare (ignorable condition))
                        #+jams-debug (log-message :info
                                                  ,(format nil "Stopping thread \"~S\"."
                                                           name))
                        ,cleanup)))
               #+jams-debug (log-message :info ,(format nil "Starting thread \"~S\"." name))
               ,@body))))

(defun thread-running-p (name)
  (let ((thread (thread-thread name)))
    (and (threadp thread)
         (thread-alive-p thread))))

(defun start-thread (name &rest args)
  (unless (thread-running-p name)
    (let* ((function (thread-function name))
           (thread (make-thread (apply #'curry function args)
                                :name (symbol-name name))))
      (setf (thread-thread name) thread)
      thread)))

(defun stop-thread (name)
  (when (thread-running-p name)
    (interrupt-thread (thread-thread name)
                      #'signal 'Thread-termination)
    t))

(defun restart-thread (name &rest args)
  (stop-thread name)
  (apply #'start-thread name args))


;;; Time and scheduling

(defun list-timers ()
  (sb-ext:list-all-timers))

(defun timer-scheduled-p (timer)
  (sb-ext:timer-scheduled-p timer))

(defun schedule-timer (name function time &key repeat-interval thread absolute-p)
  (let ((timer (sb-ext:make-timer function
                                  :name name
                                  :thread thread)))
    (sb-ext:schedule-timer timer time
                           :repeat-interval repeat-interval
                           :absolute-p absolute-p)
    timer))

(defun unschedule-timer (timer)
  (sb-ext:unschedule-timer timer))

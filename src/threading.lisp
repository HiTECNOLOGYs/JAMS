(in-package :jams)

(define-constant +max-number-of-threads+ 4)

(let ((queue (make-queue)))
  
  (defun get-queue ()
    queue)
  
  (defun (setf get-queue) (value)
    (setf queue value))
  
  (defun clear-queue ()
    (with-locked-queue queue
      (loop repeat (queue-count/no-lock queue)
            do (pop-queue/no-lock queue)))
    (values))

  (defun add-to-queue (function &rest arguments)
    (push-queue (cons function arguments)
                queue)
    (values))

  (defun run-queue ()
    (let ((channel (make-channel)))
      (with-locked-queue queue
        (let ((*task-category* 'connections)
              (task-count (queue-count/no-lock queue)))
          (loop repeat task-count
                do (apply #'submit-task channel (pop-queue/no-lock queue)))
          (loop repeat task-count
                collecting (receive-result channel)))))))

(in-package :jams)

(define-constant +max-number-of-threads+ 4)

(let ((queue (make-queue)))
  
  (defun get-queue ()
    queue)
  
  (defun empty-queue ()
    (with-locked-queue queue
      (dotimes (c (queue-count/no-lock queue))
        (pop-queue/no-lock queue)))
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

(in-package :jams)

(defparameter *packets* nil)

(defun make-packet-definition (id structure processor)
  (list id structure processor))

(defun packet-definition-id (packet-definition)
  (first packet-definition))

(defun packet-definition-structure (packet-definition)
  (second packet-definition))

(defun packet-definition-processor (packet-definition)
  (third packet-definition))

(defmacro defpacket (id structure &body body)
  "Structure format (name-for-binding type)."
  `(pushnew (make-packet-definition ,id
                                    ',structure
                                    #'(lambda ,(mapcar #'first structure)
                                        ,@body))
            *packets*
            :key #'first
            :test #'=))

(defun get-packet-definition (id)
  (assoc id *packets*
         :test #'=))


(define-condition Invalid-packet ()
  ((message :initarg :message)
   (data :initarg :data
         :initform nil)))

(defun read-field (structure data)
  (destructuring-bind (name type) structure
    (declare (ignore name))
    (let ((size (get-type-size type)))
      (case size
        (:prefix*2 (let ((final-size (* 2 (compose-bytes (list (second data)
                                                               (first data))))))
                     (values (convert type (subseq (cddr data) 0 final-size))
                             (nthcdr final-size (cddr data)))))
        (:metadata "Not supported")
        (otherwise (values (convert type (subseq data 0 size))
                           (nthcdr size data)))))))

(defun parse-packet-data (structure data)
  (if (not structure)
      (if data
          (error 'Invalid-packet "Invalid packet ID here! Hey!")
          nil)
      (multiple-value-bind (result new-data) (read-field (first structure) data)
        (cons result
              (parse-packet-data (cdr structure)
                                 new-data)))))

(defun process-packet (packet)
  (destructuring-bind (packet-id . packet-data) packet
    (let* ((packet-definition (get-packet-definition packet-id))
           (packet-strcture (packet-definition-structure packet-definition))
           (packet-processor (packet-definition-processor packet-definition)))
      (if (not (and packet-strcture packet-processor))
          (format t "Dunno what is this shit: ~A~%" packet)
          (apply packet-processor
                 (parse-packet-data packet-strcture packet-data))))))

(defun encode-packet (id data)
  (concatenate 'vector
               (vector id)
               (reduce (curry #'concatenate 'vector)
                       data
                       :key #'encode-value)))

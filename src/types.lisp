(in-package :jams)

(defparameter *type-sizes*
  '((:byte          . 1)
    (:short         . 2)
    (:integer       . 4)
    (:long          . 8)
    (:float         . 4)
    (:double        . 8)
    (:character     . 2)
    (:bool          . 1)
    (:length-prefix . 2)
    (:metadata      . :metadata)))

(defun get-type-size (typespec)
  (aif (cdr (assoc typespec *type-sizes*))
    it
    0))

(defun two-bytes-to-fixnum (vector-of-four)
  (let ((unsigned 0))
    (setf (ldb (byte 8 0) unsigned) (aref vector-of-four 0))
    (setf (ldb (byte 8 8) unsigned) (aref vector-of-four 1))
    (logior unsigned
            (- (mask-field (byte 1 15) unsigned)))))

(defun four-bytes-to-fixnum (vector-of-four)
  (let ((unsigned 0))
    (setf (ldb (byte 8 0) unsigned) (aref vector-of-four 0))
    (setf (ldb (byte 8 8) unsigned) (aref vector-of-four 1))
    (setf (ldb (byte 8 16) unsigned) (aref vector-of-four 2))
    (setf (ldb (byte 8 24) unsigned) (aref vector-of-four 3))
    (logior unsigned
            (- (mask-field (byte 1 31) unsigned)))))

(defun eight-bytes-to-fixnum (vector-of-four)
  (let ((unsigned 0))
    (setf (ldb (byte 8 0) unsigned) (aref vector-of-four 0))
    (setf (ldb (byte 8 8) unsigned) (aref vector-of-four 1))
    (setf (ldb (byte 8 16) unsigned) (aref vector-of-four 2))
    (setf (ldb (byte 8 24) unsigned) (aref vector-of-four 3))
    (setf (ldb (byte 8 32) unsigned) (aref vector-of-four 0))
    (setf (ldb (byte 8 40) unsigned) (aref vector-of-four 1))
    (setf (ldb (byte 8 48) unsigned) (aref vector-of-four 2))
    (setf (ldb (byte 8 56) unsigned) (aref vector-of-four 3))
    (logior unsigned
            (- (mask-field (byte 1 63) unsigned)))))

(defun compose-bytes (bytes)
  (loop for position from (1- (length bytes)) downto 0
        for shift = 0 then (+ 8 shift)
        for byte = (elt bytes position)
        for result = byte
                   then (logior (ash byte shift) result)
        finally (return result)))

(defgeneric decode-data (data typespec modifier))
(defgeneric encode-data (data modifier))

(defmethod decode-data (data (typespec (eql :metadata)) modifier)
  (error "Unable to decode metadata yet. =("))

(defmethod decode-data ((data vector) typespec (modifier (eql nil)))
  (declare (ignore modifier))
  (compose-bytes data))

(defmethod decode-data ((data vector) typespec (modifier (eql :array)))
  (declare (ignore modifier))
  data)

(defmethod decode-data ((data vector) (typespec (eql :character)) (modifier (eql nil)))
  (declare (ignore modifier))
  (code-char (logior (ash (elt data 0) 8)
                     (elt data 1))))

(defmethod decode-data ((data vector) (typespec (eql :character)) (modifier (eql :array)))
  (declare (ignore modifier))
  (octets-to-string data
                    :external-format (make-external-format :utf-16
                                                           :little-endian nil)))

(defmethod decode-data ((data vector) (typespec (eql :string)) (modifier (eql nil)))
  (decode-data data :character :array))

(defmethod decode-data ((data vector) (typespec (eql :float)) (modifier (eql nil)))
  (declare (ignore modifier))
  (decode-float32 (compose-bytes data)))

(defmethod decode-data ((data vector) (typespec (eql :double)) (modifier (eql nil)))
  (declare (ignore modifier))
  (decode-float64 (compose-bytes data)))

(defmethod decode-data ((data vector) (typespec (eql :float)) (modifier (eql nil)))
  (declare (ignore modifier))
  (mapcar #'decode-float32 data))

(defmethod decode-data ((data vector) (typespec (eql :double)) (modifier (eql nil)))
  (declare (ignore modifier))
  (mapcar #'decode-float64 data))

(defmethod encode-data ((data integer) (size integer))
  (if (zerop data)
      (make-array (list size)
                  :initial-element 0
                  :element-type '(unsigned-byte 8))
      (loop for shift from 0 upto (1- size) by 8
            with result = (make-array (list size) 
                                      :initial-element 0
                                      :fill-pointer 0
                                      :element-type '(unsigned-byte 8))
            do (vector-push-extend (ldb (byte 8 shift) data)
                                   result)
            finally (progn (setf (fill-pointer result)
                                 size)
                           (return (reverse result))))))

(defmethod encode-data ((data integer) (typespec (eql nil)))
  (encode-data data :byte))

(defmethod encode-data ((data integer) (typespec symbol))
  (encode-data data (get-type-size typespec)))

(defmethod encode-data ((data float) (typespec (eql nil)))
  (encode-data data :single))

(defmethod encode-data ((data float) (typespec (eql :float)))
  (encode-data data :single))

(defmethod encode-data ((data float) (typespec (eql :single)))
  (encode-data (encode-float32 data) 4))

(defmethod encode-data ((data float) (typespec (eql :double)))
  (encode-data (encode-float64 data) 8))

(defmethod encode-data ((data string) (modifier (eql :raw)))
  (string-to-octets data
                    :external-format (make-external-format :utf-16
                                                           :little-endian nil)))

(defmethod encode-data ((data string) (modifier (eql nil)))
  (concatenate 'vector
               (encode-data (length data) :short)
               (encode-data data :raw)))

(defmethod encode-data ((data vector) (modifier (eql :raw)))
  data)

(defmethod encode-data ((data vector) (modifier (eql nil)))
  (concatenate 'vector
               (encode-data (length data) :short)
               (encode-data data :raw)))

(defmethod encode-data ((data symbol) (modifier (eql nil)))
  (cond
    ((eql data t) #(1))
    ((eql data nil) #(0))
    (t (error "Can't possibly imagine how I supposed to convert ~S to bytes array." data))))


(defgeneric read-value (stream type modifier))

(defmethod read-value (stream type (modifier (eql :array)))
  (let ((prefix (read-value stream :short nil)))
    (decode-data (read-bytes stream (* prefix (get-type-size type)))
                 type
                 modifier)))

(defmethod read-value (stream (type (eql :string)) (modifier (eql nil)))
  (read-value stream :character :array))

(defmethod read-value (stream type modifier)
  (decode-data (read-bytes stream (get-type-size type))
               type
               modifier))

(defun read-typedef (stream type-definition)
  (if (listp type-definition)
      (destructuring-bind (modifier type) type-definition
        (read-value stream type modifier))
      (read-value stream type-definition nil)))

(defun encode-value (value)
  (if (listp value)
      (destructuring-bind (modifier data) value
        (encode-data data modifier))
      (encode-data value nil)))

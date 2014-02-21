(in-package :jams)

;;; Basic types

(defmacro define-simple-type (name size)
  `(setf (get ,name :size) ,size))

(defun get-type-size (typespec)
  (aif (get (if (not (listp typespec))
              typespec
              (second typespec))
            :size)
    it
    0))

(define-simple-type :byte          1)
(define-simple-type :short         2)
(define-simple-type :integer       4)
(define-simple-type :long          8)
(define-simple-type :float         4)
(define-simple-type :double        8)
(define-simple-type :character     2)
(define-simple-type :bool          1)
(define-simple-type :length-prefix 2)

(defun two-bytes-to-fixnum (vector)
  (let ((unsigned 0))
    (setf (ldb (byte 8 0) unsigned) (aref vector 0))
    (setf (ldb (byte 8 8) unsigned) (aref vector 1))
    (logior unsigned
            (- (mask-field (byte 1 15) unsigned)))))

(defun four-bytes-to-fixnum (vector)
  (let ((unsigned 0))
    (setf (ldb (byte 8 0) unsigned) (aref vector 0))
    (setf (ldb (byte 8 8) unsigned) (aref vector 1))
    (setf (ldb (byte 8 16) unsigned) (aref vector 2))
    (setf (ldb (byte 8 24) unsigned) (aref vector 3))
    (logior unsigned
            (- (mask-field (byte 1 31) unsigned)))))

(defun eight-bytes-to-fixnum (vector)
  (let ((unsigned 0))
    (setf (ldb (byte 8 0) unsigned) (aref vector 0))
    (setf (ldb (byte 8 8) unsigned) (aref vector 1))
    (setf (ldb (byte 8 16) unsigned) (aref vector 2))
    (setf (ldb (byte 8 24) unsigned) (aref vector 3))
    (setf (ldb (byte 8 32) unsigned) (aref vector 0))
    (setf (ldb (byte 8 40) unsigned) (aref vector 1))
    (setf (ldb (byte 8 48) unsigned) (aref vector 2))
    (setf (ldb (byte 8 56) unsigned) (aref vector 3))
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

(defmethod decode-data ((data vector) typespec modifier)
  (compose-bytes data))

(defmethod decode-data ((data vector) typespec (modifier (eql nil)))
  (compose-bytes data))

(defmethod decode-data ((data vector) typespec (modifier (eql :array)))
  data)

(defmethod decode-data ((data vector) (typespec (eql :character)) (modifier (eql nil)))
  (code-char (logior (ash (elt data 0) 8)
                     (elt data 1))))

(defmethod decode-data ((data vector) (typespec (eql :character)) (modifier (eql :array)))
  (octets-to-string data
                    :external-format (make-external-format :utf-16
                                                           :little-endian nil)))

(defmethod decode-data ((data vector) (typespec (eql :string)) (modifier (eql nil)))
  (decode-data data :character :array))

(defmethod decode-data ((data vector) (typespec (eql :float)) (modifier (eql nil)))
  (decode-float32 (compose-bytes data)))

(defmethod decode-data ((data vector) (typespec (eql :double)) (modifier (eql nil)))
  (decode-float64 (compose-bytes data)))

(defmethod decode-data ((data vector) (typespec (eql :float)) (modifier (eql :array)))
  (mapcar #'decode-float32 data))

(defmethod decode-data ((data vector) (typespec (eql :double)) (modifier (eql :array)))
  (mapcar #'decode-float64 data))

(defmethod encode-data (data (typespec (eql :raw)))
  (encode-data (write-to-string data) typespec))

(defmethod encode-data ((data integer) (size integer))
  (if (zerop data)
    (make-array size
                :initial-element 0
                :element-type '(unsigned-byte 8))
    (iter
      (for shift from 0 to (1- size))
      (with result = (make-array size
                                 :initial-element 0
                                 :fill-pointer 0
                                 :element-type '(unsigned-byte 8)))
      (finally (progn (setf (fill-pointer result) size)
                      (return (reverse result))))
      (vector-push-extend (ldb (byte 8 (* shift 8)) data)
                          result))))

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
    (t (error "Can't possibly imagine how am I supposed to convert ~S to something." data))))

(defun encode-value (value)
  (if (listp value)
    (destructuring-bind (modifier data) value
      (encode-data data modifier))
    (encode-data value nil)))


;;; Composite types

(defmacro define-composite-type (name &body fields)
  `(setf (get ,name :structure) ',(mapcar #'first fields)
         (get ,name :size)      (calculate-composite-type-size ',fields)))

(defun composite-type-structure (name)
  (get name :structure))

(defun calculate-composite-type-size (fields)
  (reduce #'+
          fields
          :key (compose #'get-type-size #'first)))

(defun split-vector (structure vector)
  (when structure
    (let* ((typespec (first structure))
           (size (get-type-size typespec)))
      (cons (cons typespec (subseq vector 0 size))
            (split-vector (rest structure)
                          (subseq vector size))))))

(define-composite-type :chunk-bulk-metadata
  (:integer chunk-x)
  (:integer chunk-z)
  ((:unsigned :short) primary-bit-map)
  ((:unsigned :short) add-bit-map))

(defmethod decode-data ((data vector)
                        (typespec (eql :chunk-bulk-metadata))
                        (modifier (eql nil)))
  (iter
    (for (type . field) in (split-vector (composite-type-structure typespec) data))
    (collecting
     (if (listp type)
       (decode-data field (second type) (first type))
       (decode-data field type nil)))))

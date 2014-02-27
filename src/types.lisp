(in-package :jams)

;;; ***************************************************************************************
;;; General stuff
;;; ***************************************************************************************

(defgeneric decode-data (data typespec modifier))
(defgeneric encode-data (data typespec))

;;; ***************************************************************************************
;;; Basic types
;;; ***************************************************************************************

(defmacro define-simple-type (name size)
  `(setf (get ,name :size) ,size))

(defun get-type-size (typespec)
  (aif (get (if (not (listp typespec))
              typespec
              (second typespec))
            :size)
    it
    0))

(define-simple-type :unsigned-byte    1)
(define-simple-type :byte             1)

(define-simple-type :unsigned-short   2)
(define-simple-type :short            2)

(define-simple-type :unsigned-integer 4)
(define-simple-type :integer          4)

(define-simple-type :unsigned-long    8)
(define-simple-type :long             8)

(define-simple-type :float            4)
(define-simple-type :double           8)

(define-simple-type :length-prefix    2)
(define-simple-type :character        2)

(define-simple-type :bool             1)

;;; ---------
;;; Decoding

(defun two-bytes-to-fixnum (vector)
  (let ((unsigned 0))
    (setf (ldb (byte 8 0) unsigned) (svref vector 0))
    (print (svref vector 0))
    (print unsigned)
    (setf (ldb (byte 8 8) unsigned) (svref vector 1))
    (print (svref vector 1))
    (print unsigned)
    (logior unsigned
            (- (mask-field (byte 1 15) unsigned)))))

(defun bytes-to-fixnum (vector length)
  (let ((unsigned 0))
    (iter
      (for byte from 0 below length)
      (after-each
       (setf (ldb (byte 8 (* byte 8))
                  unsigned)
             (svref vector (- length byte 1)))))
    (logior unsigned
            (- (mask-field (byte 1 (1- (* length 8)))
                           unsigned)))))

(defun compose-bytes (bytes)
  (loop for position from (1- (length bytes)) downto 0
        for shift = 0 then (+ 8 shift)
        for byte = (elt bytes position)
        for result = byte
          then (logior (ash byte shift) result)
        finally (return result)))

;;; Integers

(defmethod decode-data ((data vector) (typespec (eql :length-prefix)) (modifier (eql nil)))
  (compose-bytes data))

;; Signed

(defmethod decode-data ((data vector) (typespec (eql :byte)) (modifier (eql nil)))
  (bytes-to-fixnum data (get-type-size typespec)))

(defmethod decode-data ((data vector) (typespec (eql :short)) (modifier (eql nil)))
  (bytes-to-fixnum data (get-type-size typespec)))

(defmethod decode-data ((data vector) (typespec (eql :integer)) (modifier (eql nil)))
  (bytes-to-fixnum data (get-type-size typespec)))

(defmethod decode-data ((data vector) (typespec (eql :long)) (modifier (eql nil)))
  (bytes-to-fixnum data (get-type-size typespec)))

;; Unsigned

(defmethod decode-data ((data vector) (typespec (eql :unsigned-byte)) (modifier (eql nil)))
  data)

(defmethod decode-data ((data vector) (typespec (eql :unsigned-short)) (modifier (eql nil)))
  (compose-bytes data))

(defmethod decode-data ((data vector) (typespec (eql :unsigned-integer)) (modifier (eql nil)))
  (compose-bytes data))

(defmethod decode-data ((data vector) (typespec (eql :unsigned-long)) (modifier (eql nil)))
  (compose-bytes data))

;;; Floats

(defmethod decode-data ((data vector) (typespec (eql :float)) (modifier (eql nil)))
  (decode-float32 (compose-bytes data)))

(defmethod decode-data ((data vector) (typespec (eql :double)) (modifier (eql nil)))
  (decode-float64 (compose-bytes data)))

(defmethod decode-data ((data vector) (typespec (eql :float)) (modifier (eql :array)))
  (mapcar #'decode-float32 data))

(defmethod decode-data ((data vector) (typespec (eql :double)) (modifier (eql :array)))
  (mapcar #'decode-float64 data))

;;; Strings

(defmethod decode-data ((data vector) (typespec (eql :character)) (modifier (eql nil)))
  (code-char (logior (ash (elt data 0) 8)
                     (elt data 1))))

(defmethod decode-data ((data vector) (typespec (eql :character)) (modifier (eql :array)))
  (octets-to-string data
                    :external-format (make-external-format :utf-16
                                                           :little-endian nil)))

(defmethod decode-data ((data vector) (typespec (eql :string)) (modifier (eql nil)))
  (decode-data data :character :array))

;;; Boolean

(defmethod decode-data ((data vector) (typespec (eql :bool)) (modifier (eql nil)))
  (switch ((svref data 0) :test #'=)
    (0 nil)
    (1 t)))

;;; Byte-arrays

(defmethod decode-data ((data vector) (typespec (eql :byte)) (modifier (eql :array)))
  data)

;;; --------
;;; Encoding

(defun encode-number (number size)
  (if (zerop number)
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
      (vector-push-extend (ldb (byte 8 (* shift 8)) number)
                          result))))

;;; Raw

(defmethod encode-data (data typespec)
  (encode-data (write-to-string data) typespec))

;;; Integers

;; Signed

(defmethod encode-data ((data number) (typespec (eql :byte)))
  (encode-number data (get-type-size typespec)))

(defmethod encode-data ((data number) (typespec (eql :short)))
  (encode-number data (get-type-size typespec)))

(defmethod encode-data ((data number) (typespec (eql :integer)))
  (encode-number data (get-type-size typespec)))

(defmethod encode-data ((data number) (typespec (eql :long)))
  (encode-number data (get-type-size typespec)))

(defmethod encode-data ((data vector) (typespec (eql :byte)))
  (reduce (curry #'concatenate 'vector)
          data
          :key #'(lambda (elt) (encode-data elt typespec))))

(defmethod encode-data ((data vector) (typespec (eql :short)))
  (reduce (curry #'concatenate 'vector)
          data
          :key #'(lambda (elt) (encode-data elt typespec))))

(defmethod encode-data ((data vector) (typespec (eql :integer)))
  (reduce (curry #'concatenate 'vector)
          data
          :key #'(lambda (elt) (encode-data elt typespec))))

(defmethod encode-data ((data vector) (typespec (eql :long)))
  (reduce (curry #'concatenate 'vector)
          data
          :key #'(lambda (elt) (encode-data elt typespec))))

;; Unsigned

(defmethod encode-data ((data number) (typespec (eql :unsigned-byte)))
  (encode-number data (get-type-size typespec)))

(defmethod encode-data ((data number) (typespec (eql :unsigned-short)))
  (encode-number data (get-type-size typespec)))

(defmethod encode-data ((data number) (typespec (eql :unsigned-integer)))
  (encode-number data (get-type-size typespec)))

(defmethod encode-data ((data number) (typespec (eql :unsigned-long)))
  (encode-number data (get-type-size typespec)))

(defmethod encode-data ((data vector) (typespec (eql :unsigned-byte)))
  (reduce (curry #'concatenate 'vector)
          data
          :key #'(lambda (elt) (encode-data elt typespec))))

(defmethod encode-data ((data vector) (typespec (eql :unsigned-short)))
  (reduce (curry #'concatenate 'vector)
          data
          :key #'(lambda (elt) (encode-data elt typespec))))

(defmethod encode-data ((data vector) (typespec (eql :unsigned-integer)))
  (reduce (curry #'concatenate 'vector)
          data
          :key #'(lambda (elt) (encode-data elt typespec))))

(defmethod encode-data ((data vector) (typespec (eql :unsigned-long)))
  (reduce (curry #'concatenate 'vector)
          data
          :key #'(lambda (elt) (encode-data elt typespec))))

;;; Floats

(defmethod encode-data ((data float) (typespec (eql :float)))
  (encode-data (encode-float32 data) :integer))

(defmethod encode-data ((data float) (typespec (eql :double)))
  (encode-data (encode-float64 data) :long))

;;; Strings

(defmethod encode-data ((data string) (typespec (eql :raw)))
  (string-to-octets data
                    :external-format (make-external-format :utf-16
                                                           :little-endian nil)))

(defmethod encode-data ((data string) (typespec (eql :string)))
  (concatenate 'vector
               (encode-data (length data) :short)
               (encode-data data :raw)))

;;; Byte arrays

(defmethod encode-data ((data vector) (typespec (eql :raw)))
  data)

(defmethod encode-data ((data vector) (typespec (eql :byte-array)))
  (concatenate 'vector
               (encode-data (length data) :short)
               (encode-data data :raw)))

;;; Booleans

(defmethod encode-data ((data symbol) (typespec (eql :bool)))
  (cond
    ((eql data nil) #(0))
    ((eql data t)   #(1))))

(defun encode-value (value typespec)
  (if (listp typespec)
    (encode-data value (second typespec))
    (encode-data value typespec)))


;;; ***************************************************************************************
;;; Composite types
;;; ***************************************************************************************

(define-condition Invalid-structure ()
  ((required-structure :initarg :required-structure)
   (given-data :initarg :given-data)))

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

(defmethod encode-data :around ((data list) typespec)
  (let ((structure (composite-type-structure typespec)))
    (unless (= (length structure) (length data))
      (error 'Invalid-structure
             :required-structure structure
             :given-data data))
    (call-next-method)))

(defmethod encode-data ((data list) typespec)
  (mapcar #'(lambda (elt field-type)
              (encode-data elt field-type))
          data
          (composite-type-structure typespec)))

(defmethod decode-data ((data vector) typespec (modifier (eql nil)))
  (iter
    (for field-type in (composite-type-structure typespec))
    (for field-type-size next (get-type-size field-type))
    (for shift first 0 then (+ shift field-type-size))
    (collecting (decode-data (subseq-shift data 0 field-type-size shift)
                             field-type
                             nil))))

(defmethod decode-data ((data vector) typespec (modifier (eql :array)))
  (let ((data-length (length data))
        (type-size (get-type-size typespec)))
    (iter (for i below (floor (/ data-length type-size)))
      (collecting (decode-data (subseq data i (+ i type-size))
                               typespec
                               nil)))))

(defmethod encode-data ((data vector) typespec)
  (apply #'concatenate 'vector
         (apply #'append
                (map 'list #'(lambda (elt) (encode-data elt typespec))
                     data))))

(define-composite-type :chunk-bulk-metadata
  (:integer chunk-x)
  (:integer chunk-z)
  (:unsigned-short primary-bit-map)
  (:unsigned-short add-bit-map))

(in-package :jams)

;;; I've took some ideas from binary-types but most of the code was
;;; written from scratch. No, I can't just use binary types because it
;;; lacks some important features that would require a lot more work
;;; to implement so I decided to write types coder by myself. And no,
;;; this is *not* reinvention of the bicycle. This specific code
;;; suites my task (almost) perfectly and it is also some kind of
;;; exercise for me.
;;;
;;; Anyway, don't take it seriously.

;;; ***************************************************************************************
;;; General stuff
;;; ***************************************************************************************

(defvar *binary-types* (make-hash-table))

(defclass Binary-type ()
  ((name :initarg :name
         :accessor binary-type-name)
   (size :initarg :size
         :accessor binary-type-size)))

(defun get-type (name)
  (gethash name *binary-types*))

(defun (setf get-type) (new-value name)
  (check-type new-value Binary-type)
  (setf (gethash name *binary-types*) new-value))

(defmacro define-binary-type (type name size)
  `(setf (get-type ',name)
         (make-instance ',type
                        :name ',name
                        :size ,size)))

(defgeneric decode-data (typespec data))
(defgeneric encode-data (typespec data))

;;; ***************************************************************************************
;;; Basic types
;;; ***************************************************************************************

(defclass Basic-type (Binary-type) ())

(defclass Signed-integer (Basic-type) ())
(defclass Unsigned-integer (Basic-type) ())
(defclass Float (Basic-type) ())
(defclass Character (Basic-type) ())
(defclass Boolean (Basic-type) ())

(defmacro define-signed-integer (name size)
  `(define-binary-type Signed-integer ,name ,size))

(defmacro define-unsigned-integer (name size)
  `(define-binary-type Unsigned-integer ,name ,size))

(defmacro define-float (name size)
  `(define-binary-type Float ,name ,size))

(defmacro define-character (name size)
  `(define-binary-type Character ,name ,size))

(defmacro define-boolean (name size)
  `(define-binary-type Boolean ,name ,size))

(define-signed-integer s1 1)
(define-signed-integer s2 2)
(define-signed-integer s4 4)
(define-signed-integer s8 8)

(define-unsigned-integer u1 1)
(define-unsigned-integer u2 2)
(define-unsigned-integer u4 4)
(define-unsigned-integer u8 8)

(define-float f4 4)
(define-float f8 8)

(define-unsigned-integer length-prefix 2)

(define-character char 2)

(define-boolean bool 1)

;;; ---------
;;; Decoding

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
  (iter
    (for position from (1- (length bytes)) downto 0)
    (for byte next (elt bytes position))
    (for shift first 0 then (+ 8 shift))
    (for result first byte
         then (logior (ash byte shift) result))
    (finally (return result))))

(defmethod decode-data ((typespec symbol) data)
  (decode-data (get-type typespec) data))

;;; Integers

;; Signed

(defmethod decode-data ((typespec Signed-integer) (data vector))
  (bytes-to-fixnum data (binary-type-size typespec)))

;; Unsigned

(defmethod decode-data ((typespec Unsigned-integer) (data vector))
  (compose-bytes data))

;;; Floats

(defmethod decode-data ((typespec Float) (data vector))
  (switch ((binary-type-size typespec) :test #'=)
    (4 (decode-float32 (get-type 'u4) (compose-bytes data)))
    (8 (decode-float64 (get-type 'u8) (compose-bytes data)))))

;;; Strings

(defmethod decode-data ((typespec Character) (data vector))
  (code-char (logior (ash (elt data 0) 8)
                     (elt data 1))))

(defmethod decode-data ((typespec String) (data vector))
  (octets-to-string data
                    :external-format (make-external-format :utf-16
                                                           :little-endian nil)))

;;; Boolean

(defmethod decode-data ((typespec Boolean) (data vector))
  (switch ((svref data 0) :test #'=)
    (0 nil)
    (1 t)))

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

(defmethod encode-data ((typespec symbol) data)
  (encode-data (get-type typespec) data))

;;; Raw

(defmethod encode-data (typespec data)
  (encode-data (write-to-string data) typespec))

;;; Integers

;; Signed

(defmethod encode-data ((typespec Signed-integer) (data number))
  (encode-number data (binary-type-size typespec)))

;; Unsigned

(defmethod encode-data ((typespec Unsigned-integer) (data number))
  (encode-number data (binary-type-size typespec)))

;;; Floats

(defmethod encode-data ((typespec Float) (data float))
  (switch ((binary-type-size typespec) :test #'=)
    (4 (encode-data (encode-float32 data)))
    (8 (encode-data (encode-float64 data)))))

;;; Strings

(defmethod encode-data ((typespec String) (data string))
  (string-to-octets data
                    :external-format (make-external-format :utf-16
                                                           :little-endian nil)))

;;; Booleans

(defmethod encode-data ((typespec Boolean) (data symbol))
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

;;; -------------
;;; Basic stuff

(defclass Composite-type (Binary-type)
  ((structure :initarg :structure
              :accessor composite-type-structure)))

(define-condition Invalid-structure (error)
  ((required-structure :initarg :required-structure)
   (given-data :initarg :given-data)))

(defmacro define-composite-type (name &body fields)
  `(progn (define-binary-type Composite-type ,name
            (calculate-composite-type-size ',fields))
          (setf (composite-type-structure (get-type ',name))
                ',(mapcar #'first fields))))

(defun calculate-composite-type-size (fields)
  (reduce #'+
          fields
          :key (compose #'binary-type-size #'get-type #'first)))

(defmethod encode-data :around ((typespec Composite-type) (data list))
  (let ((structure (composite-type-structure typespec)))
    (if (not (= (length structure) (length data)))
      (error 'Invalid-structure
             :required-structure structure
             :given-data data)
      (call-next-method))))

(defmethod encode-data ((typespec Composite-type) (data list))
  (apply #'concatenate 'vector
         (mapcar #'(lambda (field-type elt)
                     (encode-data field-type elt))
                 (composite-type-structure typespec)
                 data)))

(defmethod decode-data ((typespec Composite-type) (data vector))
  (iter
    (for field-type in (composite-type-structure typespec))
    (for field-type-size next (binary-type-size (get-type field-type)))
    (with shift = 0)
    (collecting (decode-data field-type
                             (subseq-shift data 0 field-type-size shift)))
    (setf shift (+ shift field-type-size))))

;;; ------
;;; Types

(define-composite-type chunk-bulk-metadata
  (s4 chunk-x)
  (s4 chunk-z)
  (u2 primary-bit-map)
  (u2 add-bit-map))

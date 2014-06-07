(in-package :jams)

;;; I've took some ideas from binary-types but most of the code was
;;; written from scratch. No, I can't just use binary types because it
;;; lacks some important features that would require a lot more work
;;; to implement so I decided to write types coder by myself. And no,
;;; this is *not* reinvention of the wheel. This specific code
;;; suites my task (almost) perfectly and it is also some kind of
;;; exercise for me.
;;;
;;; Anyway, don't take it seriously and *don't* use it elsewhere as it
;;; may be slow, buggy and inefficient.

;;; **************************************************************************
;;;  General stuff
;;; **************************************************************************

(defvar *binary-types* (make-hash-table))

(defclass Binary-type ()
  ((name :initarg :name
         :accessor binary-type-name)
   (size :initarg :size
         :accessor binary-type-size)
   (modifiers :initarg :modifiers
              :accessor binary-type-modifiers)))

(defclass Binary-Array ()
  ((prefix-type :initarg :prefix-type
                :accessor binary-array-prefix-type)
   (element-type :initarg :element-type
                 :accessor binary-array-element-type)))

(defun modifier= (mod-1 mod-2)
  (eql (if (listp mod-1)
         (first mod-1)
         mod-1)
       mod-2))

(defun binary-type-modifier (type modifier)
  (find-if (curry #'modifier= modifier)
           (binary-type-modifiers type)))

(defun has-modifier-p (type modifier)
  (when (binary-type-modifier type modifier)
    t))

(defun get-type (name)
  (gethash name *binary-types*))

(defun (setf get-type) (new-value name)
  (check-type new-value (or Binary-type Binary-array))
  (setf (gethash name *binary-types*) new-value))

(defmacro define-binary-type (type name size &body arguments)
  `(setf (get-type ',name)
         (make-instance ',type
                        :name ',name
                        :size ,size
                        ,@arguments)))

(defmacro define-binary-array (name prefix-type element-type &body arguments)
  `(setf (get-type ',name)
         (make-instance 'Binary-Array
                        :prefix-type ',prefix-type
                        :element-type ',element-type
                        ,@arguments)))

(defgeneric decode-binary-type (type data))
(defgeneric encode-binary-type (type data))
(defgeneric read-binary-type (type stream))

;;; **************************************************************************
;;;  Basic types
;;; **************************************************************************

(defclass Basic-type (Binary-type) ())

(defclass Signed-integer (Basic-type) ())
(defclass Unsigned-integer (Basic-type) ())
(defclass Float (Basic-type) ())
(defclass Character (Basic-type) ())
(defclass Boolean (Basic-type) ())

(defclass VarInt (Signed-integer) ())

(defmacro define-signed-integer (name size)
  `(define-binary-type Signed-integer ,name ,size))

(defmacro define-unsigned-integer (name size)
  `(define-binary-type Unsigned-integer ,name ,size))

(defmacro define-varint (name)
  `(define-binary-type VarInt ,name 4) ; VarInts have variable size so type size but
                                       ; the size stored here is maximum VarInt size.
  )

(defmacro define-float (name size)
  `(define-binary-type Float ,name ,size))

(defmacro define-character (name size)
  `(define-binary-type Character ,name ,size))

(defmacro define-string (name prefix-type)
  `(define-binary-array ,name ,prefix-type char))

(defmacro define-boolean (name size)
  `(define-binary-type Boolean ,name ,size))

(define-varint VarInt)

(define-signed-integer s1 1)
(define-signed-integer s2 2)
(define-signed-integer s4 4)
(define-signed-integer s8 8)

(define-unsigned-integer u1 1)
(define-unsigned-integer u2 2)
(define-unsigned-integer u4 4)
(define-unsigned-integer u8 8)
(define-unsigned-integer u16 16)

(define-float f4 4)
(define-float f8 8)

(define-character char 1)
(define-string string VarInt)

(define-boolean bool 1)

;;; ---------
;;; Decoding

(defun bytes-to-fixnum (vector length &optional (octet-size 8))
  (let ((unsigned 0))
    (iter
      (for byte from 0 below length)
      (after-each
       (setf (ldb (byte octet-size (* byte octet-size))
                  unsigned)
             (svref vector (- length byte 1)))))
    (logior unsigned
            (- (mask-field (byte 1 (1- (* length octet-size)))
                           unsigned)))))

(defun compose-bytes (bytes &optional (octet-size 8))
  (iter
    (for position from (1- (length bytes)) downto 0)
    (for byte next (elt bytes position))
    (for shift first 0 then (+ octet-size shift))
    (for result first byte
         then (logior (ash byte shift) result))
    (finally (return result))))

(defmethod decode-binary-type ((type symbol) data)
  (decode-binary-type (get-type type) data))

;;; Integers

;; Signed

(defmethod decode-binary-type ((type Signed-integer) (data vector))
  (bytes-to-fixnum data (binary-type-size type)))

(defmethod decode-binary-type ((type VarInt) (data vector))
  (bytes-to-fixnum data (binary-type-size type) 7))

;; Unsigned

(defmethod decode-binary-type ((type Unsigned-integer) (data vector))
  (compose-bytes data))

;;; Floats

(defmethod decode-binary-type ((type Float) (data vector))
  (switch ((binary-type-size type) :test #'=)
    (4 (decode-float32 (compose-bytes data)))
    (8 (decode-float64 (compose-bytes data)))))

;;; Strings

(defmethod decode-binary-type ((type Character) (data vector))
  (code-char (svref data 0)))

(defmethod decode-binary-type ((type String) (data vector))
  (octets-to-string data :external-format :utf-8))

;;; Boolean

(defmethod decode-binary-type ((type Boolean) (data vector))
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

(defmethod encode-binary-type ((type symbol) data)
  (encode-binary-type (get-type type) data))

;;; Raw

(defmethod encode-binary-type (type data)
  (encode-binary-type (write-to-string data) type))

;;; Integers

;; Signed

(defmethod encode-binary-type ((type Signed-integer) (data number))
  (encode-number data (binary-type-size type)))

;; Unsigned

(defmethod encode-binary-type ((type Unsigned-integer) (data number))
  (encode-number data (binary-type-size type)))

;;; Floats

(defmethod encode-binary-type ((type Float) (data float))
  (switch ((binary-type-size type) :test #'=)
    (4 (encode-binary-type (encode-float32 data)))
    (8 (encode-binary-type (encode-float64 data)))))

;;; Strings

(defmethod encode-binary-type ((type String) (data string))
  (string-to-octets data
                    :external-format (make-external-format :utf-16
                                                           :little-endian nil)))

;;; Booleans

(defmethod encode-binary-type ((type Boolean) (data symbol))
  (cond
    ((eql data nil) #(0))
    ((eql data t)   #(1))))

(defun encode-value (value type)
  (if (listp type)
    (encode-binary-type value (second type))
    (encode-binary-type value type)))

;;; **************************************************************************
;;;  Composite types
;;; **************************************************************************

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

(defmethod encode-binary-type :around ((type Composite-type) (data list))
  (let ((structure (composite-type-structure type)))
    (if (not (= (length structure) (length data)))
      (error 'Invalid-structure
             :required-structure structure
             :given-data data)
      (call-next-method))))

(defmethod encode-binary-type ((type Composite-type) (data list))
  (apply #'concatenate 'vector
         (mapcar #'(lambda (field-type elt)
                     (encode-binary-type field-type elt))
                 (composite-type-structure type)
                 data)))

(defmethod decode-binary-type ((type Composite-type) (data vector))
  (with-input-from-sequence (data-stream data)
    (iter
      (for field-type in (composite-type-structure type))
      (for field-type-size next (binary-type-size (get-type field-type)))
      (collecting (decode-binary-type field-type (read-bytes data-stream field-type-size))))))

;;; ------
;;; Types

(define-composite-type chunk-bulk-metadata
  (s4 chunk-x)
  (s4 chunk-z)
  (u2 primary-bit-map)
  (u2 add-bit-map))

;;; **************************************************************************
;;;  Reading types from streams
;;; **************************************************************************

(defmethod read-binary-type ((type symbol) stream)
  (read-binary-type (get-type type) stream))

;; Basic types

(defmethod read-binary-type ((type Basic-type) stream)
  (decode-binary-type type
                      (read-bytes stream
                                  (binary-type-size type))))

(defmethod read-binary-type ((type VarInt) stream)
  (let* ((size (binary-type-size type))
         (buffer (make-array size :initial-element 0)))
    (decode-binary-type
      type
      (dotimes (i size buffer)
        (let ((byte (read-byte stream nil 0)))
          (format t "Buffer: ~A~%Byte: ~A~%"
                  buffer
                  (write-to-string byte :base 2))
          (if (not (zerop (logand (ash 1 8) byte)))
            (return buffer)
            (setf (svref buffer (- (1- size) i))
                  (logand #b01111111 ; Mask for dropping most significant bit
                          byte))))))))

(defmethod read-binary-type ((type Binary-array) stream)
  (let* ((length (read-binary-type stream (binary-array-prefix-type type))))
    (iter
      (repeat length)
      (collecting (read-binary-type stream (binary-array-element-type type))))))

;; Composite types

(defmethod read-binary-type ((type Composite-type) stream)
  (iter
    (for field in (composite-type-structure type))
    (for field-length next (binary-type-size field))
    (collecting (read-binary-type stream (get-type field)))))

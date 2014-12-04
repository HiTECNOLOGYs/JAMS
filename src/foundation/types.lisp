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
;;;  Foundation
;;; **************************************************************************

;; ----------------
;; Basic classes, functions and macros

(defvar *binary-types* (make-hash-table))

(defclass Binary-type ()
  ((name :initarg :name
         :accessor binary-type-name)
   (size :initarg :size
         :accessor binary-type-size)))

(defclass Binary-Array ()
  ((prefix-type :initarg :prefix-type
                :accessor binary-array-prefix-type)
   (element-type :initarg :element-type
                 :accessor binary-array-element-type)))

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

(defmacro define-binary-array (type name prefix-type element-type &body arguments)
  `(setf (get-type ',name)
         (make-instance ',type
                        :prefix-type ',prefix-type
                        :element-type ',element-type
                        ,@arguments)))

(defmethod binary-type-size ((type symbol))
  (binary-type-size (get-type type)))

;; ----------------
;; Reading/Writng

(defgeneric write-binary-type (type data stream)
  #+nil
  (:method (type data stream)
   ;; Can't encode — encode as string!
   (write-binary-type type (write-to-string data) stream))
  (:method ((type symbol) data stream)
   (write-binary-type (get-type type) data stream)))

(defgeneric read-binary-type (type stream)
  (:method ((type symbol) stream)
   (read-binary-type (get-type type) stream)))

;;; **************************************************************************
;;;  Binary magic
;;; **************************************************************************

(defun bytes-to-fixnum (vector length &optional (octet-size 8))
  (let ((unsigned 0))
    (iter
      (for i from 0 below length)
      (after-each
        (setf (ldb (byte octet-size (* i octet-size))
                   unsigned)
              (aref vector i))))
    (logior unsigned
            (- (mask-field (byte 1 (1- (* length octet-size)))
                           unsigned)))))

(defun compose-bytes (bytes &optional (octet-size 8))
  (iter
    (for i from (1- (length bytes)) downto 0)
    (for byte next (aref bytes i))
    (for shift first 0 then (+ octet-size shift))
    (for result first byte
         then (logior (ash byte shift) result))
    (finally (return result))))

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

;;; **************************************************************************
;;;  Basic types' classes
;;; **************************************************************************

(defclass Basic-type (Binary-type) ())

(defclass Composite-type (Binary-type)
  ((structure :initarg :structure
              :accessor composite-type-structure)))

;;; **************************************************************************
;;;  Basic types
;;; **************************************************************************

(defmethod read-binary-type ((type Basic-type) stream)
  (read-bytes (binary-type-size type) stream))

(defmethod read-binary-type ((type Binary-array) stream)
  (let* ((length (read-binary-type (binary-array-prefix-type type) stream)))
    (iter
      (repeat length)
      (collecting (read-binary-type (binary-array-element-type type) stream)))))

(defmethod write-binary-type ((type Basic-type) data stream)
  (write-bytes (if (vectorp data) data (vector data)) stream))

(defmethod write-binary-type ((type Binary-array) data stream)
  (let ((prepared-data (if (vectorp data) data (vector data))))
    (write-binary-type (binary-array-prefix-type type) (length prepared-data) stream)
    (write-bytes prepared-data stream)))

;;; **************************************************************************
;;;  Composite types
;;; **************************************************************************

(defmethod initialize-instance :after ((instance Composite-type) &rest initargs)
  (declare (ignore initargs))
  (with-slots (size) instance
    (when (null size)
      (setf size (caclculate-composite-type-size instance)))))

(defun caclculate-composite-type-size (type)
  (reduce #'+ (composite-type-structure type) :key #'binary-type-size))

(define-condition Invalid-structure (error)
  ((required-structure :initarg :required-structure)
   (given-data :initarg :given-data)))

(defmacro define-composite-type (name &body fields)
  `(progn
     (defclass ,name ()
       ,(mapcar #'second fields))
     (define-binary-type Composite-type ,name nil
       :structure ',(mapcar #'first fields))))

(defmethod write-binary-type :around ((type Composite-type) (data list) stream)
  (let ((structure (composite-type-structure type)))
    (if (= (length structure) (length data))
      (call-next-method)
      (error 'Invalid-structure
             :required-structure structure
             :given-data data))))

(defmethod write-binary-type ((type Composite-type) (data list) stream)
  (iter
    (for field in (composite-type-structure type))
    (for value in data)
    (write-binary-type field value stream)))

(defmethod read-binary-type ((type Composite-type) stream)
  (mapcar #'(lambda (field)
              (read-binary-type field stream))
          (composite-type-structure type)))

;;; **************************************************************************
;;;  Unsigned integer
;;; **************************************************************************

(defclass Unsigned-integer (Basic-type) ())

(defmacro define-unsigned-integer (name size)
  `(define-binary-type Unsigned-integer ,name ,size))

(define-unsigned-integer u1 1)
(define-unsigned-integer u2 2)
(define-unsigned-integer u4 4)
(define-unsigned-integer u8 8)
(define-unsigned-integer u16 16)

(defmethod read-binary-type ((type Unsigned-integer) stream)
  (compose-bytes (call-next-method)))

(defmethod write-binary-type ((type Unsigned-integer) (data number) stream)
  (call-next-method type (encode-number data (binary-type-size type)) stream))

;;; **************************************************************************
;;;  Signed integer
;;; **************************************************************************

(defclass Signed-integer (Basic-type) ())

(defmacro define-signed-integer (name size)
  `(define-binary-type Signed-integer ,name ,size))

(define-signed-integer s1 1)
(define-signed-integer s2 2)
(define-signed-integer s4 4)
(define-signed-integer s8 8)

(defmethod read-binary-type ((type Signed-integer) stream)
  (bytes-to-fixnum (call-next-method) (binary-type-size type)))

(defmethod write-binary-type ((type Signed-integer) (data number) stream)
  (call-next-method type (encode-number data (binary-type-size type)) stream))

;;; **************************************************************************
;;;  VarNums
;;; **************************************************************************

(defclass Var-Num (Signed-integer) ())

(defmacro define-var-num (name max-size)
  `(define-binary-type Var-Num ,name ,max-size) ; VarNums have variable size so type value
                                                ; stored here represents maximal VarNum size.
  )

(define-var-num Var-Int 4)
(define-var-num Var-Long 8)

(defmethod read-binary-type ((type Var-Num) stream)
  (with-slots (size) type
    (let ((buffer (make-array size
                              :initial-element 0
                              :fill-pointer 0
                              :element-type '(unsigned-byte 8))))
      (dotimes (i size buffer)
        (let ((byte (read-byte stream nil 0)))
          (vector-push (ldb (byte 7 0) byte) buffer)
          (when (zerop (ldb (byte 1 7) byte))
            (setf (fill-pointer buffer) size)
            (return (if (emptyp buffer)
                      buffer
                      (bytes-to-fixnum buffer size 7)))))))))

(defmethod write-binary-type ((type Var-Num) (data number) stream)
  (iter
    (with number-length = (integer-length data))
    (for i from 0 to number-length by 7)
    (after-each
      (write-byte (logior (ldb (byte 7 i) data)
                          (if (< (+ i 7) number-length)
                            (ash 1 7)
                            (ash 0 7)))
                  stream))))

;;; **************************************************************************
;;;  Floating point numbers
;;; **************************************************************************

(defclass Float (Basic-type) ())

(defmacro define-float (name size)
  `(define-binary-type Float ,name ,size))

(define-float f4 4)
(define-float f8 8)

(defmethod read-binary-type ((type Float) stream)
  (switch ((binary-type-size type) :test #'=)
    (4 (decode-float32 (compose-bytes (call-next-method))))
    (8 (decode-float64 (compose-bytes (call-next-method))))))

(defmethod write-binary-type ((type Float) (data cl:float) stream)
  (switch ((binary-type-size type) :test #'=)
    (4 (write-binary-type 'u4 (encode-float32 data) stream))
    (8 (write-binary-type 'u8 (encode-float64 data) stream))))

;;; **************************************************************************
;;;  Characters
;;; **************************************************************************

(defclass Character (Basic-type) ())

(defmacro define-character (name size)
  `(define-binary-type Character ,name ,size))

(define-character char 1)

(defmethod read-binary-type ((type Character) stream)
  (code-char (call-next-method)))

(defmethod write-binary-type ((type Character) (data cl:character) stream)
  (call-next-method type (char-code data) stream))

;;; **************************************************************************
;;;  Strings
;;; **************************************************************************

(defclass String (Binary-array) ())

(defmacro define-string (name prefix-type)
  `(define-binary-array String ,name ,prefix-type Character))

(define-string string Var-Int)

(defmethod read-binary-type ((type String) stream)
  (octets-to-string (call-next-method) :external-format :utf-8))

(defmethod write-binary-type ((type String) (data cl:string) stream)
  (call-next-method type (string-to-octets data :external-format :utf-8) stream))

;;; **************************************************************************
;;;  Booleans
;;; **************************************************************************

(defclass Boolean (Basic-type) ())

(defmacro define-boolean (name size)
  `(define-binary-type Boolean ,name ,size))

(define-boolean bool 1)

(defmethod read-binary-type ((type Boolean) stream)
  (switch ((call-next-method) :test #'=)
    (0 nil)
    (1 t)))

(defmethod write-binary-type ((type Boolean) (data symbol) stream)
  (check-type data cl:boolean)
  (call-next-method type (if data 1 0) stream))

;;; **************************************************************************
;;;  Position
;;; **************************************************************************

(defclass Position (Unsigned-integer) ())

(defmacro define-position (name size)
  `(define-binary-type Position ,name ,size))

(define-position Position 8)

(defmethod read-binary-type ((type Position) stream)
  (let ((value (compose-bytes (call-next-method))))
    (list (ash value -38) ; X
          (logand (ash value -26) #xFFF) ; Y
          (ash (ash value 38) -38) ; Z
          )))

(defmethod write-binary-type ((type Position) (data list) stream)
  (destructuring-bind (x y z) data
    (call-next-method type
                      (encode-number (logior (ash (logand x #x3FFFFFF) 38)
                                             (ash (logand y #xFFF) 26)
                                             (logand z #x3FFFFFF))
                                     (binary-type-size type))
                      stream)))

;;; **************************************************************************
;;;  UUID
;;; **************************************************************************

(defclass UUID (Unsigned-integer) ())

(defmacro define-uuid (name size)
  `(define-binary-type UUID ,name ,size))

(define-uuid UUID 16)

(defmethod read-binary-type ((type UUID) stream)
  (compose-bytes (call-next-method)))

(defmethod write-binary-type ((type UUID) (data number) stream)
  (call-next-method type (encode-number data (binary-type-size type)) stream))

;;; **************************************************************************
;;;  Chunk bulk metadata
;;; **************************************************************************

(define-composite-type chunk-bulk-metadata
  (s4 chunk-x)
  (s4 chunk-z)
  (u2 primary-bit-map)
  (u2 add-bit-map))

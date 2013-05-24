(in-package :jams)

(defun compose-bytes (bytes)
  (loop for byte in bytes
        for shift = 0 then (+ 8 shift)
        for result = byte then (logior (ash byte shift) result)
        finally (return result)))

(defun bytes->number (bytes)
  (let* ((number-raw (compose-bytes bytes))
         (number-length (* 8
                           (ceiling (/ (integer-length number-raw)
                                       8))))
         (number (ldb (byte (1- number-length)
                            0)
                      number-raw))
         (sign (ldb (byte 1 (1- number-length))
                    number-raw)))
    (if (= 1 sign)
        (+ (- (expt 2 (1- number-length)))
           number)
        number)))

(defun convert (type data)
  (case type
    (:byte  (first data))
    (:short (bytes->number (reverse data)))
    (:integer (bytes->number (reverse data)))
    (:long (bytes->number (reverse data)))
    (:float (decode-float32 (compose-bytes (reverse data))))
    (:double (decode-float64 (compose-bytes (reverse data))))
    (:string (octets-to-string data
                               :external-format (make-external-format :utf-16
                                                                      :little-endian nil)))
    (:bool (= 1 (first data)))
    (:metadata "Uable decode metadata yet =(")))

(defun get-type-size (type)
  (case type
    (:byte 1)
    (:short 2)
    (:integer 4)
    (:long 8)
    (:float 4)
    (:double 8)
    (:string :prefix*2)
    (:bool 1)
    (:metadata :metadata)))

(defun split-number-to-bytes (number &optional (size 1))
  (if (zerop number)
      #(0)
      (loop
        for shift from 0 upto (1- size)
        for vector-position from (1- size) downto 0
        with result = (make-array (list size) 
                                  :initial-element 0
                                  :element-type '(unsigned-byte 8))
        do (setf (elt result vector-position)
                 (ldb (byte 8 (* 8 shift)) number))
        finally (return result))))

(defun encode-value (value)
  "`Value' MUST be valid."
  (destructuring-bind (type data) value
    (case type
      (:byte (split-number-to-bytes data (get-type-size type)))
      (:short (split-number-to-bytes data (get-type-size type)))
      (:integer (split-number-to-bytes data (get-type-size type)))
      (:long (split-number-to-bytes data (get-type-size type)))
      (:float (split-number-to-bytes (encode-float32 data) (get-type-size type)))
      (:double (split-number-to-bytes (encode-float64 data) (get-type-size type)))
      (:string (concatenate 'vector
                            (split-number-to-bytes (length data)
                                                   (get-type-size :short))
                            (string-to-octets data
                                              :external-format (make-external-format :utf-16
                                                                                     :little-endian nil))))
      (:bool (if (= 0 data)
                 #(0) #(1)))
      (:metadata #(0)))))

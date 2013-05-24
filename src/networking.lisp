(in-package :jams)

(defun receive-data (stream)
  (loop for byte = (read-byte stream nil nil)
        while byte
        collecting byte))

(defun listener (stream)
  (dolist (response (process-packet (receive-data stream)))
    (destructuring-bind (packet-id . data) response
      (when (and packet-id data)
        (write-sequence (encode-packet packet-id data)
                        stream)))))

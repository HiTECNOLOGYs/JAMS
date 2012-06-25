;;;; jams.lisp
;;;;
;;;; Author: Mark Fedurin <hitecnologys@gmail.com>
;;;; Description: Main file.

(asdf:operate 'asdf:load-op "usocket")

(defun login-handler (stream)
  "Simply handles connection event and prints received info."
  (print (loop with result
               for char = (read-char stream nil nil)
               while char
               if (not (eq #\Nul char))
                 do (push char result)
               finally (return (reverse result)))))

(defvar *port* 25565)

(defun start-login-test ()
  "Simple login test. You can run it and try to connect with Minecraft client. It will print some info."
  (usocket:socket-server "localhost" *port* #'login-handler))

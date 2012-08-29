(in-package :cl-irc)

(defmethod pong ((connection connection) (server string) &optional server2)
  (if server2
      (send-irc-message connection :pong server)
      (send-irc-message connection :pong server server2)))

(defmethod find-channel :around ((connection connection) channel)
  (or (call-next-method)
      (error "Could not find channel ~a on connection ~a" channel connection)))

(in-package :flybot)

(defun main-loop (connection)
  (handler-bind
      ((error (lambda (e)
		(format (client-stream connection) "~a ERROR caught by ~a: ~a~%" (format-time) 'main-loop e)
		(continue e))))
    (loop (with-simple-restart (continue "internal restart")
		  (unless (read-message connection) (return-from main-loop :done))))))
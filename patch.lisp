(in-package :cl-irc)

(defmethod pong ((connection connection) (server string) &optional server2)
  (if server2
      (send-irc-message connection :pong server)
      (send-irc-message connection :pong server server2)))

(defmethod find-channel :around ((connection connection) channel)
  (or (call-next-method)
      (error "Could not find channel ~a on connection ~a" channel connection)))

(in-package :flybot)

;;; FIXME: this deserves to be more than a patch.  CL-IRC handles most things well with hooks
;;;  but this handler-binding stuff is important.

(defun main-loop (connection)
  "Handle and dispatch messages on CONNECTION."
  (handler-bind
      ((no-such-reply
	(lambda (c)
	  ;; ignore it and keep going
	  (format (client-stream connection) "Unknown IRC numeric: ~d" (irc::reply-number c)) ; reply-number should be exp
	  (continue c)))
       (error
	(lambda (e)
	  (format (client-stream connection) "~a ERROR caught by ~a: ~a~%" (format-time) 'main-loop e)
	  (continue e))))
    (loop (with-simple-restart (continue "internal restart")
		  (unless (read-message connection) (return-from main-loop :done))))))

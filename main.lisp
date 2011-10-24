(in-package :flybot)

(defconstant command-character #\:)

(defpackage :bot-commands)

(defvar *debug-p* nil)

;; Change args to (a b c d)...?
(defmacro defcmd (name args &body body)
  (if (= 4 (length args))
      `(defun ,(intern (symbol-name name) :bot-commands) ,args ,@body)
      (error "ERROR: (defcmd ~a ~a ...) has an invalid number of arguments - commands have four."
	     name args)))

(defun alias (alias fn)
  (declare (type symbol alias fn))
;  (setf (symbol-function (intern (symbol-name alias) :bot-commands))
;	(symbol-function (intern (symbol-name fn) :bot-commands))))
  (setf (symbol-function alias) (symbol-function fn)))

(defvar *nickname* "Spacebar")
(defun nickname (name) (when (stringp name) (setf *nickname* name)))
(defvar *password* nil)
(defun password (pass) (when (stringp pass) (setf *password* pass)))
(defvar *username* nil)
(defun username (name) (when (stringp name) (setf *username* name)))
(defvar *realname* nil)
(defun realname (name) (when (stringp name) (setf *realname* name)))

(defvar *log* t)
(defun logfile (filename)
  (when (stringp filename)
    (setf *log* (open filename :direction :output :if-exists :append))))

(defvar *connections* '())

;;; Here follows Shit To Make Timers Work.  Should use an external library like oconnore's multi-timers,
;;;  but that's not in quicklisp at the moment and I'm not feeling that adventurous.

#-sbcl
(defvar *timers* '()
  "A list of pairs (universal-time function).  When the time of a pair passes, the function is executed
with no arguments and the pair is removed.")

#-sbcl
(defun register-timer (func seconds)
  "Register a function to run in SECONDS seconds from the time REGISTER-TIMER is called."
  (push (cons (+ (get-universal-time) seconds) func) *timers*))

#-sbcl
(defclass non-blocking-connection (irc:connection) ())

;; KLUDGE This is a pretty ugly way to do it.  Maybe edit the library to allow a :key non-blocking higher up.
#-sbcl
(defmethod irc::read-irc-message ((connection non-blocking-connection))
  (handler-case
      (let* ((msg-string (read-protocol-line-non-blocking connection))
	     (message (when msg-string (irc::create-irc-message msg-string))))
	(when message (setf (irc:connection message) connection))
	message)
    (end-of-file ())))

;; or make read-protocol-line a generic function too and specialize on it, but that's still pretty duplicatey
#-sbcl
(defun read-protocol-line-non-blocking (connection)
  (multiple-value-bind (buf buf-len)
      (irc::read-sequence-until (irc:network-stream connection)
				(make-array 1024 :element-type '(unsigned-byte 8) :fill-pointer t)
				'(10)
				:non-blocking t)
    (when (plusp buf-len)
      (setf (fill-pointer buf)
	    (or (position-if #'(lambda (x) (member x '(10 13)))
			     buf :from-end t :end buf-len)
		buf-len))
      (irc::try-decode-line buf irc::*default-incoming-external-formats*))))

#-sbcl
(defmethod irc:read-message-loop ((connection non-blocking-connection))
  (loop while (irc::connectedp connection)
     do (irc:read-message connection)
       (setf *timers* (remove-if
		       (lambda (x)
			 (when (< (car x) (get-universal-time))
			   (funcall (cdr x))
			   t))
		       *timers*))))

#+sbcl
(defun register-timer (func seconds)
  "Register a function to run in SECONDS seconds from the time REGISTER-TIMER is called."
  (sb-ext:schedule-timer (sb-ext:make-timer func) seconds))

;; Convenience wrapper around irc:connect.  (Making for four or so layers of wrappin internally...
(defun connect (&key
		(server "irc.freenode.net")
		(port :default)
		(nick *nickname*)
		(username (or *username* *nickname*))
		(realname (or *realname* *nickname*))
		(pass *password*)
		(logfile *log* log-p)
		channels)
  (let ((connection (irc:connect :connection-type #-sbcl 'non-blocking-connection ;; so that we can have a proper event loop.
				                  #+sbcl 'irc:connection ;; or just use sbcl's timers.
				 :server server
				 :port port
				 :nickname nick
				 :username username
				 :realname realname
				 :password pass
				 :logging-stream (if log-p
						     (open logfile :direction :input :if-exists :append)
						     logfile))))
    (push connection *connections*)
    (irc:add-hook connection 'irc::irc-privmsg-message 'command-dispatcher)
    (irc:add-hook connection 'irc::irc-privmsg-message 'log-privmsg)
    (irc:add-hook connection 'irc::irc-kick-message 'rejoin-hook)
    (irc:add-hook connection 'irc::ctcp-action-message 'log-action-message)
    (irc:add-hook connection 'irc::irc-rpl_welcome-message
		  (lambda (message)
		    (declare (ignore message))
		    (map nil (lambda (x)
			       (if (listp x)
				   (irc:join connection (first x) :password (second x)) ;; Join with password
				   (irc:join connection x)))
			 channels)))))

(defun clear-commands ()
  (delete-package :bot-commands)
  (make-package :bot-commands))

;; Numbers are made up, and this should be iterative or at least use an innerfunction to avoid
;; multiple >1024 checks... though, this is an edge case, and efficiency isn't that important.
#|
(defun buffer-privmsg (connection to text)
  (if (> (length text) 400)
      (if (> (length text) 1024)
	  (irc:privmsg connection to "Computed message exceeds 1024 characters.")
	  (progn
	    (irc:privmsg connection to (subseq text 0 400))
	    (buffer-privmsg connection to (subseq text 400))))
      (irc:privmsg connection to text)))
|#

(defun buffer-privmsg (connection to text)
  (if (> (length text) 400)
      (irc:privmsg connection to (concatenate 'string (subseq text 0 400) "..."))
      (irc:privmsg connection to text)))

(defun reply (sender dest connection text &rest args)
  (if (string= dest (irc:nickname (irc:user connection)))
      (buffer-privmsg connection sender (apply #'format nil text args))
      (buffer-privmsg connection dest (format nil "~a: ~?" sender text args))))

(defun action-msg (text)
  "Make a /me form message.  Use like (privmsg ... (action-msg \"dashes off to the store\"))"
  (format nil "~cACTION ~a~c" (code-char 1) text (code-char 1)))

(defun command-symbol (string)
  (let ((symbol (intern (map 'string #'char-upcase string) :bot-commands)))
    (if (fboundp symbol)
	symbol
	(progn
	  (unintern symbol :bot-commands)
	  nil))))

(defun rejoin-hook (message)
  (format (irc:client-stream (irc:connection message))
	  "~a kicked by ~a (~a)~%"
	  (second (irc:arguments message))
	  (irc:source message)
	  (third (irc:arguments message)))
  (when (string= (second (irc:arguments message)) (irc:nickname (irc:user (irc:connection message))))
    (irc:join (irc:connection message) (first (irc:arguments message)))
    (irc:privmsg (irc:connection message) (first (irc:arguments message))
		 (random-choice "(╬ ಠ益ಠ)" ">:|" "<.<" ":'(" ">:D"))))

(defun random-choice (&rest list) (nth (random (length list)) list))

(defun format-time ()
  (multiple-value-bind (second minute hour) (get-decoded-time)
    (format nil "~2,'0d:~2,'0d:~2,'0d" hour minute second)))

(defun log-privmsg (message)
  (format (irc:client-stream (irc:connection message))
	  "~a (to ~a) <~a> ~a~%"
	  (format-time) (first (irc:arguments message)) (irc:source message) (second (irc:arguments message)))
  (force-output (irc:client-stream (irc:connection message))) ;; Not strictly necessary.
  t) ;; so that it's "handled"

(defun log-action-message (message)
  (format (irc:client-stream (irc:connection message))
	  "~a (to ~a) ***~a ~a~%"
	  (format-time) (first (irc:arguments message)) (irc:source message)
	  (subseq (second (irc:arguments message)) 8 (1- (length (second (irc:arguments message))))))
  (force-output (irc:client-stream (irc:connection message)))
  t)

(define-condition irc-user-error (error) ((reply :reader error-reply :initarg :reply)))

(defun irc-user-error (string &rest args)
  "Convenience wrapper for (error 'irc-user-error :reply (format nil ...))"
  (error 'irc-user-error :reply (apply #'format nil string args)))

(defun command-dispatcher (message)
  (multiple-value-bind (command rest)
      (split-sequence:split-sequence #\Space (second (irc:arguments message)) :count 1 :remove-empty-subseqs t)
    (let ((command (first command)))
      (when (char= (aref command 0) command-character)
	(let ((command (command-symbol (subseq command 1))))
	  (when command
	    (handler-case
		(funcall command
			 (irc:source message)
			 (first (irc:arguments message))
			 (irc:connection message)
			 (subseq (second (irc:arguments message)) rest))
	      (irc-user-error (err)
		(reply (irc:source message) (first (irc:arguments message)) (irc:connection message)
		       (error-reply err)))
	      (t (err)
		(format (irc:client-stream (irc:connection message))
			"~a ERROR in ~a: ~a~%" (format-time) command err)))))))))

;; Doesn't work.
#-(and)
(defun run-bot (connection)
  (flet ((select-handler (fd)
	   (declare (ignore fd))
	   (if (listen (irc:network-stream connection))
	       (handler-bind
		   ((no-such-reply (lambda (c)
				     (format (irc:client-stream connection) "~a~%" c)
				     (invoke-restart 'continue))))
		 (irc:read-message connection))
	       (sb-sys:invalidate-descriptor
		(sb-sys:fd-stream-fd
		 (irc:network-stream connection))))))
    (sb-sys:add-fd-handler (sb-sys:fd-stream-fd (irc:network-stream connection))
			   :input #'select-handler)))
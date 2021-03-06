(in-package :flybot)

(defconstant command-character #\:)

;; no actual effect right now, and probably there should be a better mechanism
(defvar *debug-p* nil "Whether debugging is enabled.")

;; Change args to (a b c d)...?
(defmacro defcmd (name args &body body)
  "Deprecated"
  (if (= 4 (length args))
      `(defun ,(intern (symbol-name name) :bot-commands) ,args ,@body)
      (error "ERROR: (defcmd ~a ~a ...) has an invalid number of arguments - commands have four."
	     name args)))

(defun alias (alias fn)
  "Set ALIAS to point at the same function FN does."
  (declare (type symbol alias fn))
;  (setf (symbol-function (intern (symbol-name alias) :bot-commands))
;	(symbol-function (intern (symbol-name fn) :bot-commands))))
  (setf (symbol-function alias) (symbol-function fn)))

(defvar *nickname* "Spacebar" "Default nickname.")
(defvar *password* nil "Default password for servers.")
(defvar *username* nil "Default username (if NIL, use *NICKNAME*).")
(defvar *realname* nil "Default real name (if NIL, use *NICKNAME*).")

(defvar *log* t "Default logging stream.")
(defun logfile (filename)
  "Deprecated"
  (when (stringp filename)
    (setf *log* (open filename :direction :output :if-exists :append :if-does-not-exist :create))))

(defvar *connections* '()
  "A list of connection objects.  May or may not all be actually connected.")

;;; Here follows Shit To Make Timers Work.  Should use an external library like oconnore's multi-timers,
;;;  but that's not in quicklisp at the moment and I'm not feeling that adventurous.
;;; This hopeful future refactor is why this stuff isn't in a different file.

#-sbcl
(defvar *timers* '()
  "A list of pairs (universal-time function).  When the time of a pair passes, the function is executed
with no arguments and the pair is removed.")

#-sbcl
(defun register-timer (name func seconds)
  "Register a function to run in SECONDS seconds from the time REGISTER-TIMER is called."
  (push (list* (+ (get-universal-time) seconds) name func) *timers*))

#-sbcl
(defclass non-blocking-connection (irc:connection) ())

;; KLUDGE This is a pretty ugly way to do it.  Maybe edit the library to allow a :key non-blocking higher up.
#-sbcl
(defmethod irc::read-irc-message ((connection non-blocking-connection))
  (handler-case
      (let* ((msg-string (read-protocol-line-non-blocking connection))
	     (message (when msg-string (irc::create-irc-message msg-string))))
	(when message (setf (connection message) connection))
	message)
    (end-of-file ())))

;; or make read-protocol-line a generic function too and specialize on it, but that's still pretty duplicatey
#-sbcl
(defun read-protocol-line-non-blocking (connection)
  (multiple-value-bind (buf buf-len)
      (irc::read-sequence-until (network-stream connection)
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
     ;; KLUDGE: Should poll properly.
     do (read-message connection)
       (setf *timers* (remove-if
		       (lambda (x)
			 (when (< (car x) (get-universal-time))
			   (funcall (cddr x))
			   t))
		       *timers*))))

#+sbcl
(defun register-timer (name func seconds)
  "Register a function to run in SECONDS seconds from the time REGISTER-TIMER is called."
  (sb-ext:schedule-timer (sb-ext:make-timer func :name name) seconds))

(defun find-timer (name)
  "Find the timer with the name NAME, or NIL if there is no such timer."
  (find name
	#+sbcl (sb-ext:list-all-timers) #-sbcl *timers*
	:key #+sbcl #'sb-ext:timer-name #-sbcl #'second))

(defun wconnect (&key
		 (server "irc.freenode.net")
		 (port :default)
		 (nick *nickname*)
		 (username (or *username* *nickname*))
		 (realname (or *realname* *nickname*))
		 (pass *password*)
		 (logfile *log* log-p)
		 channels)
  "Convenience wrapper around IRC:CONNECT.  Attaches appropriate hooks, joins channels, and sets up the log."
  (let ((connection (irc:connect :connection-type
				 #-sbcl 'non-blocking-connection ;; so that we can have a proper event loop.
				 #+sbcl 'connection ;; or just use sbcl's timers.
				 :server server
				 :port port
				 :nickname nick
				 :username username
				 :realname realname
				 :password pass
				 :logging-stream (if log-p
						     (open logfile :direction :output :if-exists :append
							   :if-does-not-exist :create)
						     logfile))))
    (push connection *connections*)
    (add-hook connection 'irc-privmsg-message 'command-dispatcher)
    (add-hook connection 'irc-privmsg-message 'log-privmsg)
    (add-hook connection 'irc-kick-message 'rejoin-hook)
    (add-hook connection 'ctcp-action-message 'log-action-message)
    (add-hook connection 'irc-rpl_welcome-message
	      (lambda (message)
		(declare (ignore message))
		(map nil (lambda (x)
			   (if (listp x)
			       (join connection (first x) :password (second x)) ;; Join with password
			       (join connection x)))
		     channels)))
    connection))

(defun clear-commands ()
  "Clear all bot commands."
  (delete-package :bot-commands)
  (make-package :bot-commands))

;; Numbers are made up, and this should be iterative or at least use an innerfunction to avoid
;; multiple >1024 checks... though, this is an edge case, and efficiency isn't that important.

(defun buffer-privmsg (connection to text)
  "Send a privmsg TEXT to TO on CONNECTION, making sure not to exceed the length limit."
  (if (> (length text) 400)
      (privmsg connection to (concatenate 'string (subseq text 0 400) "..."))
      (privmsg connection to text)))

(defun reply (sender dest connection text &rest args)
  ;; needs major interface change, see below somewhere
  "Send a message to SENDER on DEST on CONNECTION.  TEXT is a format control, ARGS are its args."
  (if (string= dest (irc:nickname (irc:user connection)))
      (buffer-privmsg connection sender (apply #'format nil text args))
      (buffer-privmsg connection dest (format nil "14~a: ~?" sender text args))))

(defun action-msg (text)
  "Make a /me form message.  Use like (privmsg ... (action-msg \"dashes off to the store\"))"
  (format nil "~cACTION ~a~c" (code-char 1) text (code-char 1)))

(defun command-symbol (string)
  "Find the symbol for a command, given a command's name."
  (let ((symbol (find-symbol (string-upcase string) :bot-commands)))
    (if (and symbol (fboundp symbol))
	symbol
	nil)))

(defun rejoin-hook (message)
  "Hook for rejoining channels after being kicked."
  (format (client-stream (connection message))
	  "~a kicked by ~a (~a)~%"
	  (second (arguments message))
	  (source message)
	  (third (arguments message)))
  (when (string= (second (arguments message)) (nickname (user (connection message))))
    (join (connection message) (first (arguments message)))
    (privmsg (connection message) (first (arguments message))
	     ;; FIXME: This is silly.  It should be configurably silly.
	     (random-choice "(╬ ಠ益ಠ)" ">:|" "<.<" ":'(" ">:D"))))

(defun random-choice (&rest list)
  ;; dumb
  "Pick an element random from the arglist."
  (nth (random (length list)) list))

(defun format-time ()
  "Return a string representation of the present time."
  (multiple-value-bind (second minute hour) (get-decoded-time)
    (format nil "~2,'0d:~2,'0d:~2,'0d" hour minute second)))

;;; FIXME: Logging mechanism does not record server info!
;;; Logging should use CL-IRC's CLIENT-LOG or the like, not all this stuff.
;;; With customization hooks I guess, but that requires hacking on that library if... bleh, just bleh

(defun log-privmsg (message)
  "Log a PRIVMSG to the connection's log stream."
  (format (client-stream (connection message))
	  "~a (to ~a) <~a> ~a~%"
	  (format-time) (first (arguments message)) (source message) (second (arguments message)))
  (force-output (client-stream (connection message))) ;; Not strictly necessary.
  t) ;; so that it's "handled" (by CL-IRC)

(defun log-action-message (message)
  "Log a CTCP ACTION message (/me ...) to the connection's log stream."
  (format (client-stream (connection message))
	  "~a (to ~a) ***~a ~a~%"
	  (format-time) (first (arguments message)) (source message)
	  (subseq (second (arguments message)) 8 (1- (length (second (arguments message))))))
  (force-output (client-stream (connection message)))
  t)

(define-condition irc-user-error (error) ((reply :reader error-reply :initarg :reply)))
;;; FIXME: Have format-string and format-args slots like most errors

(defun irc-user-error (string &rest args)
  "Convenience wrapper for (error 'irc-user-error :reply (format nil ...))"
  (error 'irc-user-error :reply (apply #'format nil string args)))

;;; Consider:
;;;  1) generally making this less ugly
;;;  2) moving commands into a hash (duh)
;;;  3) binding source+dest+connection as specials, and having reply act on them
;;;     * possibly with a nice "if they're all nil just write to standard output" for dbug
;;;     * providing a more general SEND or whatever for when you need to message someone specifically
;;;  4) channel-specific configuration for commands should require things here
;;;  5) should return t (for handledness) if it finds a command.  or the command's result?

(defun command-dispatcher (message)
  "Dispatch the given PRIVMSG to the appropriate command (if there is one.)"
  (multiple-value-bind (command rest)
      (split-sequence #\Space (second (arguments message)) :count 1 :remove-empty-subseqs t)
    (let ((command (first command)))
      (when (char= (aref command 0) command-character)
	(let ((command (command-symbol (subseq command 1))))
	  (when command
	    (handler-case
		(funcall command
			 (source message)
			 (first (arguments message))
			 (connection message)
			 (subseq (second (arguments message)) rest))
	      (irc-user-error (err)
		(reply (source message) (first (arguments message)) (connection message)
		       (error-reply err)))
	      (t (err)
		(format (client-stream (connection message))
			"~a ERROR in ~a: ~a~%" (format-time) command err)))))))))

;; Doesn't work.
#-(and)
(defun run-bot (connection)
  (flet ((select-handler (fd)
	   (declare (ignore fd))
	   (if (listen (network-stream connection))
	       (handler-bind
		   ((no-such-reply (lambda (c)
				     (format (client-stream connection) "~a~%" c)
				     (invoke-restart 'continue))))
		 (read-message connection))
	       (sb-sys:invalidate-descriptor
		(sb-sys:fd-stream-fd
		 (network-stream connection))))))
    (sb-sys:add-fd-handler (sb-sys:fd-stream-fd (network-stream connection))
			   :input #'select-handler)))

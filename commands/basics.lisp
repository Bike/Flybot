(in-package #:flybot)

;(defcmd echo (sender dest connection text)
(defun bot-commands::echo (sender dest connection text)
  "Echoes your text back to you.  Also known as SAY."
  (reply sender dest connection text))
(alias 'bot-commands::say 'bot-commands::echo)
(setf (documentation 'bot-commands::say 'function) "Echoes your text back to you.  Also known as ECHO.") ;; ugh
;(defcmd help (sender dest connection text)

;;; Stuff for non-command help for the bot.
;;; Possible TODO: Reconsider using CL's doc mechanism, consider allowing more than just strings as docs
;;; (so that e.g. :help cooldown could return the cooldown length)
(defvar *bot-info* (make-hash-table))

(defmethod documentation ((name symbol) (doctype (eql 'bot-info)))
  (values (gethash name *bot-info*)))

(defmethod documentation ((name string) (doctype (eql 'bot-info)))
  (let ((sym (find-symbol (string-upcase name) :flybot)))
    (when sym (documentation sym doctype))))

(defmethod (setf documentation) (new-value (name symbol) (doctype (eql 'bot-info)))
  ;; FIXME: add warning for adding documentation for things that are already functions
  (setf (gethash name *bot-info*) new-value))

;;; FIXME: should be more local
(setf (documentation 'cooldown 'bot-info)
      "Certain commands have a cooldown timer associated with them to cut down on botspam.")

;;; FIXME: Docstrings can be dropped at any time.  It's unlikely they will be, but the point remains
;;;  that docstrings aren't intended to be necessary for correct behavior (as they are here).

;;; FIXME: Arrow code.

(defun bot-commands::help (sender dest connection text)
  ":help lists all available commands and info. :help name prints the documentation for command or info of that name."
  (let ((text (remove #\Space text)))
    (if (zerop (length text))
	(reply sender dest connection "Available commands: ~{~a~^, ~}.  Other info: ~{~a~^, ~}.  ~
For further help, use :help name."
	       (let (accum) (do-symbols (x :bot-commands accum) (when (fboundp x) (push (symbol-name x) accum))))
	       (let (accum) (maphash (lambda (k v) (declare (ignore v)) (push k accum)) *bot-info*) accum))
	(or (documentation text 'bot-info)
	    (let ((command (command-symbol text)))
	      (if command
		  (let ((docs (documentation command 'function)))
		    (if docs
			(reply sender dest connection "~a" docs)
			(progn
			  (format (irc:client-stream connection) "WARNING: ~a is a defined command, but is undocumented.~%"
				  (symbol-name command))
			  (reply sender dest connection "~a is a command, but remains undocumented.  This has been reported."
				 (symbol-name command)))))
		  (reply sender dest connection "~a is not a valid command." text)))))))

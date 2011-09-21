(in-package #:flybot)

;(defcmd echo (sender dest connection text)
(defun bot-commands::echo (sender dest connection text)
  "Echoes your text back to you.  Also known as SAY."
  (reply sender dest connection text))
(alias 'bot-commands::say 'bot-commands::echo)
;(defcmd help (sender dest connection text)
(defun bot-commands::help (sender dest connection text)
  ":help lists all available commands.  :help cmdname prints the documentation for cmdname."
  (let ((text (remove #\Space text)))
    (if (zerop (length text))
	(reply sender dest connection "Available commands: ~{~a~^, ~}.  For further help, use :help cmdname."
	       (let (accum) (do-symbols (x :bot-commands accum) (when (fboundp x) (push (symbol-name x) accum)))))
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
	      (reply sender dest connection "~a is not a valid command." text))))))

(in-package :flybot)

;;; TODO: Parameterize by user?

(defvar *cooldown* nil
  "If this variable is a number, some commands will have a cooldown timer of that interval (in seconds).")

(define-condition botspam (irc-user-error) ())

(defun ensure-cooled (name)
  "If *cooldown* is NIL, do nothing.
If it's not, and no timer has yet been registered for NAME, a timer is registered 
and ENSURE-COOLED returns.
If a timer has been registered, ENSURE-COOLED immediately signals a BOTSPAM error to chastise the user."
  (when *cooldown*
    (if (find-timer name)
	(error 'botspam :reply (format nil "Please don't spam ~a." name))
	(register-timer name (lambda nil) ; we don't need it to do any more than exist
			*cooldown*))))

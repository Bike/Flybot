(in-package :flybot)

;;; TODO: Parameterize by user?

(defvar *cooldown* nil
  "If this variable is a number, some commands will have a cooldown timer of that interval (in seconds).
If it's an alist, an entry of (COMMAND . n) indicates a cooldown time of n for that command.")

(define-condition botspam (irc-user-error) ())

(defun get-cooldown (name)
  (typecase *cooldown*
    (integer *cooldown*)
    (cons (cdr (assoc name *cooldown*)))
    (t (throw 'no-cooldown nil))))

(defun ensure-cooled (name)
  "If NAME has not yet cooled, signal BOTSPAM to chastise the user.
If it has, and *cooldown* is bound in such a way that NAME has a cooldown, 
establish a timer and then return."
  (catch 'no-cooldown
    (if (find-timer name)
	(error 'botspam :reply (format nil "Please don't spam ~a." name))
	(register-timer name (lambda nil) ; we don't need it to do any more than exist
			(get-cooldown name)))))

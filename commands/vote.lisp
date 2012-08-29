(in-package #:flybot)

(defvar *vote-options* nil)
;; Really that should be in a closure for vote, but meh?, and also I don't know how (declare special) works.
;; FIXME: I now know how declare special works, but closures make debugging harder.
;;(defcmd vote (sender dest connection text)
(defun bot-commands::vote (sender dest connection text)
  "Start a vote (:vote seconds option1 | option2 ...) or vote in the current election (:vote option)."
  (flet ((timer-function ()
	   (let ((winners
		  (let ((accum '()) (max 0))
		    (dolist (x *vote-options* accum)
		      (cond ((> (second x) max)
			     (setf max (second x)
				   accum (list x)))
			    ((= (second x) max) (push x accum)))))))
	     (if (= (length winners) 1)
		 (reply sender dest connection "Time's up! ~a is the winner, with ~d votes!"
			(first (first winners)) (second (first winners)))
		 (reply sender dest connection
			"Time's up! ~{~#[~;~a~;~a and ~a~:;~@{~a~#[~;, and ~:;, ~]~}~]~} ~
                                            tied with ~d votes."
			(mapcar #'first winners)
			(second (first winners))))
	     (setf *vote-options* nil))))
    (when (alpha-char-p (aref dest 0)) (irc-user-error "\"One man, one vote\", I see?")) ; hack to determine private message
    (if *vote-options*
	(let ((choice (or (ignore-errors (1- (parse-integer text)))
			  (position text *vote-options* :key #'first :test #'string-equal)
			  (irc-user-error "~a is not an option." text))))
	  (if (typep choice `(integer 0 ,(1- (length *vote-options*))))
	      (if (member sender (third (nth choice *vote-options*)) :test #'string-equal)
		  (reply sender dest connection "You already voted!")
		  (progn
		    (incf (second (nth choice *vote-options*)))
		    (push sender (third (nth choice *vote-options*)))
		    (reply sender dest connection "~:{~a: ~d~:^; ~}" *vote-options*)))
	      (reply sender dest connection "~d is not an option." choice)))
	(multiple-value-bind (timeout rest)
	    (split-sequence:split-sequence #\Space text :count 1 :remove-empty-subseqs t)
	  (unless timeout (irc-user-error "Syntax: seconds option1 | option2 ..."))
	  (let ((timeout (or (ignore-errors (parse-integer (first timeout)))
			     (irc-user-error "The timeout must be an integer between 1 and 600, not ~a." (first timeout))))
		(prospective (remove-if (lambda (x) (zerop (length x)))
					(mapcar (lambda (x) (string-trim '(#\Space #\Tab) x))
						(split-sequence:split-sequence #\| text :start rest)))))
	    (unless (typep timeout '(integer 1 600))
	      (irc-user-error "The timeout must be an integer between 1 and 600, not ~d." timeout))
	    (unless prospective (irc-user-error "You need to provide options."))
	    (if (< (length prospective) 2)
		(reply sender dest connection "No, no, you won't be able to admonish the losers if everybody has to win!")
		(progn
		  (setf *vote-options*
			(mapcar (lambda (x) (list x 0 '())) prospective))
		  (register-timer 'voting #'timer-function timeout)
		  (reply sender dest connection "Voting starts now!  You have ~d seconds to decide. ~:{~d) ~a~:^, ~}."
			 timeout (let ((c 0)) (mapcar (lambda (x) (incf c) (list c (first x))) *vote-options*))))))))))
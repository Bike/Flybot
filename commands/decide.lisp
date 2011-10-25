(in-package #:flybot)

(defun bot-commands::decide (sender dest connection text)
  "Randomly select one of several options separated by pipes (that is, \"|\".)"
  (let ((options (mapcar (lambda (x) (string-trim " " x))
			 (split-sequence:split-sequence #\| text))))
    (reply sender dest connection
	   (if (< (length options) 2) 
	       "Syntax: option1 | option2 | ..."
	       (elt options (random (length options)))))))
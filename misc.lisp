;;; Miscellaneous utilities.
(in-package #:flybot)

(let ((si '("" "kilo" "mega" "giga" "tera" "peta" "exa" "zetta" "yotta")))
  (defun add-si-prefix (number unit &optional (plural nil))
    "Interpret the integer NUMBER as an amount of UNIT in SI, and return a string describing it.
PLURAL controls whether or not the unit is plural."
    (declare (type integer number) (type string unit))
    (do ((prefix-index 0 (1+ prefix-index))
	 (number number (/ number 1e3)))
	((< number (expt 10 3))
	 (if (> number 100)
	     (format nil "~d ~a~a~@[s~]" (floor number)
		     (or (ignore-errors (nth prefix-index si)) (irc-user-error "~f could not be expressed in SI." number))
		     unit plural) ;; not sure how to eliminate trailing . from below
	     (format nil "~4f ~a~a~@[s~]" number
		     (or (ignore-errors (nth prefix-index si)) (irc-user-error "~f could not be expressed in SI." number))
		     unit plural))))))
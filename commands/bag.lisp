;;; Contributed by Cosman246

(in-package #:flybot)

(defvar *bag* nil)


(defun bot-commands::bag (sender dest connection text)
  "Stores a bag of items"
  (ensure-cooled 'bag)
  (cond ((member text *bag* :test #'string=)
	 (reply sender dest connection "~a is already in the bag" text))
	(t
	 (push text *bag*)
	 (reply sender dest connection "Added!"))))

(defun random-elt (seq)
  (elt seq (random (length seq))))

(defun bot-commands::item (sender dest connection text)
  "Throws an item at the recipient"
  (ensure-cooled 'item)
  (reply sender dest connection "~a is thrown at ~a" (random-elt *bag*) text))

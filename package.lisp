(defpackage #:flybot
  (:use #:cl #:irc #:split-sequence)
  (:export #:wconnect
	   ;; reexports
	   #:read-message #:read-message-loop))

(defpackage :bot-commands)
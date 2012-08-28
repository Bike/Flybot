(defpackage #:flybot
  (:use #:cl #:irc #:split-sequence)
  (:export #:connect
	   ;; reexports
	   #:read-message #:read-message-loop))

(defpackage :bot-commands)
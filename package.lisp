(defpackage #:flybot
  (:use #:cl #:irc #:split-sequence)
  (:export #:wconnect #:main-loop
	   ;; reexports
	   #:read-message #:read-message-loop))

(defpackage :bot-commands)

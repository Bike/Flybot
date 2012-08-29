(defpackage #:flybot
  (:use #:cl #:irc #:split-sequence)
  (:export #:wconnect #:main-loop #:spawn-main-loop
	   #:configure #:start #:stop
	   #:*connections*
	   ;; reexports
	   #:privmsg #:join #:quit #:part
	   #:read-message #:read-message-loop))

(defpackage :bot-commands)

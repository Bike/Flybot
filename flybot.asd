(asdf:defsystem #:flybot
  :depends-on (#:cl-irc #:split-sequence)
  :components ((:file "package")
	       (:file "main" :depends-on ("package"))))
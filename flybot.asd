;;; This definition should be rewritten to account for various plugins being loadable or not.
;;; But I don't know how to do that.

(asdf:defsystem #:flybot
  :depends-on (#:cl-irc #:split-sequence #:cl-ppcre #:drakma #:cl-json)
  :components ((:file "package")
	       (:file "main" :depends-on ("package"))
	       (:file "url" :depends-on ("package"))
	       (:file "misc" :depends-on ("package" "main")))) ;; on main for irc-user-error; this shouldn't be
;;; This definition should be rewritten to account for various plugins being loadable or not.
;;; But I don't know how to do that.

(asdf:defsystem #:flybot
  :description "A simple, extensible IRC bot written in Common Lisp."
  :author "Bike"
  :license "WTFPL"
  :version "0.2.0"
  :depends-on (#:cl-irc #:alexandria #:split-sequence #:cl-ppcre #:drakma #:cl-json)
  :components ((:file "package")
	       (:file "main" :depends-on ("package"))
	       (:file "cooldown" :depends-on ("main"))
	       (:file "patch" :depends-on ("main"))
	       (:file "config" :depends-on ("main"))
	       (:file "url" :depends-on ("package"))
	       (:file "REST" :depends-on ("package"))
	       (:file "misc" :depends-on ("package" "main")) ;; on main for irc-user-error; this shouldn't be
	       (:module "commands"
			:depends-on ("package" "cooldown" "main" "url" "misc")
			:components ((:file "basics")
				     (:file "bag")
				     (:file "booru")
				     (:file "roll")
				     (:file "decide")
				     (:file "moe")
				     (:file "vote")))))

(in-package :flybot)

;;; Provisional configuration mechanism.  Needs work (KLUDGE)

(defun spawn-main-loop (connection thread-name)
  "Spawn a new thread named THREAD-NAME to handle CONNECTION."
  ;; cl-irc really needs to use event loops, or something...
  (if (find-package :bt)
      (funcall (find-symbol "MAKE-THREAD" :bt) (lambda nil (main-loop connection)) :name thread-name)
      (error "Cannot spawn thread: BORDEAUX-THREADS not loaded")))

(defun start (&key (config-file #.(merge-pathnames ".flybot" (user-homedir-pathname))))
  "Start the bot, using the configuration file at CONFIG-FILE."
  (configure config-file)
  (mapc (lambda (sconfig) (apply #'wconnect sconfig)) *servers*)
  (mapc #'spawn-main-loop *connections* (mapcar (lambda (sconfig) (getf sconfig :server)) *servers*))
  (values))

(defun stop ()
  "Stop the bot: Quit all current connections and close the log."
  (mapc #'quit *connections*)
  (setf *servers* nil *connections* nil)
  (when (open-stream-p *log*) (close *log*)) ; still pretty gross
  (values))

(defun configure (config-file)
  "Read an evaluate CONFIG-FILE.  Check README for more information on the syntax."
  (let ((eof (gensym))
	(*read-eval* nil)
	(*package* (find-package :flybot)))
    (with-open-file (config-stream config-file)
      ;; loop's long form is hard to remember, and this works
      (loop (let ((form (read config-stream nil eof)))
	      (if (eq form eof)
		  (return-from configure)
		  (run-config-form form)))))))

(defun clear-config ()
  "Clear all currently known configuration."
  (mapc (lambda (sym) (setf (symbol-value sym) nil))
	'(*servers* *global-config* *connections*
	  *nickname* *username* *realname* *password*))
  (setf *log* t)
  (values))

(defun run-config-form (form)
  "Run FORM, a cons, through the configurers."
  (check-type form cons)
  (let ((configurer (cdr (assoc (first form) *configurers*))))
    (if configurer
	(apply configurer (rest form))
	(error "Unknown configuration function ~a" (first form)))))

(defvar *servers* nil "List of server data structures for provisional config mechanism.")
(defvar *global-config* nil "List of global (default) configuration values.") ; unused (yet)
(defvar *configuring-server*) ; can't set docs!  the horror!

(defvar *configurers*
  '((server . config-server)
    (nickname . config-nickname)
    (username . config-username)
    (realname . config-realname)
    (pass . config-pass)
    (logfile . config-logfile))
  "Alist of config file symbols to functions to handle them.")

(defun config-server (name port &rest clauses)
  "Configurer for (server ...)"
  (when (assoc name *servers* :test #'string-equal)
    (error "Server ~a already specified in config" name))
  (let ((*configuring-server* (list :server name :port port)) ; plist rep., kickin it old skool
	(*configurers* (remove 'config-server ; no recursing
			       (push (cons 'channel 'config-channel)
				     *configurers*)
			       :key #'cdr)))
    (mapc #'run-config-form clauses)
    (push *configuring-server* *servers*)))

;;; Should support more options, like per-channel command restrictions.
;;; This will require reworking the command dispatcher.
(defun config-channel (name)
  "Configurer for (channel ...)"
  (push name (getf *configuring-server* :channels)))

(defun config-nickname (nick)
  "Configurer for (nickname ...)"
  (if (boundp '*configuring-server*)
      (setf (getf *configuring-server* :nick) nick)
      (setf *nickname* nick)))

(defun config-username (username)
  "Configurer for (username ...)"
  (if (boundp '*configuring-server*)
      (setf (getf *configuring-server* :username) username)
      (setf *username* username)))

(defun config-realname (realname)
  "Configurer for (username ...)"
  (if (boundp '*configuring-server*)
      (setf (getf *configuring-server* :realname) realname)
      (setf *realname* realname)))

(defun config-pass (pass)
  "Configurer for (pass ...)"
  (if (boundp '*configuring-server*)
      (setf (getf *configuring-server* :pass) pass)
      (setf *password* pass)))

(defun config-logfile (fname)
  "Configurer for (logfile ...)"
  (let ((stream (open fname :direction :output :if-exists :append :if-does-not-exist :create)))
    ;; KLUDGE: What closes this?  Nothing.  More reasons to rework the object hierarchy.
    (if (boundp '*configuring-server*)
	(setf (getf *configuring-server* :logfile) stream)
	(setf *log* stream))))

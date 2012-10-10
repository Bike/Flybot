;;;; moe.lisp: moeconomy

;;; iverum side

(defpackage #:moe
  (:use #:cl #:REST)
  (:export #:*api-key* #:moeconomy
	   #:no-such-moe #:no-such-moe-name
	   #:moe-exists #:moe-exists-name
	   #:moeconomist
	   #:moeconomist-id #:moeconomist-nick #:moepoints #:moeconomist-aliases
	   #:get-by-name
	   #:create #:update #:retrieve #:delete))

(in-package #:moe)

(defvar *api-key*) ; should be set in config

(defclass moeconomy (json-store)
  ()
  (:default-initargs :classes (mapcar #'find-class '(moeconomist stats uploading.moeconomist))))

(defclass moeconomist (resource)
  ((id :initarg :id :accessor moeconomist-id
       :type integer)
   (nick :initarg :nick :accessor moeconomist-nick
	 :type string)
   (moepoints :initarg :moepoints :accessor moepoints
	      :type integer)
   (aliases :initarg :aliases :accessor moeconomist-aliases
	    :type (vector string))))

(defmethod create ((store moeconomy) (resource moeconomist))
  (create-by-name store (moeconomist-nick resource) resource))
(defmethod update ((store moeconomy) (resource moeconomist))
  (modify-by-name store (moeconomist-nick resource) resource))
(defmethod retrieve ((store moeconomy) (resource moeconomist))
  (get-by-name store (moeconomist-nick resource)))

(defclass uploading.moeconomist ()
  ((key :initarg :key)
   (nick :initarg :nick :accessor moeconomist-nick)
   (moepoints :initarg :moepoints :accessor moepoints
	      :type integer)
   (aliases :initarg :aliases :accessor moeconomist-aliases
	    :type (vector string))))

(defclass stats (resource)
  ((accounts :initarg :accounts :accessor stats.accounts
	     :type integer)
   (aliases :initarg :aliases :accessor stats.aliases
	    :type integer)
   (moepoints :initarg :moepoints :accessor stats.moepoints
	      :type integer)))

(define-http-method all ((store moeconomy)) ())
(define-http-method stats ((store moeconomy)) () (:uri-append "stats"))
(define-http-method get-by-id ((store moeconomy) (id integer)) ()
  (:uri-append (format nil "~d" id)))
(define-http-method get-by-name ((store moeconomy) (name string)) ()
  (:error-status ((404 (http-error 'no-such-moe :name name))))
  (:uri-append name))

(defun prepare-for-upload (moeconomist)
  (make-instance 'uploading.moeconomist :key *api-key*
		 :moepoints (moepoints moeconomist)
		 :nick (moeconomist-nick moeconomist)
		 :aliases (moeconomist-aliases moeconomist)))

(define-condition no-such-moe (http-error)
  ((name :initarg :name :reader no-such-moe-name)))
(define-condition moe-exists (http-error)
  ((name :initarg :name :reader moe-exists-name)))

(define-http-method create-by-name ((store moeconomy) (name string) (object moeconomist))
    (:method :put
      :content-type "application/json"
      :content (with-output-to-string (*standard-output*)
		 (serialize store (prepare-for-upload object) *standard-output*)))
  (:error-status ((409 (http-error 'moe-exists :name name))
		  (403 (error "The API key was refused."))))
  (:uri-append (format nil "/create/~a" name)))
(define-http-method modify-by-name ((store moeconomy) (name string) (object moeconomist))
    (:method :put
      :content-type "application/json"
      :content (with-output-to-string (*standard-output*)
		 (serialize store (prepare-for-upload object) *standard-output*)))
  (:uri-append (format nil "/modify/~a" name))
  (:error-status ((404 (http-error 'no-such-moe :name name))
		  (403 (error "The API key was refused.")))))

;;; bot side

(in-package #:flybot)

(defvar *moeconomy*)

(defun bot-commands::register (sender dest connection text)
  (declare (ignore text))
  (handler-case
      (let ((*package* (find-package :moe))) ;; necessary due to how cl-json deals with symbols.  not sure how to improve.
	(moe:create *moeconomy*
		    (make-instance 'moe:moeconomist :nick sender :aliases #() :moepoints 1000))
	(reply sender dest connection
	       "~a is now registered."
	       sender))
    (moe:moe-exists (e)
      (reply sender dest connection
	     "~a is already registered." (moe:moe-exists-name e)))))

(defun bot-commands::appraise (sender dest connection text)
  (handler-case
      (let ((*package* (find-package :moe)))
	(reply sender dest connection
	       "~a has ~d moepoints."
	       text
	       (moe:moepoints (moe:retrieve *moeconomy* (make-instance 'moe:moeconomist :nick text)))))
    (moe:no-such-moe (e)
      (reply sender dest connection
	     "~a is not a registered user of the moeconomy." (moe:no-such-moe-name e))))))

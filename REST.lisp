;;;; REST.lisp: General utilities for working with REST stores.

;;; probably deserves its own system, but it's pretty small right now...
;;; depends on: alexandria, drakma, closer-mop, cl-json.  second for http and latter two for json.

(defpackage #:REST
  (:use #:cl #:alexandria)
  (:export
   ;; base classes
   #:store #:resource
   ;; CRUD
   #:create #:retrieve #:update #:destroy
   ;; serialization
   #:serialize #:deserialize
   ;; REST with HTTP (i.e. almost any REST)
   #:http-store #:define-http-method #:http-error
   #:store-uri #:store-content-type
   ;; JSON serialization/deserialization (might be factored into another thing)
   #:json-store
   #:json-store-classes
   #:no-json-serializer #:no-json-deserializer
   #:no-json-serializer-object #:no-json-deserializer-keys #:no-json-deserializer-values
   #:slots-match ; kind of an outlier here
   ))

(in-package #:REST)

;;; General

(defclass store () ())

(defclass resource () ())

(defgeneric create (store resource)
  (:documentation "Create a new resource in STORE based on the representation that is RESOURCE."))
(defgeneric retrieve (store resource)
  (:documentation "Destructively modify RESOURCE to match the appropriate resource in STORE."))
(defgeneric update (store resource)
  (:documentation "Alter STORE's copy of RESOURCE to match the local one."))
(defgeneric destroy (store resource)
  (:documentation "Delete RESOURCE from STORE."))

;;; HTTP

(defgeneric serialize (store resource stream)
  (:documentation "Output some externalizable representation of RESOURCE as needed by STORE into STREAM."))
(defgeneric deserialize (store stream)
  (:documentation "Inverse of SERIALIZE."))

(defclass http-store (store)
  ((base-uri :initarg :uri :accessor store-uri)
   (content-type :initarg :content-type :accessor store-content-type)))

(define-condition http-error ()
  ((code :initarg :code :reader http-error-code)
   (reason :initarg :reason :reader http-error-reason)
   (content :initarg :content :reader http-error-content)))

(defmacro define-http-method (name (&whole lambda-list store &rest more-args) (&rest request-args) &body options)
  (declare (ignore more-args))
  (setf options (apply #'append options))
  (with-gensyms (content code headers uri stream close reason)
  `(defmethod ,name ,lambda-list
     (multiple-value-bind (,content ,code ,headers ,uri ,stream ,close ,reason)
	 (drakma:http-request (concatenate 'string
					   (store-uri store)
					   ,(or (getf options :uri-append) ""))
			      :want-stream t
			      ,@request-args)
       (declare (ignore ,headers ,uri ,stream))
       (flet ((http-error (class &rest other-initargs)
		(apply #'error class :code ,code :reason ,reason :content ,content other-initargs)))
	 (cond ((<= 200 ,code 299)
		(unwind-protect
		     (deserialize ,(ensure-car store) ,content)
		  (when ,close (close ,content))))
	       ,@(mapcar (lambda (clause)
			   `((= ,code ,(first clause)) ,@(rest clause)))
			 (getf options :error-status))
	       (t (http-error 'http-error))))))))

;;; JSON

(push '("application" . "json") drakma:*text-content-types*)

(defclass json-store (http-store)
  ((classes :initarg :classes :accessor json-store-classes))
  (:default-initargs :content-type '("application" . "json")))

(define-condition no-json-serializer ()
  ((object :initarg :object :reader no-json-serializer-object)))

(define-condition no-json-deserializer ()
  ((keys :initarg :keys :reader no-json-deserializer-keys)
   (values :initarg :values :reader no-json-deserializer-values)))

(defmethod serialize ((store json-store) resource stream)
  (if (find (class-of resource) (json-store-classes store)) ; if it's a class known to the store
      (json:encode-json resource stream)
      (error 'no-json-serializer :object resource)))

(defun unpairlis (keys values &optional plist)
  (append
   (mapcan (lambda (k v)
	     (list (make-keyword k) v))
	   keys values)
   plist))

(defgeneric slots-match (class keys values))

(defmethod slots-match ((class standard-class) keys values)
  (c2mop:ensure-finalized class)
  (let ((slots (c2mop:class-slots class)))
    (mapc (lambda (key value)
	    (let ((slot (find key slots :key #'c2mop:slot-definition-name)))
	      (cond (slot
		     (unless (typep value (c2mop:slot-definition-type slot))
		       (return-from slots-match nil))
		     (setf slots (remove slot slots)))
		    (t (return-from slots-match nil)))))
	  keys values)
    (null slots)))

(defmethod deserialize ((store json-store) stream)
  (let (*object-keys* *object-values*)
    (declare (special *object-keys* *object-values*))
    (flet ((push-key (key)
	     (push (find-symbol (string-upcase key)) *object-keys*))
	   (push-value (value)
	     (push value *object-values*))
	   (finish-object ()
	     (dolist (class (json-store-classes store))
	       (when (slots-match class *object-keys* *object-values*)
		 (return-from finish-object
		   (apply #'make-instance class (unpairlis *object-keys* *object-values*)))))
	     (error 'no-json-deserializer :keys *object-keys* :values *object-values*)))
    (json:bind-custom-vars
	(;() ; for indenting
	 :array-type 'vector
	 :object-key #'push-key
	 :object-value #'push-value
	 :end-of-object #'finish-object
	 :object-scope '(*object-keys* *object-values*))
      (json:decode-json stream)))))

;;;;; REST.lisp ends

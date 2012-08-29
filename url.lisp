;;; Utilities for dealing with the Web.
(in-package #:flybot)

(define-condition http-status (condition)
  ((code :reader code :initarg :code)
   (reason :reader reason :initarg :reason))
  (:documentation "Web access error, e.g. 404 Not Found"))

;; from ppcre docs
(let ((url-regex (ppcre:create-scanner "[^a-z-A-Z0-9|\\-.]")))
  (defun url-encode (string)
    "URL-encodes a string."
    ;; won't work for Corman Lisp because non-ASCII chararacter aren't 8-bit there
    (flet ((convert (target-string start end match-start match-end reg-starts reg-ends)
	     (declare (ignore start end match-end reg-starts reg-ends))
             (format nil "%~2,'0x" (char-code (char target-string match-start)))))
      (ppcre:regex-replace-all url-regex string #'convert))))

(defun get-request-uri (base-uri args)
  "Returns a PURI of the base-uri with ARGS (a \"proper alist\") appended as a GET HTTP request."
    (puri:uri (format nil "~a?~(~{~{~a=~a~}~^&~}~)" base-uri args)))

(defun get-file-type (uri)
  "Request and return the file type of a file on a server, as a string."
  (multiple-value-bind (content status headers uri2 stream2 close reason)
      (drakma:http-request uri :method :head)
    (declare (ignore content uri2 stream2 close))
    (if (= status 200)
	(second
	 (split-sequence:split-sequence #\/
					(first (split-sequence:split-sequence #\; (cdr (assoc :content-type headers))))))
	(error 'http-status :reason reason :code status))))

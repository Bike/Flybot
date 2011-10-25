;;; Utilities for dealing with the Web.
(in-package #:flybot)

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
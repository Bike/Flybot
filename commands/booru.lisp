;;; Sketch of imageboard searching.
(in-package #:flybot)

(defun assoc-or-die (item alist &rest keys)
  (or (cdr (apply #'assoc item alist keys)) ;; (cdr nil) => nil
      (irc-user-error "Internal error: ~a not found in Danbooru JSON response." item)))

(defun bot-commands::danbooru (sender dest connection text)
  "Return the first image with the given tags found on Danbooru."
  (multiple-value-bind (stream status headers uri stream2 close reason)
      (let ((puri:*strict-parse* nil))
	;; This is necessary because Danbooru doesn't recognize %2b as +, etc., and other percent-encoded
	;;  characters cause server errors.
	(drakma:http-request (get-request-uri "http://danbooru.donmai.us/post/index.json"
					      (list (list 'limit 1) (list 'tags text)))
			     :want-stream t))
    (declare (ignore headers uri stream2 close))
    (if (= status 200) ;; we're good to go!
	(let ((json (car (json:decode-json stream)))) ;; exploting (car nil) => nil, wheeeee
	  (if (null json) ;; no results!
	      (reply sender dest connection "No results found for ~a." text)
	      (reply sender dest connection
		     "Danbooru: ~a image (~d × ~d pixels, ~a): ~a - ~a"
		     (string-upcase (first
				     (split-sequence:split-sequence #\. (assoc-or-die :file--url json) :from-end t :count 1)))
		     (assoc-or-die :width json)
		     (assoc-or-die :height json)
		     (add-si-prefix (assoc-or-die :file--size json) "byte" t)
		     (let ((tags (assoc-or-die :tags json)))
		       (if (> (length tags) 180) ;; arbitrary number
			 (concatenate 'string (subseq tags 0 180) "...")
			 tags))
		     (concatenate 'string "http://danbooru.donmai.us/post/show/" (princ-to-string (assoc-or-die :id json))))))
	(irc-user-error "Danbooru API returned status ~d: ~a" status reason))))
(in-package #:flybot)

#|

(defcmd roll (sender dest connection text)
  "Roll a bunch of dice.  #d#, constants, and addition are supported."
  (block abandon
    (labels ((parse-num (string)
	       (if (zerop (length string))
		   nil
		   (let ((result (ignore-errors (parse-integer string))))
		     (if result
			 (if (plusp result)
			     result
			     (return-from abandon
			       (reply sender dest connection "~d should be positive." result)))
			 (return-from abandon
			   (reply sender dest connection "~a should have been a number." string))))))
	     (parse-die (string)
	       (multiple-value-bind (strings length)
		   (split-sequence:split-sequence #\d string :count 2)
		 (unless (= length (length string))
		   (return-from abandon
		     (reply sender dest connection "~a is not a valid die spec." string)))
		 (destructuring-bind (count &optional weight) strings
		   (cond ((null weight) (parse-num count))
			 ((zerop (length weight))
			  (return-from abandon
			    (reply sender dest connection "~a is not a valid die spec." string)))
			 (t
			  (reduce #'+
				  (mapcar #'random
					  (make-list (or (parse-num count) 1)
						     :initial-element (parse-num weight))))))))))
      (reply sender dest connection
	     "Rolled ~a = ~d" text (reduce #'+
					   (mapcar #'parse-die
						   (split-sequence:split-sequence #\+
										  (remove #\Space text))))))))

|#

#|

(defcmd roll (sender dest connection text)
  "Roll some dice.  The syntax is complicated enough to be best described with a compiler compiler, but see http://en.wikipedia.org/wiki/Dice_notation"
  (let ((dice-msg ""))
    (labels ((infix-eval (list)
	       (when (numberp list) (return-from infix-eval list))
	       (when (symbolp list) (irc-user-error "Invalid syntax: ~a should be a number." list))
	       (if (= (length list) 1)
		   (etypecase (first list)
		     (list (infix-eval (first list)))
		     (number (first list))
		     (symbol (irc-user-error "~a symbol in invalid syntactic position." (first list))))
		   (if (symbolp (first list))
		       (if (eq (first list) 'd)
			   (case (third list)
			     ((h) (infix-eval
				   (cons
				    (dx 1 (if (eq (second list) '%)
					      100
					      (infix-eval (second list)))
					(infix-eval (fourth list))
					#'>)
				    (cddddr list))))
			     ((l) (infix-eval
				   (cons
				    (dx 1 (if (eq (second list) '%)
					      100
					      (infix-eval (second list)))
					(infix-eval (fourth list))
					#'<)
				    (cddddr list))))
			     (t (infix-eval
				 (cons
				  (d 1 (if (eq (second list) '%)
					   100
					   (infix-eval (second list))))
				  (cddr list)))))
			   (irc-user-error "Invalid syntax: ~a is not a number." (first list)))
		       (case (second list)
			 ((* /)
			  (if (or (eq (fourth list) 'd) ;; Higher precedence.  This is a clusterfuck, eh?
				  (eq (third list) 'd))
			      (funcall (second list)
				       (infix-eval (first list))
				       (infix-eval (cddr list)))
			      (infix-eval (cons
					   (funcall (second list)
						    (infix-eval (first list))
						    (infix-eval (third list)))
					   (cdddr list)))))
			 ((+ -)
			  (if (or (member (fourth list) '(* / d)) ;; AHAHAHAHAHAHAHA
				  (eq (third list) 'd))
			      (funcall (second list)
				       (infix-eval (first list))
				       (infix-eval (cddr list)))
			      (infix-eval (cons
					   (funcall (second list)
						    (infix-eval (first list))
						    (infix-eval (third list)))
					   (cdddr list)))))
			 ((d) ;; WTF is precedence?
			  (case (fourth list)
			    ((h) (infix-eval (cons
					      (dx (infix-eval (first list))
						  (if (eq (third list) '%)
						      100
						      (infix-eval (third list)))
						  (infix-eval (fifth list))
						  #'>)
					      (cddr (cdddr list)))))
			    ((l) (infix-eval (cons
					      (dx (infix-eval (first list))
						  (if (eq (third list) '%)
						      100
						      (infix-eval (third list)))
						  (infix-eval (fifth list))
						  #'<)
					      (cddr (cdddr list)))))
			    (t (infix-eval (cons
					    (d (infix-eval (first list))
					       (if (eq (third list) '%)
						   100
						   (infix-eval (third list))))
					    (cdddr list))))))))))
	     (d (count weight)
	       (unless (integerp count)
		 (irc-user-error "Are you actually playing a game that makes you roll ~a of a die?" (mod count 1)))
	       (unless (integerp weight)
		 (irc-user-error  "~a of a side?  What?" (mod weight 1)))
	       (when (> count 1000)
		 (irc-user-error "Too many dice."))
	       (let ((dice (mapcar (lambda (x) (1+ (random x))) (make-list count :initial-element weight))))
		 (setf dice-msg (concatenate 'string dice-msg (format nil "~{~d~^, ~}; " dice)))
		 (reduce #'+ dice)))
	     (dx (count weight keep pred)
	       (unless (integerp count)
		 (irc-user-error "Are you actually playing a game that makes you roll ~a of a die?" (mod count 1)))
	       (unless (rationalp weight)
		 (irc-user-error "~a of a side?  These are good dice, my friend, I don't want to ruin them!"
				 (mod weight 1)))
	       (unless (rationalp keep)
		 (irc-user-error "I can't keep ~a dice.  I don't even know what that means. Go away." (mod weight 1)))
	       (when (> count 1000)
		 (irc-user-error "Too many dice."))
	       (if (> keep count)
		   (irc-user-error "Keeping ~d dice from ~d rolled doesn't make sense." keep count)
		   (let* ((dice (sort (mapcar (lambda (x) (1+ (random x))) (make-list count :initial-element weight)) pred))
			  (stay (subseq dice 0 keep))
			  (drop (subseq dice keep)))
		     (setf dice-msg (concatenate 'string dice-msg (format nil "~{~d~^, ~}/~{~d~^, ~}; " stay drop)))
		     (reduce #'+ stay))))
	   (prepare (string)
	     "Makes the string of dicey gobbledegook slightly more palatable for read-delimited-string."
	     (apply #'concatenate 'string
		    (map 'list
			 (lambda (char)
			   (cond ((member char '(#\d #\h #\l #\* #\( #\) #\/ #\+ #\- #\%) :test #'char-equal)
				  (concatenate 'string " " (make-string 1 :initial-element char) " ")) ;; This sucks.
				 ((digit-char-p char) (make-string 1 :initial-element char))
				 ((member char '(#\Space #\Tab)) "")
				 (t (irc-user-error "Invalid syntax: ~:c disallowed." char))))
			 (concatenate 'string string ")")))))
      (if (plusp (length text))
	  (let ((result (infix-eval (read-delimited-list #\) (make-string-input-stream (prepare text))))))
	    (reply sender dest connection
		   (concatenate 'string
				"Rolled ~a = "
				(when (> (length dice-msg) 2)
				  (if (> (length dice-msg) 100)
				      (concatenate 'string (subseq dice-msg 0 100) "...")
				      (subseq dice-msg 0 (- (length dice-msg) 2))))
				": ~d")
		   text result))))))

|#

(defvar *fail* (cons nil nil))
(defvar *no-bindings* (cons nil nil))

(defun pat-match (pattern input &optional (bindings *no-bindings*))
  "Match pattern against input in the context of the bindings."
  (cond ((eq bindings *fail*) *fail*)
	((variable-p pattern)
	 (match-variable pattern input bindings))
	((eql pattern input) bindings)
	((segment-pattern-p pattern)
	 (segment-matcher pattern input bindings))
	((single-pattern-p pattern)
	 (single-matcher pattern input bindings))
	((and (consp pattern) (consp input))
	 (pat-match (rest pattern) (rest input)
		    (pat-match (first pattern) (first input)
			       bindings)))
	(t *fail*)))

(defun variable-p (x)
  "Is x a variable (a symbol beginning with '?')?"
  (and (symbolp x) (char= (char (symbol-name x) 0) #\?)))
(defun get-binding (var bindings)
  "Find a (variable . value) pair in a binding list."
  (assoc var bindings))
(defun binding-var (binding)
  "Get the variable part of a single binding."
  (car binding))
(defun binding-val (binding)
  "Get the value part of a single binding."
  (cdr binding))
(defun make-binding (var val) (cons var val))
(defun lookup (var bindings)
  "Get the value part for var from a binding list."
  (binding-val (get-binding var bindings)))

(defun extend-bindings (var val bindings)
  "Add a (var . value) pair to a binding list."
  (cons (make-binding var val)
	;; Once we add a real binding we can kill *no-bindings*
	(if (and (eq bindings *no-bindings*))
	    nil
	    bindings)))

(defun match-variable (var input bindings)
  "Does VAR match input?  Uses (or updates) and returns BINDINGS."
  (let ((binding (get-binding var bindings)))
    (cond ((not binding) (extend-bindings var input bindings))
	  ((equal input (binding-val binding)) bindings)
	  (t *fail*))))

(setf (get '?is 'single-match) 'match-is)
(setf (get '?or 'single-match) 'match-or)
(setf (get '?and 'single-match) 'match-and)
(setf (get '?not 'single-match) 'match-not)

(setf (get '?* 'segment-match) 'segment-match)
(setf (get '?+ 'segment-match) 'segment-match+)
(setf (get '?? 'segment-match) 'segment-match?)
(setf (get '?if 'segment-match) 'match-if)

(defun segment-pattern-p (pattern)
  "Is this a segment-matching pattern like ((?* var) ...)?"
  (and (consp pattern) (consp (first pattern))
       (symbolp (first (first pattern)))
       (segment-match-fn (first (first pattern)))))
(defun single-pattern-p (pattern)
  "Is this a single-matching pattern?  E.g. (?is x predicate) (?and ...) (?or ...)."
  (and (consp pattern)
       (single-match-fn (first pattern))))
(defun segment-matcher (pattern input bindings)
  "Call the right function for this kind of segment pattern."
  (funcall (segment-match-fn (first (first pattern)))
	   pattern input bindings))
(defun single-matcher (pattern input bindings)
  "Call the right function for this kind of single pattern."
  (funcall (single-match-fn (first pattern))
	   pattern input bindings))
(defun segment-match-fn (x)
  "Get the segment match function for x, if it is a symbol that has one."
  (when (symbolp x) (get x 'segment-match)))
(defun single-match-fn (x)
  "Get the single match function for x, if it is a symbol that has one."
  (when (symbolp x) (get x 'single-match)))

(defun match-is (var-and-pred input bindings)
  "Succeed and bind var if the input satisfies pred, where var-and-pred = (var pred)."
  (let* ((var (first var-and-pred))
	 (pred (second var-and-pred))
	 (new-bindings (pat-match var input bindings)))
    (if (or (eq new-bindings *fail*)
	    (not (funcall pred input)))
	*fail*
	new-bindings)))
(defun match-and (patterns input bindings)
  "Succeed if all the patterns match the input."
  (cond ((eq bindings *fail*) *fail*)
	((null patterns) bindings)
	(t (match-and (rest patterns) input
		      (pat-match (first patterns) input bindings)))))
(defun match-or (patterns input bindings)
  "Succeed if any one of the patterns match the input."
  (if (null patterns)
      *fail*
      (let ((new-bindings (pat-match (first patterns)
				     input bindings)))
	(if (eq new-bindings *fail*)
	    (match-or (rest patterns) input bindings)
	    new-bindings))))
(defun match-not (patterns input bindings)
  "Succeed if none of the patterns match the input.  This will never bind any variables."
  (if (match-or patterns input bindings)
      *fail*
      bindings))

(defun segment-match (pattern input bindings &optional (start 0))
  "Match the segment pattern ((?* var) ...) against input."
  (let ((var (second (first pattern)))
	(pat (rest pattern)))
    (if (null pat)
	(match-variable var input bindings)
	(let ((pos (first-match-pos (first pat) input start)))
	  (if (null pos)
	      *fail*
	      (let ((b2 (pat-match
			 pat (subseq input pos)
			 (match-variable var (subseq input 0 pos)
					 bindings))))
		;; If this failed, try a longer one.
		(if (eq b2 *fail*)
		    (segment-match pattern input bindings (1+ pos))
		    b2)))))))
(defun first-match-pos (pat1 input start)
  "Find the first position that PAT1 could possibly match INPUT, starting at START.
   If PAT1 is variable, just return START."
  (cond ((and (atom pat1) (not (variable-p pat1)))
	 (position pat1 input :start start :test #'equal))
	((< start (length input)) start)
	(t nil)))

(defun segment-match+ (pattern input bindings)
  "Match one or more elements of input."
  (segment-match pattern input bindings 1))
(defun segment-match? (pattern input bindings)
  "Match zero or one element of input."
  (let ((var (second (first pattern)))
	(pat (rest pattern)))
    (or (pat-match (cons var pat) input bindings)
	(pat-match pat input bindings))))

(defun match-if (pattern input bindings)
  "Test an arbitrary Lisp expression involving variables.  The pattern looks like ((?if code) ...)"
  (and (eval (sublis bindings (second (first pattern))))
       (pat-match (rest pattern) input bindings)))

(defun rule-based-translator (input rules &key (matcher #'pat-match) (rule-if #'first) (rule-then #'rest) (action #'sublis))
  "Find the first rule in RULES that matches INPUT, and apply ACTION to that rule."
  (some (lambda (rule)
	  (let ((result (funcall matcher (funcall rule-if rule) input)))
	    (unless (eq result *fail*)
	      (funcall action result (funcall rule-then rule)))))
	rules))

(defun pat-match-abbrev (symbol expansion)
  "Define SYMBOL as a macro standing for a pat-match pattern."
  (setf (get symbol 'expand-pat-match-abbrev) expansion))
(defun expand-pat-match-abbrev (pat)
  "Expand out all pattern matching abbreviations in PAT."
  (cond ((and (symbolp pat) (get pat 'expand-pat-match-abbrev)))
	((atom pat) pat)
	(t (cons (expand-pat-match-abbrev (first pat))
		 (expand-pat-match-abbrev (rest pat))))))

(pat-match-abbrev '?a+ '(?+ ?a))
(pat-match-abbrev '?b+ '(?+ ?b))
(pat-match-abbrev '?c+ '(?+ ?c))

(defparameter *infix->prefix-rules*
  (mapcar #'expand-pat-match-abbrev
	  '((% (quote 100))
	    ((d ?a) (dice 1 ?a 0))
	    ((d ?a h ?b) (dice 1 ?a ?b))
	    ((d ?a l ?b) (dice 1 ?a (- ?b)))
	    ((?a d ?b) (dice ?a ?b 0))
	    ((?a d ?b h ?c) (dice ?a ?b ?c))
	    ((?a d ?b l ?c) (dice ?a ?b (- ?c)))
	    ((d ?a+) (irc-user-error "Expression is ill-formed."))
	    ((- ?a+) (- ?a+))
	    ((+ ?a+) (+ ?a+))
	    ((?a+ + ?b+) (+ ?a ?b))
	    ((?a+ - ?b+) (- ?a ?b))
	    ((?a+ * ?b+) (* ?a ?b))
	    ((?a+ / ?b+) (/ ?a ?b))))
  "A list of rules, ordered by precedence.")

(defun infix->prefix (exp)
  "Translate an infix expression into prefix notation."
  (cond ((atom exp) exp)
	((= (length exp) 1) (infix->prefix (first exp)))
	((rule-based-translator exp *infix->prefix-rules*
				:rule-if #'first :rule-then #'second
				:action (lambda (bindings response)
					  (sublis (mapcar (lambda (pair) (cons (first pair) (infix->prefix (rest pair))))
							  bindings)
						  response))))
	((symbolp (first exp))
	 (list (first exp) (infix->prefix (rest exp))))
	(t (irc-user-error "Expression is ill-formed."))))

(defun roll-prep (string)
  (apply #'concatenate 'string
	 (append
	  (map 'list
	       (lambda (c)
		 (cond ((digit-char-p c) (make-array 1 :element-type 'character :initial-element c))
		       ((member c '(#\( #\) #\* #\/ #\+ #\- #\d #\h #\l #\Space) :test #'char-equal)
			(make-array 3 :element-type 'character :initial-contents (list #\Space c #\Space)))
		       (t (irc-user-error "Invalid syntax: ~c disallowed." c))))
	       string)
	  (list " ]"))))

(defvar *dice-info* nil
  "The special variable for accumulating intermediate dice results.")

(defun dice (count weight keep)
  (unless (and (integerp count) (plusp count)) (irc-user-error "Can't roll ~a dice." count))
  (unless (and (integerp weight) (plusp weight)) (irc-user-error "Can't roll dice with ~a sides." weight))
  (unless (and (integerp keep) (< (abs keep) count)) (irc-user-error "Can't keep ~a dice of ~d." keep count))
  (let ((initial (mapcar #'(lambda (x) (1+ (random x))) (make-list count :initial-element weight))))
    (cond ((zerop keep)
	   (when (< (length *dice-info*) 10) ;; ARBITRARY!
	     (push initial *dice-info*))
	   (reduce #'+ initial))
	  ((plusp keep)
	   (let ((keeping (subseq (sort initial #'>) 0 keep)))
	     (when (< (length *dice-info*) 10)
	       (push keeping *dice-info*))
	     (reduce #'+ keeping)))
	  ((minusp keep)
	   (let ((keeping (subseq (sort initial #'<) 0 (- keep))))
	     (when (< (length *dice-info*) 10)
	       (push keeping *dice-info*))
	     (reduce #'+ keeping))))))

;;(defcmd roll (sender dest connection text)
(defun bot-commands::roll (sender dest connection text)
  (let ((*package* (find-package :flybot))
	(*dice-info* nil))
    (let ((total (eval (infix->prefix (read-delimited-list #\] (make-string-input-stream (roll-prep text)))))))
      (reply sender dest connection
	     "~@[~{~{~d~^, ~}~^; ~} : ~]~d" (reverse *dice-info*) total))))
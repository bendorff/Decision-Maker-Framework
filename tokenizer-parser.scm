;;;; TO-DO:
;;
;; Strict English Subset
;; English -> Scheme tokenizer/parser
;; "inference machine"
;; "knowledge base"

;;;;;;;;;;;;;;;;;;;;;;;
;; Syntax for DecisionEnglish
;; 
;; ??y IS ??x    - IS-A relationship
;; ??y DOES ??x  - Action 
;;
;;
;;
;;
;;
;;
;; Country A annexes land from Country B.
;; What should it know? A is an object of type Country.
;; annexes means the "annex" action
;; land is an object
;; objects can be owned
;; B is an object of type Country
;; annex operates on an object and (optionally) an original owner
;;
;; Annex is an example of a "take" action
;;
;; Possible action types:
;; APPEAR, DISAPPEAR, TAKE, GIVE, HURT, HELP
;;
;; Statements should be separated by .
;;

;;; Tokenizer

(define (tokenize filename)
  (define split-charset (char-set #\space))
  (define punc-charset (char-set #\! #\" #\# #\$ #\% #\& #\' #\( #\) #\* #\+ #\, #\- #\. #\/ #\: #\; #\< #\= #\> #\? #\@ #\[ #\\ #\] #\^ #\_ #\` #\{ #\| #\} #\~)) 
  (define (split line)
    (let split-lp ((buffer "")
		   (rem-line line)
		   (tokens '()))
      (if (= (string-length rem-line) 0)
	  (if (= (string-length buffer) 0)
	      tokens
	      (append tokens (list (string-downcase buffer))))
	  (let ((cur-char (string-ref rem-line 0)))
	    (cond 
	     ((char-set-member? split-charset cur-char)
	      (if (= (string-length buffer) 0)
		  (split-lp "" (substring rem-line 1 (string-length rem-line)) tokens)
		  (split-lp "" (substring rem-line 1 (string-length rem-line)) (append tokens (list (string-downcase buffer))))))
	     ((char-set-member? punc-charset cur-char)
	      (if (= (string-length buffer) 0)
		  (split-lp "" (substring rem-line 1 (string-length rem-line)) tokens)
		  (split-lp "" (substring rem-line 1 (string-length rem-line)) (append tokens (list (string-downcase buffer))))))
	     (else
	      (split-lp (string-append buffer (string cur-char)) (substring rem-line 1 (string-length rem-line)) tokens)))))))
  (let ((file (open-input-file filename)))
	(let token-lp ((tokens-list '())
		       (cur-line (read-line file)))
	  (if (eof-object? cur-line)
	      tokens-list
	      (token-lp (append tokens-list (list (split cur-line))) (read-line file))))))
    
;;; work for simple english below here

(define (parse filename)
  (let parse-lp ((k-list '())
                 (tokens (tokenize filename)))
    (if (= (length tokens) 0)
        k-list
        (parse-lp (append k-list (list (parse:tokens (car tokens)))) (cdr tokens)))))

;; Word class definition functions.

(define word-class-dictionary (make-eq-hash-table))

(define (create-word-class! class . wlist)
  (let loop ((wlist wlist))
    (cond ((null? wlist) class)
	  ((null? (cdr wlist)) (error "Malformed wlist"))
	  (else
	   (add-word-to-class! class (car wlist) #t)
	   (loop (cdr wlist))))))

(define (add-words-to-class! class . words)
  (for-each
   (lambda (word)
     (let ((wlist
	    (hash-table/get word-class-dictionary class '())))
       (let ((vcell (assq word wlist)))
	 (if vcell
	     (set-cdr! vcell value)
	     (hash-table/put! word-class-dictionary class
			      (cons (cons word #t) wlist))))))
   words)
  class)

(define (get-word-class class)
  (filter string?
	  (map car
	       (hash-table/get word-class-dictionary class '()))))

(define (get-full-word-class class)
  (let ((wlist (map car (hash-table/get word-class-dictionary class '()))))
    (append (filter string? wlist) (append-map (lambda (subclass) (get-word-subclass class subclass)) (filter symbol? wlist)))))

(define (get-word-class-subclasses class)
  (filter symbol?
	  (map car
	       (hash-table/get word-class-dictionary class '()))))

(define (remove-words-from-class! class . words)
  (for-each
   (lambda (word)
     (let ((wlist
	    (hash-table/get word-class-dictionary class '())))
       (let ((vcell (assoc word wlist)))
	 (if vcell
	     (hash-table/put! word-class-dictionary class (delete! vcell wlist))))))
   words)
  class)

(define (create-word-subclass! class subclass)
  (let ((wlist
	 (hash-table/get word-class-dictionary class '())))
    (let ((vcell (assq subclass wlist)))
      (if vcell
	  (set-cdr! vcell '())
	  (hash-table/put! word-class-dictionary class
			   (cons (cons subclass '()) wlist)))))
  (list class subclass))

(define (remove-word-subclass! class subclass)
  (let ((wlist
	 (hash-table/get word-class-dictionary class '())))
    (let ((vcell (assq subclass wlist)))
      (if vcell
	  (hash-table/put! word-class-dictionary class (delete! vcell wlist)))))
  (list class subclass))

(define (add-words-to-subclass! class subclass . words)
  (let* ((wlist-top
	  (hash-table/get word-class-dictionary class '()))
	 (wlist-sub
	  (assq subclass wlist-top)))
    (if wlist-sub
	(append! wlist-sub words)
	(begin
	  (create-word-subclass! class subclass)
	  (apply add-words-to-subclass! (cons class (cons subclass words))))))
  (list class subclass))

(define (remove-word-from-subclass! class subclass word)
  (let* ((wlist-top
	  (hash-table/get word-class-dictionary class '()))
	 (wlist-sub
	  (assq subclass wlist-top)))
    (if wlist-sub
	(delete! word wlist-sub)))
  (list class subclass))

(define (get-word-subclass class subclass)
  (let ((wlist
	 (hash-table/get word-class-dictionary class '())))
    (cdr (assq subclass wlist))))

;; predicates for language
;; appear disappear take give hurt help
;; possession

(define (syn:single-match tokens match-pattern)
  ((match:->combinators match-pattern) tokens '() (lambda (dict) dict)))

(define (syn:class-match tokens word-class match-pattern)
  (call-with-current-continuation
   (lambda (exit)
     (for-each 
      (lambda (word)
	(let ((parsed ((match:->combinators (match-pattern word)) tokens '() (lambda (dict) dict))))
	  (if parsed
	      (exit (cons (list 'class-word word) parsed)))))
      word-class)
     #f)))

(define (syn:high-priority? tokens)
  (or
   (syn:is-a? tokens)
   (syn:if-then? tokens)))

(define (syn:medium-priority? tokens)
  (if (not (syn:high-priority? tokens))
      (or
       (syn:take? tokens)
       (syn:take-from? tokens)
       (syn:harm? tokens))
      #f))

(define (syn:medium-priority tokens match)
  (if (not (syn:high-priority? tokens))
      match
      #f))

(define (syn:low-priority? tokens)
  (if (not (syn:medium-priority? tokens))
      (or
       (syn:generic-action? tokens)
       )
      #f))

(define (syn:low-priority tokens match)
  (if (not 
       (or
	(syn:high-priority? tokens)
	(syn:medium-priority? tokens)))
      match
      #f))

(define (syn:floor-priority tokens match)
  (if (not
       (or
	(syn:high-priority? tokens)
	(syn:medium-priority? tokens)
	(syn:low-priority? tokens)))
      match
      #f))

(define (syn:is-a? tokens)
  (syn:single-match tokens '((?? object) "is" (?? property))))

(define (syn:article? tokens)
  (syn:floor-priority
   tokens
   (syn:class-match tokens (get-full-word-class 'article) (lambda (word) `(,word (?? object))))))

(define (syn:if-then? tokens)
  (syn:single-match tokens '("if" (?? predicate) "then" (?? consequent))))

(define (syn:take? tokens)
  (syn:medium-priority
   tokens
   (syn:class-match tokens (get-word-subclass 'action 'take) (lambda (word) `((?? taker) ,word (?? object))))))

(define (syn:take-from? tokens)
  (syn:medium-priority
   tokens
   (syn:class-match tokens (get-word-subclass 'action 'take) (lambda (word) `((?? taker) ,word (?? object) "from" (?? takee))))))

(define (syn:harm? tokens)
  (syn:medium-priority
   tokens
   (syn:class-match tokens (get-word-subclass 'action 'harm) (lambda (word) `((?? agressor) ,word (?? victim))))))

(define (syn:generic-action? tokens)
  (syn:low-priority
   tokens
   (syn:class-match tokens (get-full-word-class 'action) (lambda (word) `((?? actor) ,word (?? else))))))

(define (syn:path? tokens)
  (syn:medium-priority
   tokens
   (syn:class-match tokens (get-full-word-class 'path) (lambda (word) `(,word (?? else))))))

;; handler for parsing language

(define parse:tokens
  (make-generic-operator 1 'nop (lambda (tokens) tokens)))

(defhandler parse:tokens
  (lambda (tokens)
    (let ((parsed (syn:is-a? tokens)))
      `(IS-A ,(parse:tokens (cadr (assq 'object parsed)))
	     ,(parse:tokens (cadr (assq 'property parsed))))))
  syn:is-a?)

(defhandler parse:tokens
  (lambda (tokens)
    (cdr tokens))
  syn:article?)

(defhandler parse:tokens
  (lambda (tokens)
    (let ((parsed (syn:if-then? tokens)))
      `(IF-THEN ,(parse:tokens (cadr (assq 'predicate parsed)))
		,(parse:tokens (cadr (assq 'consequent parsed))))))
  syn:if-then?)

(defhandler parse:tokens
  (lambda (tokens)
    (let ((parsed (syn:take? tokens)))
      `(TAKE ,(parse:tokens (cadr (assq 'taker parsed)))
	     ,(parse:tokens (cadr (assq 'object parsed))))))
  syn:take?)

(defhandler parse:tokens
  (lambda (tokens)
    (let ((parsed (syn:take-from? tokens)))
      `(TAKE-FROM ,(parse:tokens (cadr (assq 'taker parsed)))
		  ,(parse:tokens (cadr (assq 'object parsed)))
		  ,(parse:tokens (cadr (assq 'takee parsed))))))
  syn:take-from?)

(defhandler parse:tokens
  (lambda (tokens)
    (let ((parsed (syn:harm? tokens)))
      `(HARMS ,(parse:tokens (cadr (assq 'agressor parsed)))
	      ,(parse:tokens (cadr (assq 'victim parsed))))))
  syn:harm?)

(defhandler parse:tokens
  (lambda (tokens)
    (let ((parsed (syn:generic-action? tokens)))
      `(ACTION ,(parse:tokens (cadr (assq 'class-word parsed)))
	       ,(parse:tokens (cadr (assq 'actor parsed)))
	       ,(parse:tokens (cadr (assq 'else parsed))))))
  syn:generic-action?)

(defhandler parse:tokens
  (lambda (tokens)
    (let ((parsed (syn:path? tokens)))
      `(PATH ,(parse:tokens (cadr (assq 'class-word parsed)))
	     ,(parse:tokens (cadr (assq 'else parsed))))))
  syn:path?)

;; dictionary

(create-word-class! 'article)
(add-words-to-class! 'article "a" "an" "the")

(create-word-class! 'action)
(add-words-to-class! 'action "jump" "jumps" "runs")

(create-word-class! 'path)
(add-words-to-class! 'path "over" "under" "through" "past" "by")

(create-word-subclass! 'action 'take)
(add-words-to-subclass! 'action 'take "take" "takes" "annex" "annexed")

(create-word-subclass! 'action 'harm)
(add-words-to-subclass! 'action 'harm "hit" "hurt" "kill" "attack" "harm" "harms")
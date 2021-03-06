;;; Tokenizer

(define (split line split-charset punc-charset)
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
		(split-lp "" (substring rem-line 1 
					   (string-length rem-line)) tokens)
		(split-lp "" (substring rem-line 1 
					   (string-length rem-line)) 
					 (append tokens (list (string-downcase buffer))))))
	   ((char-set-member? punc-charset cur-char)
	    (if (= (string-length buffer) 0)
		(split-lp "" (substring rem-line 1 
					   (string-length rem-line)) tokens)
		(split-lp "" (substring rem-line 1 
					   (string-length rem-line)) 
					 (append tokens (list (string-downcase buffer))))))
	   (else
	    (split-lp (string-append buffer (string cur-char)) 
				  (substring rem-line 1 
				    (string-length rem-line)) tokens)))))))

(define split-charset (char-set #\space))
(define punc-charset (char-set #\! #\# #\$ #\% #\& #\( #\) #\* #\+ #\, 
							   #\- #\. #\/ #\: #\; #\< #\= #\> #\? #\@ 
							   #\[ #\\ #\] #\^ #\_ #\` #\{ #\| #\} #\~)) 

(define (tokenize filename)
  (let ((file (open-input-file filename)))
    (let token-lp ((tokens-list '())
		   (cur-line (read-line file)))
      (if (eof-object? cur-line)
	  tokens-list
	  (token-lp (append tokens-list 
				  (list (split cur-line split-charset punc-charset))) 
				(read-line file))))))

(define (tokenize-string string)
  (split string split-charset punc-charset))
    
;;; work for simple english below here

(define (parse filename)
  (let parse-lp ((k-list '())
                 (tokens (tokenize filename)))
    (if (= (length tokens) 0)
        k-list
        (parse-lp (append k-list 
				    (list (parse:tokens (car tokens)))) (cdr tokens)))))

(define (parse-string string)
  (parse:tokens (tokenize-string string)))

;; Word class definition functions.

(define word-class-dictionary (make-eq-hash-table))

(define (create-word-class! class . wlist)
  (let loop ((wlist wlist))
    (cond ((null? wlist) class)
	  ((null? (cdr wlist)) (error "Malformed wlist"))
	  (else
	   (add-words-to-class! class (car wlist) #t)
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
  (let ((wlist (map car 
			     (hash-table/get word-class-dictionary class '()))))
    (append (filter string? wlist) 
			(append-map 
			  (lambda (subclass) 
					  (get-word-subclass class subclass)) 
			  (filter symbol? wlist)))))

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
	     (hash-table/put! word-class-dictionary 
						  class 
						  (delete! vcell wlist))))))
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
	  (hash-table/put! word-class-dictionary 
					   class 
					   (delete! vcell wlist)))))
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
	  (apply add-words-to-subclass! 
			 (cons class (cons subclass words))))))
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
	(let ((parsed 
		     ((match:->combinators (match-pattern word)) tokens 
														 '() 
														 (lambda (dict) 
														   dict))))
	  (if parsed
	      (exit (cons (list 'class-word word) parsed)))))
      word-class)
     #f)))

(define (syn:high-priority? tokens)
  (or
   (syn:is-a? tokens)
   (syn:if-then? tokens)
   (syn:what-is? tokens)))
   
(define (syn:high-priority tokens match)
  match)

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

(define (syn:is-type? tokens)
  (syn:single-match tokens '((?? object) "is" "type" (?? type))))

(define (syn:is-a? tokens)
  (let ((parsed (syn:single-match tokens 
								  '((?? object) "is" (?? property)))))
    (if (and parsed
	     (not (syn:is-type? tokens)))
	(if (= (length (cadr (assq 'object parsed))) 0)
	    #f
	    parsed)
	#f)))

(define (syn:article? tokens)
  (syn:floor-priority
   tokens
   (syn:class-match tokens 
					(get-full-word-class 'article) 
					(lambda (word) `(,word (?? object))))))

(define (syn:if-then? tokens)
  (syn:single-match tokens 
    '("if" (?? predicate) "then" (?? consequent))))

(define (syn:take? tokens)
  (syn:medium-priority
   tokens
   (syn:class-match tokens 
					(get-word-subclass 'action 'take) 
					(lambda (word) `((?? taker) ,word (?? object))))))

(define (syn:take-from? tokens)
  (syn:medium-priority
   tokens
   (syn:class-match 
     tokens 
	 (get-word-subclass 'action 'take) 
	 (lambda (word) 
	   `((?? taker) ,word (?? object) "from" (?? takee))))))

(define (syn:harm? tokens)
  (syn:medium-priority
   tokens
   (syn:class-match 
     tokens 
	 (get-word-subclass 'action 'harm) 
	 (lambda (word) `((?? aggressor) ,word (?? victim))))))

(define (syn:generic-action? tokens)
  (syn:low-priority
   tokens
   (syn:class-match tokens 
					(get-full-word-class 'action) 
					(lambda (word) `((?? actor) ,word (?? else))))))

(define (syn:path? tokens)
  (syn:medium-priority
   tokens
   (syn:class-match tokens 
					(get-full-word-class 'path) 
					(lambda (word) `(,word (?? else))))))
   
(define (syn:what-is? tokens)
  (syn:high-priority
    tokens
    (syn:single-match tokens '("what" "is" (?? object)))))
    
(define (syn:who-is? tokens)
  (syn:high-priority
    tokens
    (syn:single-match tokens '("who" "is" (?? property)))))
    
(define (syn:who? tokens)
  (syn:medium-priority
    tokens
    (syn:single-match tokens '("who" (?? action)))))
    
(define (syn:when-is? tokens)
  (syn:high-priority
    tokens
    (syn:class-match tokens 
					 (get-full-word-class 'when) 
					 (lambda (word) `("when" ,word (?? consequent))))))
    
(define (syn:what-if? tokens)
  (syn:high-priority
    tokens
    (syn:single-match 
      tokens 
	  '("what" (?? construction) "if" (?? predicate)))))

;; handler for parsing language

(define (parse:create-assoc property parsed #!optional tag)
  (let ((result (parse:tokens (cadr (assq property parsed)))))
    (if (and (pair? result)
	     (= (length result) 1))
	(set! result (car result)))
    (if (default-object? tag)
	(let ((tag property))
	  (if (string? result)
	      (cons tag (list result))
	      (list tag result)))
	(if (string? result)
	    (cons tag (list result))
	    (list tag result)))))
	    
(define parse:tokens
  (make-generic-operator 1 'nop (lambda (tokens) tokens)))

(defhandler parse:tokens
  (lambda (tokens)
    (let ((parsed (syn:is-type? tokens)))
      `(IS-TYPE
	,(list
	  (parse:create-assoc 'object parsed)
	  (parse:create-assoc 'type parsed)))))
  syn:is-type?)

(defhandler parse:tokens
  (lambda (tokens)
    (let ((parsed (syn:is-a? tokens)))
      `(IS-A
	,(list
	  (parse:create-assoc 'object parsed)
	  (parse:create-assoc 'property parsed)))))
  syn:is-a?)

(defhandler parse:tokens
  (lambda (tokens)
    (cdr tokens))
  syn:article?)

(defhandler parse:tokens
  (lambda (tokens)
    (let ((parsed (syn:if-then? tokens)))
      `(IF-THEN
	,(list
	  (parse:create-assoc 'predicate parsed)
	  (parse:create-assoc 'consequent parsed)))))
  syn:if-then?)

(defhandler parse:tokens
  (lambda (tokens)
    (let ((parsed (syn:take? tokens)))
      `(TAKE 
	,(list
	  (parse:create-assoc 'taker parsed)
	  (parse:create-assoc 'object parsed)))))
  syn:take?)

(defhandler parse:tokens
  (lambda (tokens)
    (let ((parsed (syn:take-from? tokens)))
      `(TAKE-FROM 
	,(list
	  (parse:create-assoc 'taker parsed)
	  (parse:create-assoc 'object parsed)
	  (parse:create-assoc 'takee parsed)))))
  syn:take-from?)

(defhandler parse:tokens
  (lambda (tokens)
    (let ((parsed (syn:harm? tokens)))
      `(HARMS 
	,(list
	  (parse:create-assoc 'aggressor parsed)
	  (parse:create-assoc 'victim parsed)))))
  syn:harm?)

(defhandler parse:tokens
  (lambda (tokens)
    (let ((parsed (syn:generic-action? tokens)))
      `(ACTION 
	,(list
	  (parse:create-assoc 'class-word parsed 'action)
	  (parse:create-assoc 'actor parsed)
	  (parse:create-assoc 'else parsed)))))
  syn:generic-action?)

(defhandler parse:tokens
  (lambda (tokens)
    (let ((parsed (syn:path? tokens)))
      `(PATH 
	,(list
	  (parse:create-assoc 'class-word parsed 'path)
	  (parse:create-assoc 'else parsed)))))
  syn:path?)

(defhandler parse:tokens
  (lambda (tokens)
    (let ((parsed (syn:what-is? tokens)))
      `(WHAT-IS
	,(list
	  (parse:create-assoc 'object parsed)))))
  syn:what-is?)
  
(defhandler parse:tokens
  (lambda (tokens)
    (let ((parsed (syn:who-is? tokens)))
      `(WHO-IS
	,(list
	  (parse:create-assoc 'property parsed)))))
  syn:who-is?)
  
(defhandler parse:tokens
  (lambda (tokens)
    (let ((parsed (syn:who? tokens)))
      `(WHO ,parsed)))
  syn:who?)
  
(defhandler parse:tokens
  (lambda (tokens)
    (let ((parsed (syn:when-is? tokens)))
      `(WHEN-IS
	,(list
	  (parse:create-assoc 'class-word parsed 'when-is)
	  (parse:create-assoc 'consequent parsed)))))
  syn:when-is?)
  
(defhandler parse:tokens
  (lambda (tokens)
    (let ((parsed (syn:what-if? tokens)))
      `(WHAT-IF
	,(list
	  (parse:create-assoc 'predicate parsed)))))
  syn:what-if?)

;; dictionary

(create-word-class! 'article)
(add-words-to-class! 'article "a" "an" "the")

(create-word-class! 'action)
(add-words-to-class! 'action "jump" "jumps" "runs" "likes" "crawls")

(create-word-class! 'path)
(add-words-to-class! 'path "over" "under" "through" "past" "by" "to" 
						   "above")

(create-word-class! 'justification)
(add-words-to-class! 'justification "justify" "justifies" "justified"
								   "allow" "allows" "allowed"
								   "permit" "permits" "permitted")
								   
(create-word-class! 'query)
(add-words-to-class! 'query "what" "why" "when" "who" "how" "where")

(create-word-class! 'when)
(add-words-to-class! 'when "is" "does" "are" "do")

(create-word-subclass! 'action 'take)
(add-words-to-subclass! 'action 'take "take" "takes" "took" 
									  "annex" "annexed")

(create-word-subclass! 'action 'harm)
(add-words-to-subclass! 'action 'harm "hit" "hits" "hurt" "hurts" 
									  "kill" "kills" "killed" 
									  "attack" "attacks" "attacked" 
									  "harm" "harms" "harmed"
									  "fight" "fights" "fought")

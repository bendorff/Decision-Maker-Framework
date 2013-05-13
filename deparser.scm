;;; Deparser

; Example token sets needing to be deparsed:
;
; (is-type ((object "america") (type "country")))
; (is-a ((object "america") (property "powerful")))
; ("america") ;; no articles
; (if-then ((predicate "a1") (consequent "a2")))
; (harms ((aggressor "america") (victim "britain")))
; (action ((action "runs") (actor "america") (else)))
; (action ((action "jumps") (actor "dog") 
;							(else path ((path "over") (else "cat")))))

(define (deparse:get-parsed-type parsed-tokens)
  (car parsed-tokens))

(define (deparse:get-parsed-assoc parsed-tokens)
  (cadr parsed-tokens))

(define (deparse:get-property property parsed-tokens)
  (cadr (assoc property (deparse:get-parsed-assoc parsed-tokens))))

(define (deparse:is-type? type)
  (lambda (tokens)
    (if (pair? tokens)
	(equal? type (deparse:get-parsed-type tokens))
	#f)))

(define (deparse:spacify tokens)
  (if (= (length tokens) 1)
      tokens
      (let lp ((rem-tok tokens)
	       (ret '()))
	(if (= (length rem-tok) 1)
	    (append ret (list (car rem-tok)))
	    (lp (cdr rem-tok)
		(append ret (list (car rem-tok)) '(" ")))))))

(define deparse:tokens
  (make-generic-operator
   1
   'default
   (lambda (tokens)
     (apply string-append (deparse:spacify tokens)))))

(defhandler deparse:tokens
  (lambda (tokens)
    tokens)
  string?)

(defhandler deparse:tokens
  (lambda (tokens)
    (string-append 
      (deparse:tokens (deparse:get-property 'object tokens))
	  " is type "
	  (deparse:tokens (deparse:get-property 'type tokens))
	  "."))
  (deparse:is-type? 'is-type))

(defhandler deparse:tokens
  (lambda (tokens)
    (string-append 
      (deparse:tokens (deparse:get-property 'object tokens))
	  " is "
	  (deparse:tokens (deparse:get-property 'property tokens))
	  "."))
  (deparse:is-type? 'is-a))

(defhandler deparse:tokens
  (lambda (tokens)
    (string-append 
           "If "
		   (deparse:tokens (deparse:get-property 'predicate tokens))
		   ", then "
		   (deparse:tokens (deparse:get-property 'consequent tokens))
		   "."))
  (deparse:is-type? 'if-then))

(defhandler deparse:tokens
  (lambda (tokens)
    (string-append 
      (deparse:tokens (deparse:get-property 'aggressor tokens))
	  " harms "
	  (deparse:tokens (deparse:get-property 'victim tokens))
	  "."))
  (deparse:is-type? 'harms))

(defhandler deparse:tokens
  (lambda (tokens)
    (string-append 
      (deparse:tokens (deparse:get-property 'taker tokens))
	  " takes "
	  (deparse:tokens (deparse:get-property 'object tokens))
	  "."))
  (deparse:is-type? 'take))

(defhandler deparse:tokens
  (lambda (tokens)
    (string-append 
      (deparse:tokens (deparse:get-property 'taker tokens))
	  " takes "
	  (deparse:tokens (deparse:get-property 'object tokens))
	  " from "
	  (deparse:tokens (deparse:get-property 'takee tokens))
	  "."))
  (deparse:is-type? 'take-from))

(defhandler deparse:tokens
  (lambda (tokens)
    (let ((tmpstring ""))
      (if (null? (deparse:get-property 'else tokens))
	  (set! tmpstring ".")
	  (set! tmpstring (string-append 
					    (deparse:get-property 'else tokens) 
					    ".")))
      (string-append 
        (deparse:tokens (deparse:get-property 'actor tokens))
		" "
		(deparse:tokens (deparse:get-property 'action tokens))
		tmpstring)))
  (deparse:is-type? 'action))

(defhandler deparse:tokens
  (lambda (tokens)
    (string-append 
      (deparse:tokens (deparse:get-property 'path tokens))
	  " "
	  (deparse:tokens (deparse:get-property 'else tokens))))
  (deparse:is-type? 'path))
  
(defhandler deparse:tokens
  (lambda (tokens)
    (string-append "What is "
		   (deparse:tokens (deparse:get-property 'object tokens))
		   "?"))
  (deparse:is-type? 'what-is))
  
(defhandler deparse:tokens
  (lambda (tokens)
    (string-append "Who is "
		   (deparse:tokens (deparse:get-property 'property tokens))
		   "?"))
  (deparse:is-type? 'who-is))
  
(defhandler deparse:tokens
  (lambda (tokens)
    (string-append "Who "
		   (deparse:tokens (deparse:get-property 'action tokens))
		   "?"))
  (deparse:is-type? 'who))
  
(defhandler deparse:tokens
  (lambda (tokens)
    (string-append "When "
    	   (deparse:tokens (deparse:get-property 'when-is tokens))
    	   " "
		   (deparse:tokens (deparse:get-property 'consequent tokens))
		   "?"))
  (deparse:is-type? 'when-is))
  
(defhandler deparse:tokens
  (lambda (tokens)
    (string-append "What happens if "
		   (deparse:tokens (deparse:get-property 'predicate tokens))
		   "?"))
  (deparse:is-type? 'what-if))

(define (deparse:kb-get-parsed-type kb-tokens)
  (if (pair? (cadr kb-tokens))
      (cadr (assoc 'type (cadr kb-tokens)))
      #f))

(define (deparse:kb-type? type)
  (lambda (kb-tokens)
    (and
     (pair? kb-tokens)
     (not (= (length kb-tokens) 1))
     (equal? (deparse:kb-get-parsed-type kb-tokens) type))))

(define deparse:kb
  (make-generic-operator 
   1 
   'tmp 
   (lambda (kb-tokens) 
     (if (pair? kb-tokens)
	 (apply string-append (deparse:spacify kb-tokens))
	 kb-tokens))))

(defhandler deparse:kb
  (lambda (kb-tokens)
    (string-append "If "
		   (deparse:tokens (caadar kb-tokens))
		   "."))
  (deparse:kb-type? 'predicate))

(defhandler deparse:kb
  (lambda (kb-tokens)
    (string-append "Then "
		   (deparse:kb (caadar kb-tokens))
		   "."))
  (deparse:kb-type? 'consequent))

(define (deparse:kb-multi kb-tokens)
  (map
   (lambda (new-kb-tokens)
     (deparse:kb new-kb-tokens))
   (map (lambda (old-tokens) 
				(cons (car kb-tokens) (list old-tokens))) 
		kb-tokens)))

;works
(defhandler deparse:kb
  (lambda (kb-tokens)
    (string-append "It is "
		   (deparse:kb (cadadr kb-tokens))
		   "."))
  (deparse:kb-type? 'is-a))

;works
(defhandler deparse:kb
  (lambda (kb-tokens)
    (string-append "It is type "
		   (deparse:kb (cadadr kb-tokens))
		   "."))
  (deparse:kb-type? 'is-type))

;works
(defhandler deparse:kb
  (lambda (kb-tokens)
    (string-append (deparse:kb (cadadr kb-tokens))
		   " has this property."))
  (deparse:kb-type? 'is-a-prop))

;works
(defhandler deparse:kb
  (lambda (kb-tokens)
    (string-append (deparse:kb (cadadr kb-tokens))
		   " has this type."))
  (deparse:kb-type? 'is-a-type))
  
(defhandler deparse:kb
  (lambda (kb-tokens)
    (string-append "It is "
		   (deparse:kb (cadadr kb-tokens))
		   "."))
  (deparse:kb-type? 'what-is))
  
(defhandler deparse:kb
  (lambda (kb-tokens)
    (string-append "It is "
		   (deparse:kb (cadadr kb-tokens))
		   "."))
  (deparse:kb-type? 'who-is))
  
(defhandler deparse:kb
  (lambda (kb-tokens)
    (string-append (deparse:kb (cadadr kb-tokens))
		   "does."))
  (deparse:kb-type? 'who))
  
(defhandler deparse:kb
  (lambda (kb-tokens)
    (string-append "When "
		   (deparse:kb (cadadr kb-tokens))
		   "."))
  (deparse:kb-type? 'when-is))
  
(defhandler deparse:kb
  (lambda (kb-tokens)
    (string-append 
		   (deparse:kb (cadadr kb-tokens))
		   "."))
  (deparse:kb-type? 'what-if))

(defhandler deparse:kb
  (lambda (kb-tokens)
    (string-append "It takes "
		   (deparse:kb (cadr (assoc 'object (cadr kb-tokens))))
		   "."))
  (deparse:kb-type? 'takes))

(defhandler deparse:kb
  (lambda (kb-tokens)
    (pp (cdr kb-tokens))
    (string-append "It takes "
		   (deparse:kb (cadr (assoc 'object (cadr kb-tokens))))
		   " from "
		   (deparse:kb (cadr (assoc 'takee (cadr kb-tokens))))
		   "."))
  (deparse:kb-type? 'takes-from))

(defhandler deparse:kb
  (lambda (kb-tokens)
    (string-append "It loses "
		   (deparse:kb (cadr (assoc 'object (cadr kb-tokens))))
		   " to "
		   (deparse:kb (cadr (assoc 'taker (cadr kb-tokens))))
		   "."))
  (deparse:kb-type? 'loses-to))

(defhandler deparse:kb
  (lambda (kb-tokens)
    (string-append "It harms "
		   (deparse:kb (cadr (assoc 'victim (cadr kb-tokens))))
		   "."))
  (deparse:kb-type? 'harms))

(defhandler deparse:kb
  (lambda (kb-tokens)
    (string-append "It is harmed by "
		   (deparse:kb (cadr (assoc 'aggressor (cadr kb-tokens))))
		   "."))
  (deparse:kb-type? 'harmed-by))

(defhandler deparse:kb
  (lambda (kb-tokens)
    (let ((tmpstr ""))
      (if (not (assoc 'else (cdr kb-tokens)))
	  (set! tmpstr ".")
	  (set! tmpstr (string-append 
				     (cadr (assoc 'else (cadr kb-tokens))) 
				     ".")))
      (string-append 
        "It "
		(deparse:kb (cadr (assoc 'action (cadr kb-tokens))))
		tmpstr)))
  (deparse:kb-type? 'action))

(defhandler deparse:kb
  (lambda (kb-tokens)
    (let ((tmpstr ""))
      (if (not (assoc 'else (cdr kb-tokens)))
	  (set! tmpstr ".")
	  (set! tmpstr (string-append 
				     " " 
				     (cadr (assoc 'else (cadr kb-tokens))) 
				     ".")))
      (string-append 
        (deparse:kb (cadr (assoc 'actor (cadr kb-tokens))))
		" does it"
		tmpstr)))
  (deparse:kb-type? 'actor))

; is type
; is a
; article
; if-then
; harm
; generic action
; path

; (is-type ((object "america") (type "country")))
; (is-a ((object "america") (property "powerful")))
; ("america") ;; no articles
; (if-then ((predicate "a1") (consequent "a2")))
; (harms ((aggressor "america") (victim "britain")))
; (action ((action "runs") (actor "america") (else)))
; (action ((action "jumps") (actor "dog") (else path ((path "over") (else "cat")))))

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
    (string-append (deparse:tokens (deparse:get-property 'object tokens))
		   " is type "
		   (deparse:tokens (deparse:get-property 'type tokens))
		   "."))
  (deparse:is-type? 'is-type))

(defhandler deparse:tokens
  (lambda (tokens)
    (string-append (deparse:tokens (deparse:get-property 'object tokens))
		   " is "
		   (deparse:tokens (deparse:get-property 'property tokens))
		   "."))
  (deparse:is-type? 'is-a))

(defhandler deparse:tokens
  (lambda (tokens)
    (string-append "If "
		   (deparse:tokens (deparse:get-property 'predicate tokens))
		   ", then "
		   (deparse:tokens (deparse:get-property 'consequent tokens))
		   "."))
  (deparse:is-type? 'if-then))

(defhandler deparse:tokens
  (lambda (tokens)
    (string-append (deparse:tokens (deparse:get-property 'aggressor tokens))
		   " harms "
		   (deparse:tokens (deparse:get-property 'victim tokens))
		    "."))
  (deparse:is-type? 'harms))

(defhandler deparse:tokens
  (lambda (tokens)
    (string-append (deparse:tokens (deparse:get-property 'taker tokens))
		   " takes "
		   (deparse:tokens (deparse:get-property 'object tokens))
		    "."))
  (deparse:is-type? 'take))

(defhandler deparse:tokens
  (lambda (tokens)
    (string-append (deparse:tokens (deparse:get-property 'taker tokens))
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
	  (set! tmpstring (string-append (deparse:get-property 'else tokens) ".")))
      (string-append (deparse:tokens (deparse:get-property 'actor tokens))
		     " "
		     (deparse:tokens (deparse:get-property 'action tokens))
		     tmpstring)))
  (deparse:is-type? 'action))

(defhandler deparse:tokens
  (lambda (tokens)
    (string-append (deparse:tokens (deparse:get-property 'path tokens))
		   " "
		   (deparse:tokens (deparse:get-property 'else tokens))))
  (deparse:is-type? 'path))
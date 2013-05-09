;; knowledge base for decision maker

(define knowledge-base (make-eq-hash-table))

;; NOTE: This *will* reinitialize the knowledge base, erasing all the data within!
(define (know:initialize-knowledge-base!)
  (hash-table/put! knowledge-base 'event-knowledge (make-equal-hash-table))
  (hash-table/put! knowledge-base 'rules (make-equal-hash-table))
  (hash-table/put! knowledge-base 'facts (make-equal-hash-table))
  'done)

(define (know:retrieve-knowledge-base knowledge-type)
  (hash-table/get knowledge-base knowledge-type '()))
  
(define (know:get-parsed-type parsed-tokens)
  (car parsed-tokens))

(define (know:get-parsed-assoc parsed-tokens)
  (cadr parsed-tokens))

(define (know:create-rule! parsed-tokens)
  (let ((kb (know:retrieve-knowledge-base 'rules))
	(type (know:get-parsed-type parsed-tokens))
	(asc (know:get-parsed-assoc parsed-tokens)))
    (let ((preds
	   (hash-table/get kb (assoc 'predicate asc) '()))
	  (cnsqs
	   (hash-table/get kb (assoc 'consequent asc) '())))
      (hash-table/put! kb (cdr (assoc 'predicate asc)) (cons (list (cons 'type '(consequent)) (cdr (assoc 'consequent asc))) preds))
      (hash-table/put! kb (cdr (assoc 'consequent asc)) (cons (list (cons 'type '(predicate)) (cdr (assoc 'predicate asc))) cnsqs))))
'done)

(define (know:create-fact! parsed-tokens)
  (let ((kb (know:retrieve-knowledge-base 'facts))
	(type (know:get-parsed-type parsed-tokens))
	(asc (know:get-parsed-assoc parsed-tokens)))
    (let ((objs
	   (hash-table/get kb (assoc 'object asc) '()))
	  (props
	   (hash-table/get kb (assoc 'property asc) '())))
    (hash-table/put! kb (cdr (assoc 'object asc)) (cons (list (cons 'type '(is-a)) (cdr (assoc 'property asc))) objs))
    (hash-table/put! kb (cdr (assoc 'property asc)) (cons (list (cons 'type '(is-a-prop)) (cdr (assoc 'object asc))) props))))
'done)

(define (know:is-event-type? event-type)
  (lambda (parsed-tokens)
    (equal? (know:get-parsed-type parsed-tokens) event-type)))

(define know:create-specialized-event-knowledge!
  (make-generic-operator 1 'error (lambda (parsed-tokens) (error "Handler not found"))))

(defhandler know:create-specialized-event-knowledge!
  (lambda (parsed-tokens)
    (let ((kb (know:retrieve-knowledge-base 'event-knowledge))
	  (asc (know:get-parsed-assoc parsed-tokens)))
      (let ((objs
	     (hash-table/get kb (cdr (assoc 'taker asc)) '())))
	(hash-table/put! kb (cdr (assoc 'taker asc)) (cons (list (cons 'type '(takes)) (assoc 'object asc)) objs)))))
  (know:is-event-type? 'take))

(defhandler know:create-specialized-event-knowledge!
  (lambda (parsed-tokens)
    (let ((kb (know:retrieve-knowledge-base 'event-knowledge))
	  (asc (know:get-parsed-assoc parsed-tokens)))
      (let ((objs-gained
	     (hash-table/get kb (cdr (assoc 'taker asc)) '()))
	    (objs-lost
	     (hash-table/get kb (cdr (assoc 'takee asc)) '())))
	(pp objs-gained)
	(pp objs-lost)
	(hash-table/put! kb (cdr (assoc 'taker asc)) (cons (list (cons 'type '(takes)) (assoc 'object asc) (assoc 'takee asc)) objs-gained))
	(hash-table/put! kb (cdr (assoc 'takee asc)) (cons (list (cons 'type '(loses)) (assoc 'object asc) (assoc 'taker asc)) objs-lost)))))
  (know:is-event-type? 'take-from))

(defhandler know:create-specialized-event-knowledge!
  (lambda (parsed-tokens)
    (let ((kb (know:retrieve-knowledge-base 'event-knowledge))
	  (asc (know:get-parsed-assoc parsed-tokens)))
      (let ((harms
	     (hash-table/get kb (cdr (assoc 'aggressor asc)) '()))
	    (harmed
	     (hash-table/get kb (cdr (assoc 'victim asc)) '())))
	(hash-table/put! kb (cdr (assoc 'aggressor asc)) (cons (list (cons 'type (list 'harms)) (assoc 'victim asc)) harms))
	(hash-table/put! kb (cdr (assoc 'victim asc)) (cons (list (cons 'type (list 'harmed-by)) (assoc 'aggressor asc)) harmed)))))
  (know:is-event-type? 'harms))

(defhandler know:create-specialized-event-knowledge!
  (lambda (parsed-tokens)
    (let ((kb (know:retrieve-knowledge-base 'event-knowledge))
	  (asc (know:get-parsed-assoc parsed-tokens)))
      (let ((actions
	     (hash-table/get kb (cdr (assoc 'actor asc)) '()))
	    (actors
	     (hash-table/get kb (cdr (assoc 'action asc)) '())))
	(hash-table/put! kb (cdr (assoc 'actor asc)) (cons (list (cons 'type '(action)) (cons 'action (cdr (assoc 'action asc))) (assoc 'else asc)) actions))
	(hash-table/put! kb (cdr (assoc 'action asc)) (cons (list (cons 'type '(actor)) (assoc 'actor asc) (assoc 'else asc)) actors)))))
  (know:is-event-type? 'action))
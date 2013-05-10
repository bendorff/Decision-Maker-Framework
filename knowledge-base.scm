;; knowledge base for decision maker

;;;; Example of usage:
;;
;; Putting knowledge into the system from a file:
;; (know:learn-file! "example2.txt")
;; 
;; To see what's in the knowledge base:
;; (know:find-all-events)
;; (know:find-all-facts)
;; (know:find-all-rules)
;;
;; To query:
;;
;; (know:find-consequence (parse-string "america harms britain"))
;; (know:find-cause (parse-string "america and britain go to war"))
;;
;; (know:find-properties "america")
;; (know:find-types "america")
;; 
;; (know:find-object-by-type "country")
;; (know:find-object-by-prop "powerful")
;;
;; (know:find-actions "runs")
;; (know:find-actors "britain")
;;

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

(define (know:is-type? type)
  (lambda (parsed-tokens)
    (equal? (know:get-parsed-type parsed-tokens) type)))

(define know:create-rule!
  (make-generic-operator 1 'error (lambda (parsed-tokens) (error "Handler not found"))))

(defhandler know:create-rule!
  (lambda (parsed-tokens)
    (let ((kb (know:retrieve-knowledge-base 'rules))
	  (type (know:get-parsed-type parsed-tokens))
	  (asc (know:get-parsed-assoc parsed-tokens)))
      (let ((preds
	     (hash-table/get kb (cadr (assoc 'predicate asc)) '()))
	    (cnsqs
	     (hash-table/get kb (cadr (assoc 'consequent asc)) '())))
	(hash-table/put! kb (cadr (assoc 'predicate asc)) (cons (list (cons 'type '(consequent)) (cdr (assoc 'consequent asc))) preds))
	(hash-table/put! kb (cadr (assoc 'consequent asc)) (cons (list (cons 'type '(predicate)) (cdr (assoc 'predicate asc))) cnsqs))))
    'done)
  (know:is-type? 'if-then))

(define know:create-fact!
  (make-generic-operator 1 'error (lambda (parsed-tokens) (error "Handler not found"))))

(defhandler know:create-fact!
  (lambda (parsed-tokens)
    (let ((kb (know:retrieve-knowledge-base 'facts))
	  (type (know:get-parsed-type parsed-tokens))
	  (asc (know:get-parsed-assoc parsed-tokens)))
      (let ((objs
	     (hash-table/get kb (cadr (assoc 'object asc)) '()))
	    (props
	     (hash-table/get kb (cadr (assoc 'property asc)) '())))
	(hash-table/put! kb (cadr (assoc 'object asc)) (cons (list (cons 'type '(is-a)) (cdr (assoc 'property asc))) objs))
	(hash-table/put! kb (cadr (assoc 'property asc)) (cons (list (cons 'type '(is-a-prop)) (cdr (assoc 'object asc))) props))))
    'done)
  (know:is-type? 'is-a))

(defhandler know:create-fact!
  (lambda (parsed-tokens)
    (let ((kb (know:retrieve-knowledge-base 'facts))
	  (type (know:get-parsed-type parsed-tokens))
	  (asc (know:get-parsed-assoc parsed-tokens)))
      (let ((objs
	     (hash-table/get kb (cadr (assoc 'type asc)) '()))
	    (types
	     (hash-table/get kb (cadr (assoc 'object asc)) '())))
	(hash-table/put! kb (cadr (assoc 'object asc)) (cons (list (cons 'type '(is-type)) (cdr (assoc 'type asc))) types))
	(hash-table/put! kb (cadr (assoc 'type asc)) (cons (list (cons 'type '(is-a-type)) (cdr (assoc 'object asc))) objs))))
    'done)
  (know:is-type? 'is-type))

(define know:create-specialized-event-knowledge!
  (make-generic-operator 1 'error (lambda (parsed-tokens) (error "Handler not found"))))

(defhandler know:create-specialized-event-knowledge!
  (lambda (parsed-tokens)
    (let ((kb (know:retrieve-knowledge-base 'event-knowledge))
	  (asc (know:get-parsed-assoc parsed-tokens)))
      (let ((objs
	     (hash-table/get kb (cadr (assoc 'taker asc)) '())))
	(hash-table/put! kb (cadr (assoc 'taker asc)) (cons (list (cons 'type '(takes)) (assoc 'object asc)) objs)))))
  (know:is-type? 'take))

(defhandler know:create-specialized-event-knowledge!
  (lambda (parsed-tokens)
    (let ((kb (know:retrieve-knowledge-base 'event-knowledge))
	  (asc (know:get-parsed-assoc parsed-tokens)))
      (let ((objs-gained
	     (hash-table/get kb (cadr (assoc 'taker asc)) '()))
	    (objs-lost
	     (hash-table/get kb (cadr (assoc 'takee asc)) '())))
	(hash-table/put! kb (cadr (assoc 'taker asc)) (cons (list (cons 'type '(takes-from)) (assoc 'object asc) (assoc 'takee asc)) objs-gained))
	(hash-table/put! kb (cadr (assoc 'takee asc)) (cons (list (cons 'type '(loses-to)) (assoc 'object asc) (assoc 'taker asc)) objs-lost)))))
  (know:is-type? 'take-from))

(defhandler know:create-specialized-event-knowledge!
  (lambda (parsed-tokens)
    (let ((kb (know:retrieve-knowledge-base 'event-knowledge))
	  (asc (know:get-parsed-assoc parsed-tokens)))
      (let ((harms
	     (hash-table/get kb (cadr (assoc 'aggressor asc)) '()))
	    (harmed
	     (hash-table/get kb (cadr (assoc 'victim asc)) '())))
	(hash-table/put! kb (cadr (assoc 'aggressor asc)) (cons (list (cons 'type (list harms)) (assoc 'victim asc)) harms))
	(hash-table/put! kb (cadr (assoc 'victim asc)) (cons (list (cons 'type (list 'harmed-by)) (assoc 'aggressor asc)) harmed)))))
  (know:is-type? 'harms))

(defhandler know:create-specialized-event-knowledge!
  (lambda (parsed-tokens)
    (let ((kb (know:retrieve-knowledge-base 'event-knowledge))
	  (asc (know:get-parsed-assoc parsed-tokens)))
      (let ((actions
	     (hash-table/get kb (cadr (assoc 'actor asc)) '()))
	    (actors
	     (hash-table/get kb (cadr (assoc 'action asc)) '())))
	(hash-table/put! kb (cadr (assoc 'actor asc)) (cons (list (cons 'type '(action)) (cons 'action (cdr (assoc 'action asc))) (assoc 'else asc)) actions))
	(hash-table/put! kb (cadr (assoc 'action asc)) (cons (list (cons 'type '(actor)) (assoc 'actor asc) (assoc 'else asc)) actors)))))
  (know:is-type? 'action))

(define (know:is-rule? parsed-tokens)
  (or
   ((know:is-type? 'if-then) parsed-tokens)
   ))

(define (know:is-fact? parsed-tokens)
  (or
   ((know:is-type? 'is-a) parsed-tokens)
   ((know:is-type? 'is-type) parsed-tokens)
   ))

(define (know:is-event? parsed-tokens)
  (or
   ((know:is-type? 'take) parsed-tokens)
   ((know:is-type? 'take-from) parsed-tokens)
   ((know:is-type? 'harms) parsed-tokens)
   ((know:is-type? 'action) parsed-tokens)
   ))

(define (know:learn! parsed-tokens)
  (cond ((know:is-rule? parsed-tokens)
	 (know:create-rule! parsed-tokens))
	((know:is-fact? parsed-tokens)
	 (know:create-fact! parsed-tokens))
	((know:is-event? parsed-tokens)
	 (know:create-specialized-event-knowledge! parsed-tokens))))

(define (know:learn-file! filename)
  (for-each know:learn! (parse filename)))

(define (know:find-consequence predicate)
  (hash-table/get (know:retrieve-knowledge-base 'rules) predicate '()))

(define (know:find-cause consequent)
  (hash-table/get (know:retrieve-knowledge-base 'rules) consequent '()))

(define (know:get-type-from-facts fact-list)
  (filter (lambda (fact) (equal? (cadar fact) 'is-type)) fact-list))

(define (know:get-props-from-facts fact-list)
  (filter (lambda (fact) (equal? (cadar fact) 'is-a)) fact-list))

(define (know:find-properties object)
  (know:get-props-from-facts
   (hash-table/get (know:retrieve-knowledge-base 'facts) object '())))

(define (know:find-types object)
  (know:get-type-from-facts
   (hash-table/get (know:retrieve-knowledge-base 'facts) object '())))

(define (know:find-facts object)
  (hash-table/get (know:retrieve-knowledge-base 'facts) object '()))

(define (know:find-object-by-type type)
  (hash-table/get (know:retrieve-knowledge-base 'facts) type '()))

(define (know:find-object-by-prop prop)
  (hash-table/get (know:retrieve-knowledge-base 'facts) prop '()))

(define (know:find-actions actor)
  (hash-table/get (know:retrieve-knowledge-base 'event-knowledge) actor '()))

(define (know:find-actors action)
  (hash-table/get (know:retrieve-knowledge-base 'event-knowledge) action '()))

;these functions dump the hash table as assoc lists
(define (know:find-all-events)
  (hash-table->alist (know:retrieve-knowledge-base 'event-knowledge)))

(define (know:find-all-facts)
  (hash-table->alist (know:retrieve-knowledge-base 'facts)))

(define (know:find-all-rules)
  (hash-table->alist (know:retrieve-knowledge-base 'rules))) 
  
(define (know:query query)
  (let 
    ((parsed-query (parse-string query)))
    (cond ((string=? "what-is" (string (car parsed-query))) 
		   (know:find-types (caar (cdaadr parsed-query))))
		  ((string=? "when-is" (string (car parsed-query))) 
		   (know:find-actors (caar (cdaadr parsed-query))))
		  ((string=? "who-is" (string (car parsed-query))) 
		   (know:find-object-by-prop (caar (cdaadr parsed-query))))
		  ((string=? "who" (string (car parsed-query))) 
		   (know:find-actors (caadr (caadar (cdaadr parsed-query)))))
	)))

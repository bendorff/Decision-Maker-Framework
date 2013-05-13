(define (syn:zimbg? tokens)
  (syn:single-match tokens '("groz" (? baz) (?? florg))))

(define (syn:zimbv? tokens)
  (syn:single-match tokens '((? baz) "verg" (?? florg))))

(define (syn:zimbt? tokens)
  (syn:single-match tokens '((? baz) (?? florg) "tam" (?? grog))))

(defhandler parse:tokens
  (lambda (tokens)
    (let ((parsed (syn:zimbg? tokens)))
      `(GROZ
	,(list
	  (parse:create-assoc 'baz parsed)
	  (parse:create-assoc 'florg parsed)))))
  syn:zimbg?)

(defhandler parse:tokens
  (lambda (tokens)
    (let ((parsed (syn:zimbv? tokens)))
      `(VERG
	,(list
	  (parse:create-assoc 'baz parsed)
	  (parse:create-assoc 'florg parsed)))))
  syn:zimbv?)

(defhandler parse:tokens
  (lambda (tokens)
    (let ((parsed (syn:zimbt? tokens)))
      `(TAM
	,(list
	(parse:create-assoc 'baz parsed)
	(parse:create-assoc 'florg parsed)
	(parse:create-assoc 'grog parsed)))))
  syn:zimbt?)
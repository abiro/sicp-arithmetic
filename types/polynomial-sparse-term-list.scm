;; sparse-term-list
;; Dependencies: dependencies/data-directed-dispatch.scm, dependencies/util.scm,
;; types/polynomial-term.scm

((lambda ()

   ;; internal procedures
   
   (define (coeff term) (apply-generic 'coeff term))
   
   (define make-term make-polynomial-term)

   (define (order term) (apply-generic 'order term))
   
   (define (tag term-list) (attach-tag 'sparse-term-list term-list))


   ;; exposed procedures
   
   (define (adjoin-term term term-list)
     (define (insert-term term term-list)
       (let ((t1 (first-term term-list)))
	 (cond ((= (order t1) (order term))
		(error "Term is already in term list -- ADJOIN-TERM"
		       term
		       term-list))
	       ((< (order t1) (order term)) (cons term term-list))
	       (else
		(cons t1 (insert-term term (rest-terms term-list)))))))
     (cond ((apply-generic '=zero? (coeff term)) term-list)
	   ((empty-term-list? term-list) (list term))
	   (else (insert-term term term-list))))

   (define (coefficients term-list) (map coeff term-list))
   
   (define empty-term-list? null?)
	   
   (define first-term car)

   (define (make-term-list term-list)
     (map (lambda (term)
	    (make-term (order term) (drop (coeff term))))
	  (filter (lambda (term) (not (apply-generic '=zero? (coeff term))))
		  term-list)))

   (define (map-terms proc term-list)
     (map proc term-list))
   
   (define (negate term-list)
     (map (lambda (term)
	    (make-term (order term)
		       (apply-generic 'negate (coeff term))))
	  term-list))
   
   (define rest-terms cdr)


   ;; interface to the rest of the system

   ;; TODO hackery with poly-term
   (put 'adjoin-term '(polynomial-term sparse-term-list)
	(lambda (term term-list)
	  (tag (adjoin-term (make-term (car term) (cadr term))
			    term-list))))

   (put 'all-terms '(sparse-term-list) identity)
   
   (put 'coefficients '(sparse-term-list) coefficients)
   
   (put 'empty-term-list? '(sparse-term-list) empty-term-list?)
   
   (put 'first-term '(sparse-term-list) first-term)

   (put 'make '(sparse-term-list)
	(lambda (term-list) (tag (make-term-list term-list))))

   ;; not to be used with apply-generic
   (put 'map-terms 'sparse-term-list
	(lambda (proc term-list) (tag (map-terms proc (contents term-list)))))
   
   (put 'negate '(sparse-term-list)
	(lambda (term-list) (tag (negate term-list))))
   
   (put 'rest-terms '(sparse-term-list)
	(lambda (term-list) (tag (rest-terms term-list))))

   (put 'the-empty-term-list 'sparse-term-list
	(lambda () (tag '())))
   
   'done))

;; dense-term-list
;; Dependency: dependencies/data-directed-dispatch.scm, dependencies/util.scm,
;; types/polynomial-term.scm
((lambda ()

   ;; internal procedures
   
   (define (coeff term) (apply-generic 'coeff term))

   (define make-term make-polynomial-term)

   (define (order term) (apply-generic 'order term))
   
   (define (tag term-list) (attach-tag 'dense-term-list term-list))


   ;; exposed procedures
   
   (define (adjoin-term term term-list)
     (define (insert-term term term-list)
       (let ((t1 (first-term term-list)))
	 (if (= (order t1) (order term))
	      (if (= (coeff t1) 0)
		  (cons (coeff term) (rest-terms term-list))
		  (error "Term is already in term list -- ADJOIN-TERM"
			 term
			 term-list))
	      (cons (coeff t1) (insert-term term (rest-terms term-list))))))
	       
     (define (prepend-term term term-list)
       (if (= (order term) (length term-list))
	   (cons (coeff term) term-list)
	   (prepend-term term (cons 0 term-list))))

     (cond ((=zero? (coeff term)) term-list)
	   ((< (order term) (length term-list)) (insert-term term term-list))
	   (else (prepend-term term term-list))))

   (define (all-terms term-list)
     (filter (lambda (term) (not (apply-generic '=zero? (coeff term))))
	     (map make-term
		  (reverse (iota (length term-list)))
		  term-list)))
   
   (define (first-term term-list)
     (make-term (-1+ (length term-list))
		(car term-list)))
   
   (define (make-term-list term-list)
     (map drop term-list))

   (define (map-terms proc term-list)
     (map coeff
	  (map proc
	       (all-terms term-list))))
   
   (define (negate term-list)
     (map (bind-to-args apply-generic 'negate)
	  term-list))
   
   (define (rest-terms term-list) (cdr term-list))


   ;; interface to the rest of the system
   
   ;; TODO hackery with poly term
   (put 'adjoin-term '(polynomial-term dense-term-list)
	(lambda (term term-list)
	  (tag (adjoin-term (make-term (car term)
				       (cadr term))
			    term-list))))

   (put 'all-terms '(dense-term-list) all-terms)
   
   (put 'coefficients '(dense-term-list) identity)

   (put 'empty-term-list? '(dense-term-list) null?)
   
   (put 'first-term '(dense-term-list) first-term)
   
   (put 'make '(dense-term-list)
	(lambda (term-list) (tag (make-term-list term-list))))

   ;; Not to be used with apply generic.
   (put 'map-terms 'dense-term-list
	(lambda (proc term-list) (tag (map-terms proc (contents term-list)))))
   
   (put 'negate '(dense-term-list)
	(lambda (term-list) (tag (negate term-list))))
   
   (put 'rest-terms '(dense-term-list)
	(lambda (term-list) (tag (rest-terms term-list))))

   (put 'the-empty-term-list 'dense-term-list
	(lambda () (tag '())))
   
   'done))

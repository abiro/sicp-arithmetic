;; polynomial
;; Dependencies: dependencies/data-directed-dispatch.scm, dependenceis/util.scm,
;; types/polynomial-dense-package.scm, types/polynomial-term.scm,
;; types/polynomial-sparse-package.scm

(load "types/polynomial-term.scm")
(load "types/polynomial-dense-term-list.scm")
(load "types/polynomial-sparse-term-list.scm")

((lambda ()

   ;;internal procedures
   
   (define (add-terms L1 L2)
     (cond ((empty-term-list? L1) L2)
	   ((empty-term-list? L2) L1)
	   (else
	    (let ((t1 (first-term L1)) (t2 (first-term L2)))
	      (cond ((> (order t1) (order t2))
		     (adjoin-term
		      t1 (add-terms (rest-terms L1) L2)))
		    ((< (order t1) (order t2))
		     (adjoin-term
		      t2 (add-terms L1 (rest-terms L2))))
		    (else
		     (adjoin-term
		      (make-term (order t1)
				 (apply-generic 'add (coeff t1) (coeff t2)))
		      (add-terms (rest-terms L1)
				 (rest-terms L2)))))))))

   (define (adjoin-term term term-list) (apply-generic 'adjoin-term term term-list))   

   (define (all-terms term-list) (apply-generic 'all-terms term-list))   

   (define (coeff term) (apply-generic 'coeff term))
   
   (define (coefficients term-list) (apply-generic 'coefficients term-list))
  
   (define (div-terms L1 L2)
     (if (empty-term-list? L1)
	 (list (the-empty-term-list) (the-empty-term-list))
	 (let ((t1 (first-term L1))
	       (t2 (first-term L2)))
	   (if (> (order t2) (order t1))
	       (list (the-empty-term-list) L1)
	       (let* ((res-term (make-term (- (order t1) (order t2))
					   (apply-generic 'div (coeff t1) (coeff t2))))
		      (res-x-divisor (mul-terms L2 (make-sparse-term-list
						    (list res-term))))
		      (rest-of-res (div-terms
				    (add-terms L1
					       (apply-generic 'negate res-x-divisor))
				    L2)))
		 (list (adjoin-term res-term (car rest-of-res))
		       (cadr rest-of-res)))))))
   
   (define (empty-term-list? term-list)
     (apply-generic 'empty-term-list? term-list))
   
   ;; returns a term list
   (define (find-integerizing-factor L1 L2)
     (let ((t1 (first-term L1))
	   (t2 (first-term L2)))
       (make-sparse-term-list
	(list
	 (make-term 0
		    (expt (coeff t2)
			  (1+ (- (order t1) (order t2)))))))))
   
   (define (first-term term-list) (apply-generic 'first-term term-list))

   (define (gcd-gen . args)
     (apply apply-generic (cons 'greatest-common-divisor args)))

   (define (gcd-terms L1 L2)
     (if (empty-term-list? L2)
	 L1
	 (let ((result (gcd-terms L2 (pseudoremainder-terms L1 L2))))
	   (divide-coefficients result (apply gcd-gen (coefficients result))))))
   
   (define (make-dense-term-list terms)
     ((get 'make '(dense-term-list)) terms))

   (define (make-sparse-term-list terms)
     ((get 'make '(sparse-term-list)) terms))

   (define make-term make-polynomial-term)

   ;; returns a term list
   (define (divide-coefficients term-list divisor)
       (map-terms (lambda (term)
		    (make-term (order term)
			       (div-gen (coeff term) divisor)))
		  term-list))
   
   (define (map-terms proc term-list)
     ((get 'map-terms (type-tag term-list)) proc term-list))
   
   (define (mul-terms L1 L2)
     (if (empty-term-list? L1)
	 (the-empty-term-list)
	 (add-terms (mul-term-by-all-terms (first-term L1) L2)
		    (mul-terms (rest-terms L1) L2))))

   (define (mul-term-by-all-terms t1 L)
     (if (empty-term-list? L)
	 (the-empty-term-list)
	 (let ((t2 (first-term L)))
	   (adjoin-term
	    (make-term (+ (order t1) (order t2))
		       (apply-generic 'mul (coeff t1) (coeff t2)))
	    (mul-term-by-all-terms t1 (rest-terms L))))))
   
   (define (order term) (apply-generic 'order term))

   (define (pseudoremainder-terms L1 L2)
     (cadr (div-terms (mul-terms L1
				 (find-integerizing-factor L1 L2))
		      L2)))

   (define (reduce-terms n d)
     (let* ((g (gcd-terms n d))
	    (int-factor (if (< (coeff (first-term n)) (coeff (first-term d)))
			    (find-integerizing-factor d g)
			    (find-integerizing-factor n g)))
	    (nn (car (div-terms (mul-terms n int-factor) g)))
	    (dd (car (div-terms (mul-terms d int-factor) g)))
	    (gg (apply gcd-gen (append (coefficients nn)
				       (coefficients dd)))))
       (list (divide-coefficients nn gg)
	     (divide-coefficients dd gg))))
   
   (define (rest-terms term-list) (apply-generic 'rest-terms term-list))
   
   (define (same-variable? v1 v2)
     (and (variable? v1) (variable? v2) (eq? v1 v2)))
   
   (define (tag p) (attach-tag 'polynomial p))

   (define (term-list p) (cadr p))

   (define (the-empty-term-list) ((get 'the-empty-term-list 'sparse-term-list)))
   
   (define (variable p) (car p))
   
   (define (variable? x) (symbol? x))


   ;; exposed procedures
   
   (define (add-poly p1 p2)
     (if (same-variable? (variable p1) (variable p2))
	 (make-poly (variable p1)
		    (add-terms (term-list p1)
			       (term-list p2)))
	 (error "Polys not in same var -- ADD-POLY"
		(list p1 p2))))

   (define (div-gen x y) (apply-generic 'div x y))
   
   (define (div-poly p1 p2)
     (if (same-variable? (variable p1) (variable p2))
	 (let ((var-symbol (variable p1))
	       (result (div-terms (term-list p1)
				  (term-list p2))))
	   (list (make-poly var-symbol (car result)) ;; quotient
		 (make-poly var-symbol (cadr result)))) ;; remainder
	 (error "Polys not in same var -- DIV-POLY"
		(list p1 p2))))

   (define (equ?-poly p1 p2)
     (and (same-variable? (variable p1) (variable p2))
	  (fold-left eq? #t (map equ?
				 (all-terms (term-list p1))
				 (all-terms (term-list p2))))))

   (define (gcd-poly p1 p2)
     (if (same-variable? (variable p1) (variable p2))
	 (make-poly (variable p1) (gcd-terms (term-list p1)
					     (term-list p2)))
	 (error "Polys not in same var -- GCD-POLY")))

   (define (make-poly variable term-list)
     (list variable
	   (cond ((or (eq? (type-tag term-list) 'sparse-term-list)
		      (eq? (type-tag term-list) 'dense-term-list))
		  term-list)
		 ((eq? 'polynomial-term (type-tag (car term-list)))
		  (make-sparse-term-list term-list))
		 (else (make-dense-term-list term-list)))))
   
   (define (mul-poly p1 p2)
     (if (same-variable? (variable p1) (variable p2))
	 (make-poly (variable p1)
		    (mul-terms (term-list p1)
			       (term-list p2)))
	 (error "Polys not in same var -- MUL-POLY"
		(list p1 p2))))

   (define (negate p)
     (make-poly (variable p)
		(apply-generic 'negate (term-list p))))

   (define (reduce-poly n d)
     (if (same-variable? (variable n) (variable d))
	 (map (bind-to-args make-poly (variable n))
	      (reduce-terms (term-list n) (term-list d)))
	 (error "Polys not in same var -- ADD-POLY"
		(list p1 p2))))
   
   (define (sub-poly p1 p2)
     (add-poly p1 (negate p2)))
   

   ;; interface to rest of the system

   (put 'add '(polynomial polynomial) (lambda (p1 p2) (tag (add-poly p1 p2))))

   (put 'equ? '(polynomial polynomial) equ?-poly)

   (put 'div '(polynomial polynomial)
	(lambda (p1 p2)
	  (let ((res (div-poly p1 p2)))
	    (list (tag (car res)) ;; quotient
		  (tag (cadr res)))))) ;; remainder

   (put 'greatest-common-divisor '(polynomial polynomial)
	(lambda (p1 p2) (tag (gcd-poly p1 p2))))

   (put 'make '(polynomial)
	(lambda (var terms) (tag (make-poly var terms))))

   (put 'mul '(polynomial polynomial) 
	(lambda (p1 p2) (tag (mul-poly p1 p2))))

   (put 'negate '(polynomial)
	(lambda (p) (tag (negate p))))

   (put 'project '(polynomial)
	(lambda (p) (coeff (first-term (term-list p)))))

   (put 'reduce '(polynomial polynomial)
	(lambda (p1 p2) (map tag (reduce-poly p1 p2))))

   (put 'sub '(polynomial polynomial) (lambda (p1 p2) (tag (sub-poly p1 p2))))

   (put '=zero? '(polynomial)
	(lambda (p) (empty-term-list? (term-list p))))
   

   'done))


(define (make-polynomial var terms)
  ((get 'make '(polynomial)) var terms))

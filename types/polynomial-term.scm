;; polynomial-term
;; Dependency: dependencies/data-directed-dispatch.scm

((lambda ()

   ;; internal procedures
   
   (define (tag term) (attach-tag 'polynomial-term term))


   ;; exposed procedures
   
   (define coeff cadr)

   (define (equ?-term t1 t2)
     (and (= (order t1) (order t2))
	  (apply-generic 'equ? (coeff t1) (coeff t2))))
   
   (define (make order coeff)
     (list order coeff))

   (define order car)


   ;; interface to the rest of the system
   
   (put 'coeff '(polynomial-term)
	(lambda (term) (coeff term)))

   (put 'equ? '(polynomial-term polynomial-term) equ?-term)
   
   (put 'make '(polynomial-term)
	(lambda (order coeff) (tag (make order coeff))))
   
   (put 'order '(polynomial-term)
	(lambda (term) (order term)))

   (put '=zero? '(polynomial-term)
	(lambda (term) (apply-generic '=zero? (coeff term))))

   'done))

(define (make-polynomial-term order coeff)
  ((get 'make '(polynomial-term)) order coeff))

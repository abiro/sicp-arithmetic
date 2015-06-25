;; rational
;; Dependency: dependencies/data-directed-dispatch.scm

((lambda ()
   
   ;; internal procedures

   (define (add-gen x y) (apply-generic 'add x y))

   (define (denom x) (cadr x))
   
   (define (div-gen x y) (apply-generic 'div x y))

   (define (gcd-gen x y) (apply-generic 'greatest-common-divisor x y))
   
   (define (mul-gen x y) (apply-generic 'mul x y))

   (define (negate-gen x) (apply-generic 'negate x))
   
   (define (numer x) (car x))
   
   (define (sub-gen x y) (apply-generic 'sub x y))

   (define (tag x) (attach-tag 'rational x))

   (define (=zero?-gen x) (apply-generic '=zero? x))
   

   ;; exposed  procedures

   (define (add-rat x y)
     (make-rat (add-gen (mul-gen (numer x) (denom y))
			(mul-gen (numer y) (denom x)))
	       (mul-gen (denom x) (denom y))))

   (define (div-rat x y)
     (make-rat (mul-gen (numer x) (denom y))
	       (mul-gen (denom x) (numer y))))
   
   (define (equ-rat? x y)
     (and (equ? (numer x)
		(numer x))
	  (equ? (denom x)
		(denom y))))

   ;; TODO extend it to allow only one arg to be polynomial
   (define (make-rat a b)
     (cond ((and (eq? 'polynomial (type-tag a)) (eq? 'polynomial (type-tag b)))
	    (apply-generic 'reduce a b))
	   ;; exact, denominator and numerator are implemented by mit-scheme
	   ((and (exact? a) (exact? b))
	    (let* ((n (* (numerator a) (denominator b)))
		   (d (* (denominator a) (numerator b)))
		   (g (gcd n d)))
	      (list (/ n g) (/ d g))))
	   (else
	    (error "Argument types are not supported -- MAKE-RAT" a b))))
    
   (define (mul-rat x y)
     (make-rat (mul-gen (numer x) (numer y))
	       (mul-gen (denom x) (denom y))))

   (define (negate-rat x)
     (make-rat (negate (numer x))
	       (denom x)))

   (define (sub-rat x y)
     (make-rat (sub-gen (mul-gen (numer x) (denom y))
			(mul-gen (numer y) (denom x)))
	       (mul-gen (denom x) (denom y))))  
   
   (define (=zero?-rat x)
     (=zero?-gen (numer x)))


   ;; interface to rest of the system

   (put 'add '(rational rational)
	(lambda (x y) (tag (add-rat x y))))

   (put 'div '(rational rational)
	(lambda (x y) (tag (div-rat x y))))

   (put 'equ? '(rational rational) equ-rat?)
   
   (put 'make '(rational)
	(lambda (n d) (tag (make-rat n d))))

   (put 'mul '(rational rational)
	(lambda (x y) (tag (mul-rat x y))))
   
   (put 'negate '(rational)
	(lambda (x) (tag (negate-rat x))))
   
   (put 'project '(rational)
	(lambda (x) ((get 'make '(scheme-number)) (/ (numer x) (denom x)))))
   
   (put 'raise '(rational)
	(lambda (x)
	  ((get 'make-from-real-imag '(complex)) (/ (numer x) (denom x)) 0)))
   
   (put 'sub '(rational rational)
	(lambda (x y) (tag (sub-rat x y))))

   (put '=zero? '(rational) =zero?-rat)
   

   'done))


(define (make-rational n d)
  ((get 'make '(rational)) n d))

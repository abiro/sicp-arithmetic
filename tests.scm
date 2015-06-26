(load "arithmetic-package.scm")

(define (run-tests tests)
  (every (lambda (test)
	   (let ((equal? (eval (car test) (nearest-repl/environment)))
		 (term1 (eval (cadr test) (nearest-repl/environment)))
		 (term2 (eval (caddr test) (nearest-repl/environment))))
	     (if (equal? term1 term2)
		 #t
		 ;; Prevent map from catching the error.
		 (standard-error-handler (error "\nThe following test failed:\n"
						test
						"Term 1:"
						term1
						"Term 2:"
						term2)))))
	 tests))


(define mpt make-polynomial-term)

(define n1 (make-scheme-number -1))
(define n2 (make-scheme-number 2))

(define p1-term-list ((get 'make '(sparse-term-list)) (list (mpt 1 2) (mpt 0 1))))
(define p1 (make-polynomial 'x p1-term-list))
(define p2 (make-polynomial 'x (list (mpt 0 -1))))
(define p3 (make-polynomial 'x (list 2 1))) ;; dense term list
(define p4 (make-polynomial 'x (list (mpt 5 1) (mpt 0 -1))))
(define p5 (make-polynomial 'x (list (mpt 2 1) (mpt 0 -1))))

(define r1 (make-rational 1 3))
(define r2 (make-rational 3 8))
(define r3 (make-rational 2 1))
(define r4 (make-rational 5 -6))

(define rf1 (make-rational p1 p2))

(define z1 (make-complex-from-real-imag 3 4))
(define z2 (make-complex-from-mag-ang 5 1))
(define z3 (make-complex-from-real-imag r1 1))
(define z4 (make-complex-from-mag-ang 1 r1))


(define arithmetic-tests
  (list '(equ? (make-complex-from-real-imag 4 4) (add z1 1))
	'(equ? (make-complex-from-real-imag 4 4) (add 1 z1))
	'(equ? (make-polynomial 'x (list 2 0)) (add p1 p2))
	'(equ? 5 (add p2 6))
	'(equ? (make-rational -6 5)  (div 1 r4))
	'(equ? 1 (car (div p1 p3)))
	'(equ? (make-rational p2 p1) (div 1 rf1))
	'(equ? (make-polynomial 'x (list (mpt 2 -6) (mpt 0 2)))
	       (mul 2 (make-polynomial 'x (list -3 0 1))))
	'(equ? (mul rf1 -1) (negate rf1))
	'(equ? (mul z2 r3) (add z2 z2))
	'(equ? (make-complex-from-real-imag 0 1)
	       (sub z3 r1))
	'(eq? #t (=zero? (sub -1 p2)))))

(define complex-tests
  (list '(= 1 (angle z2))
	'(= 4 (imag-part z1))
	'(= 5 (magnitude z1))
	'(= 5 (magnitude z2))
	'(= 3 (real-part z1))
	'(equ? (make-complex-from-real-imag 6 8)
	       (add z1 z1))
	'(equ? (make-complex-from-real-imag 11/3 6)
	       (add (add z3 z3) z1))
	'(equ? (make-complex-from-mag-ang 1 0)
	       (div z2 z2))
	'(equ? (make-complex-from-mag-ang 25 2)
	       (mul z2 z2))
	'(equ? (make-complex-from-mag-ang 1 2/3)
	       (mul z4 z4))
	'(equ? (make-complex-from-real-imag 0 0)
	       (sub z2 z2))
	'(eq? #t (=zero? (make-complex-from-mag-ang 0 0)))))

(define ddr-tests
  (list '(eq? 'scheme-number (type-tag n1))
	'(= -1 (contents n1))

	'(eq? 'rational (type-tag r1))
	
	'(= 1 (car (contents r1)))
	'(= 3 (cadr (contents r1)))
	'(equ? (make-rational 1/3 2/1) 1/6)

	'(eq? 'complex (type-tag z1))
	'(eq? 'rectangular (type-tag (contents z1)))
	'(= 3 (cadr (contents z1)))
	'(= 4 (cddr (contents z1)))

	'(eq? #f (all-same-type? (list n1 n2 r1 r2 z1 z2)))
	'(eq? #t (all-same-type? (list r1 r1 r1 r1)))
	'(eq? #t (all-same-type? (list n1 n2)))
	'(eq? #t (all-same-type? (raise-args (list n1 z1 n2 r1 r2 z2 r3))))
	'(eq? 'complex (type-tag (car (raise-args (list n1 z1 n2 r1 r2 z2 r3)))))

	'(equ? r3 (raise n2))
	'(equ? (make-complex-from-real-imag 1.5 0) (raise 1.5))	
	'(equ? (make-complex-from-real-imag 2 0) (raise r3))

	'(equ? (drop r3) n2)
	'(equ? (drop (make-complex-from-real-imag 2 0)) 2)))

(define polynomial-tests
  (list '(equ? (make-polynomial 'x (list (mpt 1 2)))
	       (add p1 p2))
	'(equ? (make-polynomial 'x (list (mpt 1 2)))
	       (add p3 p2))
	'(eq? #t (equ? p2 p2))
	'(eq? #t (equ? p3 p3))
	'(eq? #t (equ? p1 p3))
	'(eq? #f (equ? p1 p2))
	'(eq? #t (equ? p4 (make-polynomial 'x (list 1 0 0 0 0 -1))))
	'(equ? (make-polynomial 'x (list (mpt 2 -1) (mpt 1 1)))
	       (greatest-common-divisor
		(make-polynomial 'x (list (mpt 4 1) (mpt 3 -1) (mpt 2 -2) (mpt 1 2)))
		(make-polynomial 'x (list (mpt 3 1) (mpt 1 -1)))))
	'(equ? p3 (greatest-common-divisor p3 p3))
	'(equ? (make-polynomial 'x (list (mpt 2 4) (mpt 1 4) (mpt 0 1)))
	       (mul p1 p1))
	'(equ? (make-polynomial 'x (list (mpt 2 4) (mpt 1 4) (mpt 0 1)))
	       (mul p1 p3))
	'(equ? (make-polynomial 'x (list (mpt 3 1) (mpt 1 1)))
	       (car (div p4 p5)))
	'(equ? (make-polynomial 'x (list (mpt 1 1) (mpt 0 -1)))
	       (cadr (div p4 p5)))
	'(equ? (make-rational (make-polynomial 'x (list (mpt 1 -4) (mpt 0 -2)))
			      (make-polynomial 'x (list (mpt 0 1))))
	       (add rf1 rf1))
	'(equ? (make-rational (make-polynomial 'x (list (mpt 2 4) (mpt 1 4) (mpt 0 1)))
			      (make-polynomial 'x (list (mpt 0 1))))
	       (mul rf1 rf1))
	'(eq? #t (=zero? (sub p1 p1)))
	'(eq? #t (=zero? (sub p3 p3)))
	'(eq? #t (=zero? (sub p1 p3)))))

(define rational-tests
  (list '(equ? (make-rational 9 27) (make-rational 1 3))
	'(equ? (make-rational p1 (make-polynomial 'x (list 1)))
	       (make-rational (mul p1 p3) p3))
	'(equ? (make-rational 17 24)
	       (add r1 r2))
	'(equ? (make-rational 8 9)
	       (div r1 r2))
	'(equ? (make-rational 3 24)
	       (mul r1 r2))
	'(equ? (make-rational -1 24)
	       (sub r1 r2))
	'(eq? #t (=zero? (make-rational 0 1)))))

(define scheme-number-tests
  (list '(= 5 (add 2 3))
	'(= 1 (add n1 n2))
	'(= 1 (div -1 -1))
	'(= -2 (div n2 n1))
	'(= 10 (mul 2 5))
	'(= -2 (mul n1 n2))
	'(= 0 (sub -1 -1))
	'(= -3 (sub n1 n2))
	'(eq? #f (=zero? 1))
	'(eq? #f (=zero? n1))))

(run-tests arithmetic-tests)
(run-tests complex-tests)
(run-tests ddr-tests)
(run-tests polynomial-tests)
(run-tests rational-tests)
(run-tests scheme-number-tests)

(newline)
(display "+++ tests succesfully completed +++")

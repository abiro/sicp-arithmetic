;; complex
;; Dependencies: dependencies/data-directed-dispatch.scm,
;; tpyes/complex-polar-package.scm, types/complex-rectangular-package.scm

(load "types/complex-polar-package.scm")
(load "types/complex-rectangular-package.scm")

((lambda ()
  
  ;; internal procedures
  
  (define (make-from-mag-ang r a)
    ((get 'make-from-mag-ang '(polar)) r a))

  (define (make-from-real-imag x y)
    ((get 'make-from-real-imag '(rectangular)) x y))

  (define (tag z) (attach-tag 'complex z))  

  
  ;; exposed procedures
  
  (define (add-complex z1 z2)
    (make-from-real-imag (+ (real-part z1) (real-part z2))
                         (+ (imag-part z1) (imag-part z2))))

  (define (equ?-complex z1 z2)
    (and (= (real-part z1)
	    (real-part z2))
	 (= (imag-part z1)
	    (imag-part z2))))
  
  (define (div-complex z1 z2)
    (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
                       (- (angle z1) (angle z2))))

  (define (imag-part z)
    (apply-generic 'imag-part z))
  
  (define (mul-complex z1 z2)
    (make-from-mag-ang (* (magnitude z1) (magnitude z2))
                       (+ (angle z1) (angle z2))))

  (define (negate-complex z)
    (make-from-real-imag (apply-generic 'negate (real-part z))
			 (apply-generic 'negate (imag-part z))))
  
  (define (real-part z)
    (apply-generic 'real-part z))
  
  (define (sub-complex z1 z2)
      (make-from-real-imag (- (real-part z1) (real-part z2))
                           (- (imag-part z1) (imag-part z2))))

  

  ;; interface to rest of the system

  (put 'add '(complex complex)
       (lambda (z1 z2) (tag (add-complex z1 z2))))

  (put 'angle '(complex)
       (lambda (z) (apply-generic 'angle z)))

  (put 'div '(complex complex)
       (lambda (z1 z2) (tag (div-complex z1 z2))))

  (put 'equ? '(complex complex) equ?-complex)
  
  (put 'imag-part '(complex) imag-part)
       
  (put 'magnitude '(complex)
       (lambda (z) (apply-generic 'magnitude z)))

  (put 'make-from-mag-ang '(complex)
       (lambda (r a) (tag (make-from-mag-ang r a))))

  (put 'make-from-real-imag '(complex)
       (lambda (x y) (tag (make-from-real-imag x y))))

  (put 'mul '(complex complex)
       (lambda (z1 z2) (tag (mul-complex z1 z2))))

  (put 'negate '(complex)
       (lambda (z) (tag (negate-complex z))))

  (put 'project '(complex)
       (lambda (z) ((get 'make '(scheme-number)) (real-part z))))

  ;; TODO what variable?
  (put 'raise '(complex)
       (lambda (z)
	 ((get 'make '(polynomial))
	  'x
	  (list (make-polynomial-term 0
				      (tag (make-from-real-imag (real-part z)
								(imag-part z))))))))
  
  (put 'real-part '(complex) real-part)

  (put 'sub '(complex complex)
       (lambda (z1 z2) (tag (sub-complex z1 z2))))

  (put '=zero? '(complex)
       (lambda (z) (= (real-part z) (imag-part z) 0)))
  
  
  'done))


;; Generic selectors

(define (angle z) (apply-generic 'angle z))

(define (imag-part z) (apply-generic 'imag-part z))

(define (magnitude z) (apply-generic 'magnitude z))

(define (real-part z) (apply-generic 'real-part z))


;; Constructors for complex numbers

(define (make-complex-from-real-imag x y)
  ((get 'make-from-real-imag '(complex)) x y))

(define (make-complex-from-mag-ang r a)
  ((get 'make-from-mag-ang '(complex)) r a))

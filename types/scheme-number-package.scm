;; scheme-number
;; Dependency: dependencies/data-directed-dispatch.scm

((lambda ()

  ;; internal procedures
   
  (define (tag x)
    (attach-tag 'scheme-number x))


  ;; exposed procedures


  ;; interface to the rest of the system
  
  (put 'add '(scheme-number scheme-number)
       (lambda (x y) (tag (+ x y))))

  (put 'div '(scheme-number scheme-number)
       (lambda (x y) (tag (/ x y))))
  
  (put 'equ? '(scheme-number scheme-number)
       (lambda (x y) (= (contents x) (contents y))))

  (put 'greatest-common-divisor '(scheme-number ...)
       (lambda args
	 (if (= 0 (length (filter inexact? args)))
	     (apply gcd args)
	     (error "Arguments must be exact. -- GREATEST-COMMON-DIVISOR" args))))
  
  (put 'sub '(scheme-number scheme-number)
       (lambda (x y) (tag (- x y))))

  (put 'mul '(scheme-number scheme-number)
       (lambda (x y) (tag (* x y))))

  (put 'make '(scheme-number)
       (lambda (x) (tag x)))

  (put 'negate '(scheme-number)
       (lambda (x) (tag (* -1 x))))
  
  (put 'raise '(scheme-number)
       (lambda (x)
	 (if (exact? x)
	     ((get 'make '(rational)) x 1)
	     ((get 'make-from-real-imag '(complex)) x 0))))
  
  (put '=zero? '(scheme-number)
       (lambda (x) (zero? (contents x))))
  

  'done))

(define (make-scheme-number n)
  ((get 'make '(scheme-number)) n))

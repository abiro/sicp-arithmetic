(load "dependencies/data-directed-dispatch.scm")
(load "dependencies/util.scm")

(load "types/complex-package.scm")
(load "types/polynomial-package.scm")
(load "types/rational-package.scm")
(load "types/scheme-number-package.scm")


;; Set hierarchy of types.
(put 'level '(polynomial) (lambda args 3))
(put 'level '(complex) (lambda args 2))
(put 'level '(rational) (lambda args 1))
(put 'level '(scheme-number) (lambda args 0))


(define (add x y) (apply-generic 'add x y))

;; If an argument is a polynomial, the result is a list, where the first element
;; is the quotient and the second is the remainder.
(define (div x y) (apply-generic 'div x y))

;; GCD is only imeplemented for the exact 'scheme-number' and 'polynomial' types.
;; When one of the arguments is a polynomial, it only accepts two arguments.
(define (greatest-common-divisor . args)
  (apply apply-generic (cons 'greatest-common-divisor args)))

(define (mul x y) (apply-generic 'mul x y))

(define (negate x) (apply-generic 'negate x))

(define (sub x y) (apply-generic 'sub x y))

(define (=zero? x) (apply-generic '=zero? x))

(define (bind-to-args proc . args)
  (lambda more-args (apply proc
			   (append args
				   more-args))))

(define (identity x) x)

;; debugging aid
(define (print . args)
  (begin (for-each (lambda (x)
		     (newline)
		     (display x))
		   args)
	 (newline)
	 (display ".")
	 (car args)))

(define (truthy? x) (if x #t #f))


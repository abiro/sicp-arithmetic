;; polar
;; Dependency: dependencies/data-directed-dispatch.scm

((lambda ()

  ;; internal procedures
   
  (define (tag x) (attach-tag 'polar x))

  
  ;; exposed procedures
  
  (define (angle z) (drop (cdr z)))

  (define (imag-part z)
    (* (magnitude z) (sin (angle z))))
  
  (define (magnitude z) (drop (car z)))
  
  (define (make-from-mag-ang r a) (cons r a))
  
  (define (make-from-real-imag x y) 
    (cons (sqrt (+ (square x) (square y)))
          (atan y x)))
  
  (define (real-part z)
    (* (magnitude z) (cos (angle z))))


  
  ;; interface to the rest of the system
  
  (put 'angle '(polar) angle)
  
  (put 'imag-part '(polar) imag-part)
  
  (put 'magnitude '(polar) magnitude)
  
  (put 'make-from-mag-ang '(polar)
       (lambda (r a) (tag (make-from-mag-ang r a))))

  (put 'make-from-real-imag '(polar)
         (lambda (x y) (tag (make-from-real-imag x y))))  

  (put 'real-part '(polar) real-part)


  'done))

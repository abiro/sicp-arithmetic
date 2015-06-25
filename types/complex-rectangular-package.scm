;; rectangular
;; Dependency: dependencies/data-directed-dispatch.scm

((lambda ()

  ;; internal procedures

  (define (tag x) (attach-tag 'rectangular x))


  ;; exposed procedures

  (define (angle z)
    (atan (imag-part z) (real-part z)))

  (define (imag-part z) (drop (cdr z)))

  (define (magnitude z)
    (sqrt (+ (square (real-part z))
             (square (imag-part z)))))

  (define (make-from-mag-ang r a) 
    (cons (* r (cos a)) (* r (sin a))))
  
  (define (make-from-real-imag x y) (cons x y))
  
  (define (real-part z) (drop (car z)))

  
  ;; interface to the rest of the system
  
  (put 'angle '(rectangular) angle)  
  
  (put 'imag-part '(rectangular) imag-part)
  
  (put 'magnitude '(rectangular) magnitude)

  (put 'make-from-mag-ang '(rectangular)
       (lambda (r a) (tag (make-from-mag-ang r a))))

  (put 'make-from-real-imag '(rectangular)
       (lambda (x y) (tag (make-from-real-imag x y))))
  
  (put 'real-part '(rectangular) real-part)


  'done))

;; Following the data-directed programming approach, these procedures implement a modular
;; system that allows exposing generic procedures that work on various types.
;; The appropriate procedures are dispatched based on the types of the arguments.
;; Coercions are implemented with the tower-of-types approach.
;;
;; A type must provide the following procedures to work with the data-directed dispatch
;; system:
;; (equ? x y),
;; (project x), unless lowest level in type tower
;; (raise x), unless highest level in type tower


(define (all-same-type? data)
  (if (< (length data) 2)
      #t
      (let ((first-type (type-tag (car data))))
	(every (lambda (datum)
		 (eq? first-type (type-tag datum)))
	       (cdr data)))))

(define (apply-generic op . args)
  (let* ((type-tags (map type-tag args))
	 (proc (get op type-tags)))
    (if proc
	(apply proc (map contents args))
	(let ((raised-args (raise-args args)))
	  (if raised-args
	      (apply apply-generic (cons op raised-args))
	      (error "No method for these types. -- APPLY-GENERIC"
		     (list op type-tags)))))))

(define (attach-tag type-tag contents)
  (if (number? contents)
      contents
      (cons type-tag contents)))

(define (contents datum)
  (cond ((number? datum)
	 datum)
	((pair? datum)
	 (cdr datum))
	(else
	 (error "Bad tagged datum -- CONTENTS" datum))))

(define (drop x)
  (let ((project (get 'project (list (type-tag x)))))
    (if project
	(let ((projected-x (project (contents x))))
	  (if (equ? x projected-x)
	      (drop projected-x)
	      x))
	x)))

(define (equ? x y) (apply-generic 'equ? x y))

(define (get-level x) (apply-generic 'level x))

(define (make-table)
  (define (assoc key records)
    (cond ((null? records) false)
	  ((equal? key (caar records)) (car records))
	  (else (assoc key (cdr records)))))

  ;; A key for a procedure with arbitrary number of arguments can be specified as
  ;; follows: (type-tag ...)  
  (let ((local-table (list '*table*)))
    (define (lookup key-1 key-2)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record (or (assoc key-2 (cdr subtable))
			      (if (list? key-2) (assoc (list (car key-2) '...)
						       (cdr subtable))))))
              (if record
                  (cdr record)
                  false))
            false)))

    (define (insert! key-1 key-2 value)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
              (if record
                  (set-cdr! record value)
                  (set-cdr! subtable
                            (cons (cons key-2 value)
                                  (cdr subtable)))))
            (set-cdr! local-table
                      (cons (list key-1
                                  (cons key-2 value))
                            (cdr local-table)))))
      'ok)

    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert!-proc) insert!)
            (else (error "Unknown operation -- MAKE-TABLE" m))))

    dispatch))

(define (raise x) (apply-generic 'raise x))

(define (raise-args args)
  (let ((raised-args
	(if (all-same-type? args)
	    (map raise-if-possible args)
	    (let ((sorted-args (sort args
				     (lambda (x y) (> (get-level x) (get-level y))))))
	      (map (bind-to-args raise-to
				 (type-tag (car sorted-args)))
		   args)))))
    (if (every truthy? raised-args)
	raised-args
	#f)))

(define (raise-if-possible x)
  (let ((raise (get 'raise (list (type-tag x)))))
    (and raise (raise (contents x)))))

(define (raise-to type x)
  (if (or (not x) (eq? type (type-tag x)))
      x
      (raise-to type (raise-if-possible x))))

(define (type-tag datum)
  (cond ((number? datum)
	 'scheme-number)
	((pair? datum)
	 (car datum))
	(else
	 (error "Bad tagged datum -- TYPE-TAG" datum))))


(define operation-table (make-table))

(define get (operation-table 'lookup-proc))

(define put (operation-table 'insert!-proc))

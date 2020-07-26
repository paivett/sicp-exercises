
(define (raise-integer x) (make-rational (contents x) 1))
(define (raise-rational q) (make-real (/ (numer q) (denom q)))) ; Suppose that number and denom are exposed as public
(define (raise-real x) (make-complex-from-real-imag (contents x) 0))


; Install the generic raise function for every type except the complex
(put 'raise '(integer) raise-integer)
(put 'raise '(rational) raise-rational)
(put 'raise '(real) raise-real)


(define (project-complex x) (make-real (real x)))
(define (project-real x) (make-rat (numerator  x) (denominator x)))
(define (project-rational q) (make-integer (numer q)))


; Install the generic raise function for every type except the complex
(put 'project '(complex) project-complex)
(put 'project '(real) project-real)
(put 'project '(rational) project-rational)


(define (raise obj)
  (let ((type (type-tag obj)))
    (let ((raise-type-func (get 'raise type)))
         (if raise-type-func
             (raise-type-func obj)
             (error "Cannot raise type" type)))))

; This is the tower from the highest to lowest
(define TOWER '(complex real rational integer))


(define (find-first e1 e2 elements)
  (if (null? elements)
      (error "List must contain at least one of the elements")
      (let ((front (car elements)))
           (cond ((eq? front e1) e1)
                 ((eq? front e2) e2)
                 (else (find-first e1 e2 (cdr elements)))))))


; t1 is higher that t2 if it is the first appearing in the TOWER hierarchy
(define (type> t1 t2) (eq? t1 (find-first t1 t2 TOWER)))


(define (highest-type types)
  (reduce (lambda (t1 t2) (if (type> t1 t2) t1 t2)) (car types) types))


(define (raise-to-target target-type obj)
  (if (eq? target-type (type-tag obj))
      obj
      (raise-to-target target-type (raise obj))))


(define (drop obj)
  (let ((project (get 'project (type-tag obj))))
       (if (equ? obj (raise (project obj)))
           (drop (project obj))
           obj)))


(define (no-method-error op types)
  (error "No method for these types" (list op types)))


(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (drop (apply proc (map contents args)))
          (if (> (length args) 1)
              (let ((target-type (get-highest-type type-args)))
                   (apply-generic op (map (lambda (arg) (raise-to-target target-type arg)) args)))
              (no-method-error op type-args))))))
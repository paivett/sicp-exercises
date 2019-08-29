(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (not-operator? s)
  (or (number? s) (variable? s)))

;------------------------------------------------

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        ((is-operation? a2) (list '+ a1 a2))
        ((pair? a2)
          (if (> (length a2) 1)
              (list '+ a1 (make-sum (car a2) (cdr a2)))
              (list '+ a1 (car a2))))
        (else (list '+ a1 a2))))

(define (addend s) (cadr s))

(define (augend s) 
  (if (> (length (cddr s)) 1)
      (make-sum (caddr s) (cdddr s))
      (caddr s)))

(define (sum? x)
  (and (pair? x) (eq? (car x) '+)))

;------------------------------------------------

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        ((is-operation? a2) (list '* a1 a2))
        ((pair? a2)
          (if (> (length a2) 1)
              (list '* a1 (make-product (car a2) (cdr a2)))
              (list '* a1 (car a2))))
        (else (list '* m1 m2))))

(define (multiplier p) (cadr p))

(define (multiplicand p) 
  (if (> (length (cddr p)) 1)
      (make-product (caddr p) (cdddr p))
      (caddr p)))

(define (product? x)
  (and (pair? x) (eq? (car x) '*)))

;------------------------------------------------

(define (make-exponentiation base exponent)
    (cond ((=number? exponent 0) 1)
          ((=number? exponent 1) base)  
          (else (list '** base exponent))))

(define (base expr) (cadr expr))

(define (exponent expr) (caddr expr))

(define (exponentiation? expr) 
    (and (pair? expr) (eq? (car expr) '**)))

;------------------------------------------------

(define (is-operation? e)
  (or (exponentiation? e) (sum? e) (product? e)))

;------------------------------------------------

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
           (make-product (multiplier exp)
                         (deriv (multiplicand exp) var))
           (make-product (deriv (multiplier exp) var)
                         (multiplicand exp))))
        ((exponentiation? exp)
         (make-product
            (exponent exp)
            (make-product (make-exponentiation (base exp) (- (exponent exp) 1))
                          (deriv (base exp) var))))
        (else
         (error "unknown expression type -- DERIV" exp))))

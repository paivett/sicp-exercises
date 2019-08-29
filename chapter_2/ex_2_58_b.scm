(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (prefix exp e)
  (define (iter-prefix acc exp)
    (if (eq? (car exp) e)
        acc
        (iter-prefix (append acc (list (car exp))) (cdr exp))))
  (iter-prefix '() exp))

;------------------------------------------------

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list a1 '+ a2))))

(define (addend s)
  (let ((a (prefix s '+)))
       (if (< (length a) 2) (car a) a)))

(define (augend s)
  (let ((a (cdr (memq '+ s))))
       (if (< (length a) 2) (car a) a)))

(define (sum? x)
  (and (pair? x) (pair? (memq '+ x))))

;------------------------------------------------

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list '* m1 m2))))

(define (multiplier p) (car p))

(define (multiplicand p) 
  (let ((m (cdr (memq '* p))))
       (if (< (length m) 2) (car m) m)))


(define (product? x)
  (and (not (sum? x)) (pair? (memq '* x))))

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
        (else
         (error "unknown expression type -- DERIV" exp))))

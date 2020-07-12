; (define (deriv exp var)
;   (cond ((number? exp) 0)
;         ((variable? exp) (if (same-variable? exp var) 1 0))
;         ((sum? exp)
;          (make-sum (deriv (addend exp) var)
;                    (deriv (augend exp) var)))
;         ((product? exp)
;          (make-sum
;            (make-product (multiplier exp)
;                          (deriv (multiplicand exp) var))
;            (make-product (deriv (multiplier exp) var)
;                          (multiplicand exp))))
;         <more rules can be added here>
;         (else (error "unknown expression type -- DERIV" exp))))

; Using dispatching on type on the above exercise, the derive operation would be written as

(define (deriv exp var)
   (cond ((number? exp) 0)
         ((variable? exp) (if (same-variable? exp var) 1 0))
         (else ((get 'deriv (operator exp)) (operands exp)
                                            var))))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))

; A) 
; What was done is to assign a type to each compound algebraic expression, and register, for each type, a deriv operation
; that knows how to implement that derivation of that particular algebraic expression. This way, new algebraic expressions
; can be added without having to touch the deriv implementation.
; We cannot assimilate number? and same-variable? into the data-directed dispatch because they operate on primitive types
; and we cannot type annotate numbers or symbols.

; B)
(define (install-sum-package)
  ;; internal procedures
  (define (make-sum a b) (cons a b))
  (define (addend exp) (car exp))
  (define (augend exp) (cdr exp))
  (define (deriv-inner exp) 
    ; we rename deriv to something else, because in the implementation we need to derive not another sum, but a generic expression
    (make-sum (deriv (addend exp)) (deriv (augend exp))))
  
  ;; interface to the rest of the system
  (define (tag x) (attach-tag '+ x))
  (put 'deriv '(+) 
        (lambda (exp) (tag (deriv-inner exp))))
  (put 'addend '(+) addend)
  (put 'augend '(+) augend)
  (put 'make-sum '+
       (lambda (x y) (tag (make-sum x y))))
  'done)

; Then we should install the package and define the selectors and constructor

(define (addend exp) (apply-generic 'addend exp))
(define (augend exp) (apply-generic 'augent exp))
(define (make-sum a b) ((get 'make-sum '+ ) a b))


(define (install-prod-package)
  ;; internal procedures
  (define (make-product-inner a b) (cons a b))
  (define (multiplicand exp) (car exp))
  (define (multiplier exp) (cdr exp))
  (define (deriv-inner exp)
    (make-sum
           (make-product (multiplier s)
                         (deriv (multiplicand exp) var))
           (make-product (deriv (multiplier exp) var)
                         (multiplicand exp))))
  
  ;; interface to the rest of the system
  (define (tag x) (attach-tag '* x))
  (put 'deriv '(*) deriv-inner)
  (put 'multiplicand '(*) multiplicand)
  (put 'multiplier '(*) multiplier)
  (put 'make-product '*
       (lambda (x y) (tag (make-product-inner x y))))
  'done)

(define (multiplier exp) (apply-generic 'multiplier exp))
(define (multiplicand exp) (apply-generic 'multiplicand exp))
(define (make-product a b) ((get 'make-product '* ) a b))
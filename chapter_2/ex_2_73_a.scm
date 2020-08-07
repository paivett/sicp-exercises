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

; (define (deriv exp var)
;    (cond ((number? exp) 0)
;          ((variable? exp) (if (same-variable? exp var) 1 0))
;          (else ((get 'deriv (operator exp)) (operands exp)
;                                             var))))
; (define (operator exp) (car exp))
; (define (operands exp) (cdr exp))

; A) 
; What was done is to assign a type to each compound algebraic expression, and register, for each type, a deriv operation
; that knows how to implement that derivation of that particular algebraic expression. This way, new algebraic expressions
; can be added without having to touch the deriv implementation.
; We cannot assimilate number? and same-variable? into the data-directed dispatch because they operate on primitive types
; and we cannot type annotate numbers or symbols.

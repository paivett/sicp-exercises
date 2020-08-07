; This was taken from section 3.3.3 of the book as it is, only so that we can implement the
; exercise and make it work
(define (make-table)
  (let ((local-table (list '*table*)))
    (define (lookup key-1 key-2)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
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
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation -- TABLE" m))))
    dispatch))

(define operation-table (make-table))
(define get (operation-table 'lookup-proc))
(define put (operation-table 'insert-proc!))

;=======================================================================

; Procedures defined in section 2.3.2 to work with algebraic expressions

(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

;=======================================================================

; Procedures to work with tagged data, taken from section 2.4.2

(define (attach-tag type-tag contents)
  (cons type-tag contents))

(define (type-tag datum)
  (if (pair? datum)
      (car datum)
      (error "Bad tagged datum -- TYPE-TAG" datum)))

(define (contents datum)
  (if (pair? datum)
      (cdr datum)
      (error "Bad tagged datum -- CONTENTS" datum)))

;=======================================================================

; Procedure taken from section 2.4.3 to work with data-directed programming

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (error
            "No method for these types -- APPLY-GENERIC"
            (list op type-tags))))))

;=======================================================================

; This is the proposed deriv method that performs derivation of expressions
; using the data-directed strategy

(define (deriv exp var)
   (cond ((number? exp) 0)
         ((variable? exp) (if (same-variable? exp var) 1 0))
         (else ((get 'deriv (operator exp)) (operands exp)
                                            var))))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))

;=======================================================================

; Implementation of sum and prod expressions as packages

(define (install-sum-package)
  ;; internal procedures
  (define (make-sum-inner a b) (cons a b))
  (define (addend-inner exp) (car exp))
  (define (augend-inner exp) (cdr exp))
  (define (deriv-inner exp var) 
    ; we rename deriv to something else, because in the implementation we need to derive not another sum, but a generic expression
    (make-sum (deriv (addend-inner exp) var) (deriv (augend-inner exp) var)))

  ;; interface to the rest of the system
  (define (tag x) (attach-tag '+ x))
  (put 'deriv '+ deriv-inner)
  (put 'addend '(+) addend-inner)
  (put 'augend '(+) augend-inner)
  (put 'make-sum '+
       (lambda (x y) (tag (make-sum-inner x y))))
  'done)

; Then we should install the package and define the selectors and constructor
(install-sum-package)
(define (addend exp) (apply-generic 'addend exp))
(define (augend exp) (apply-generic 'augend exp))
(define (make-sum a b) ((get 'make-sum '+ ) a b))

(define s1 (make-sum 1 2))
(define s2 (make-sum 2 'x))

(define (install-prod-package)
  ;; internal procedures
  (define (make-product-inner a b) (cons a b))
  (define (multiplicand-inner exp) (car exp))
  (define (multiplier-inner exp) (cdr exp))
  (define (deriv-inner exp var)
    (make-sum
           (make-product (multiplier-inner exp)
                         (deriv (multiplicand-inner exp) var))
           (make-product (deriv (multiplier-inner exp) var)
                         (multiplicand-inner exp))))
  
  ;; interface to the rest of the system
  (define (tag x) (attach-tag '* x))
  (put 'deriv '* deriv-inner)
  (put 'multiplicand '(*) multiplicand-inner)
  (put 'multiplier '(*) multiplier-inner)
  (put 'make-product '*
       (lambda (x y) (tag (make-product-inner x y))))
  'done)

(install-prod-package)
(define (multiplier exp) (apply-generic 'multiplier exp))
(define (multiplicand exp) (apply-generic 'multiplicand exp))
(define (make-product a b) ((get 'make-product '* ) a b))


(define p1 (make-product 1 2))
(define p2 (make-product 1 'x ))


(define (install-exp-package)
  ;; internal procedures
  (define (make-exp-inner b e) (cons b e))
  (define (base exp) (car exp))
  (define (exponent exp) (cdr exp))
  (define (deriv-inner exp var)
    (make-product
           (exponent exp)
           (make-product (make-exp (base exp) (- (exponent exp) 1))
                         (deriv (base exp) var))))

  ;; interface to the rest of the system
  (define (tag x) (attach-tag '** x))
  (put 'deriv '** deriv-inner)
  (put 'base '(**) base)
  (put 'exponent '(**) exponent)
  (put 'make-exponentiation '**
       (lambda (b e)
        (cond ((= e 0) 1)
              ((= e 1) b)  
              (else (tag (make-exp-inner b e))))))
  'done)

(install-exp-package)
(define (base exp) (apply-generic 'base exp))
(define (exponent exp) (apply-generic 'exponent exp))
(define (make-exp b e) ((get 'make-exponentiation '** ) b e))

(define e1 (make-exp 1 2))
(define e2 (make-exp 'x 2))
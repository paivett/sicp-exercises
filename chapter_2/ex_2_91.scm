#lang sicp

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

(define (put-coercion t1 t2 f) (put 'coercion (list t1 t2) f))

(define (get-coercion t1 t2) (get 'coercion (list t1 t2)))

; The proposted new apply-generic with coercion

; Given a list of symbols, returns if all are equal using eq?
(define (all-eq? symbols)
  (if (> (length symbols) 1)
      (and (eq? (car symbols) (cadr symbols)) (all-eq? (cdr symbols)))
      true))

; Given a list of types (t1, ..., tN), returns a list of (f1, ..., fN) where fi is a function that coerces ti
; into target-type, or false if such function is not available
(define (get-coercions-to-type types target-type)
  (map (lambda (type) (get-coercion type target-type)) types))

; Given a list of elements, returns true if any element satisfies the condition
(define (any? elements condition)
  (if (null? elements)
      false
      (if (condition (car elements))
          true
          (any? (cdr elements) condition))))

(define (any-false? elements) (any? elements (lambda (e) (eq? e false))))

; Given a list of types (t1, ..., tN), if there is a type tj to which all types in the list can be coerced to, then
; returns a list if functions (f1, ..., fN) where each function coerces type ti to tj.
; If it is not possible to find N such functions for any target type, the functions returns false.
(define (get-coercion-funcs types) 
  (define (get-coercion-funcs-iter candidate-types)
    (if (null? candidate-types)
        false ; There are no more types to try
        (let ((coercions (get-coercions-to-type types (car candidate-types))))
             (if (any-false? coercions)
                 (get-coercion-funcs-iter (cdr candidate-types))
                 coercions))))

  (get-coercion-funcs-iter types))


; Given a list of functions (f1, ..., fn), and a list of elements (e1, ..., en), returns the list ((f1 e1), ..., (fn en))
; Both lists should be of the same length
(define (map-funcs funcs elements)
  (if (null? funcs)
      elements
      (cons ((car funcs) (car elements)) (map-funcs (cdr funcs) (cdr elements)))))


(define (no-method-error op types)
  (error "No method for these types" (list op types)))


(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (if (> (length args) 1)
              (let ((coercion-funcs (get-coercion-funcs type-tags)))
                   (cond ((all-eq? type-tags)
                          (error "No procedure found, no coercion possible" op type-tags))
                         (coercion-funcs
                          (apply apply-generic op (map-funcs coercion-funcs args)))
                         (else
                          (error "Coercion not possible" op type-tags))))
              (no-method-error op type-tags))))))

;=======================================================================

(define (primitive? datum) (number? datum))

(define (attach-tag type-tag contents)
  (if (primitive? contents)
      contents
      (cons type-tag contents)))

(define (type-tag datum)
  (cond ((pair? datum) (car datum))
        ((number? datum) 'scheme-number)
        (else (error "Bad tagged datum -- TYPE-TAG" datum))))

(define (contents datum)
  (cond ((pair? datum) (cdr datum))
        ((number? datum) datum)
        (else (error "Bad tagged datum -- CONTENTS" datum))))

;=======================================================================

(define (install-scheme-number-package)
  (define (tag x)
    (attach-tag 'scheme-number x))    
  (put 'add '(scheme-number scheme-number)
       (lambda (x y) (tag (+ x y))))
  (put 'sub '(scheme-number scheme-number)
       (lambda (x y) (tag (- x y))))
  (put 'mul '(scheme-number scheme-number)
       (lambda (x y) (tag (* x y))))
  (put 'div '(scheme-number scheme-number)
       (lambda (x y) (tag (/ x y))))
  (put 'equ? '(scheme-number scheme-number) =)
  (put '=zero? '(scheme-number) (lambda (x) (= x 0)))
  (put 'neg '(scheme-number) (lambda (x) (tag (- x))))
  (put 'make 'scheme-number
       (lambda (x) (tag x)))
  'done)

(define (make-scheme-number n)
  ((get 'make 'scheme-number) n))

(install-scheme-number-package)

;=======================================================================

(define (install-rational-package)
  ;; internal procedures
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  (define (make-rat n d)
    (let ((g (gcd n d)))
      (cons (/ n g) (/ d g))))
  (define (add-rat x y)
    (make-rat (+ (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (sub-rat x y)
    (make-rat (- (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (mul-rat x y)
    (make-rat (* (numer x) (numer y))
              (* (denom x) (denom y))))
  (define (div-rat x y)
    (make-rat (* (numer x) (denom y))
              (* (denom x) (numer y))))
  (define (equ?-rational x y) (and (= (numer x) (numer y)) (= (denom x) (denom y))))
  (define (=zero?-rational x) (= 0 (numer x)))
  ;; interface to rest of the system
  (define (tag x) (attach-tag 'rational x))
  (put 'add '(rational rational)
       (lambda (x y) (tag (add-rat x y))))
  (put 'sub '(rational rational)
       (lambda (x y) (tag (sub-rat x y))))
  (put 'mul '(rational rational)
       (lambda (x y) (tag (mul-rat x y))))
  (put 'div '(rational rational)
       (lambda (x y) (tag (div-rat x y))))
  (put 'equ? '(rational rational) equ?-rational)
  (put '=zero? '(rational) =zero?-rational) 

  (put 'make 'rational
       (lambda (n d) (tag (make-rat n d))))
  'done)

(define (make-rational n d)
  ((get 'make 'rational) n d))

(install-rational-package)

;=======================================================================

(define (square x) (* x x))

(define (install-polar-package)
  ;; internal procedures
  (define (magnitude z) (car z))
  (define (angle z) (cdr z))
  (define (make-from-mag-ang r a) (cons r a))
  (define (real-part z)
    (* (magnitude z) (cos (angle z))))
  (define (imag-part z)
    (* (magnitude z) (sin (angle z))))
  (define (make-from-real-imag x y) 
    (cons (sqrt (+ (square x) (square y)))
          (atan y x)))
  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'polar x))
  (put 'real-part '(polar) real-part)
  (put 'imag-part '(polar) imag-part)
  (put 'magnitude '(polar) magnitude)
  (put 'angle '(polar) angle)
  (put 'make-from-real-imag 'polar
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'polar 
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

(define (install-rectangular-package)
  ;; internal procedures
  (define (real-part z) (car z))
  (define (imag-part z) (cdr z))
  (define (make-from-real-imag x y) (cons x y))
  (define (magnitude z)
    (sqrt (+ (square (real-part z))
             (square (imag-part z)))))
  (define (angle z)
    (atan (imag-part z) (real-part z)))
  (define (make-from-mag-ang r a) 
    (cons (* r (cos a)) (* r (sin a))))
  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'rectangular x))
  (put 'real-part '(rectangular) real-part)
  (put 'imag-part '(rectangular) imag-part)
  (put 'magnitude '(rectangular) magnitude)
  (put 'angle '(rectangular) angle)
  (put 'make-from-real-imag 'rectangular 
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'rectangular 
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

(install-rectangular-package)
(install-polar-package)

(define (real-part z) (apply-generic 'real-part z))
(define (imag-part z) (apply-generic 'imag-part z))
(define (magnitude z) (apply-generic 'magnitude z))
(define (angle z) (apply-generic 'angle z))

;=======================================================================

(define (install-complex-package)
  ;; imported procedures from rectangular and polar packages
  (define (make-from-real-imag x y)
    ((get 'make-from-real-imag 'rectangular) x y))
  (define (make-from-mag-ang r a)
    ((get 'make-from-mag-ang 'polar) r a))
  ;; internal procedures
  (define (add-complex z1 z2)
    (make-from-real-imag (+ (real-part z1) (real-part z2))
                         (+ (imag-part z1) (imag-part z2))))
  (define (sub-complex z1 z2)
    (make-from-real-imag (- (real-part z1) (real-part z2))
                         (- (imag-part z1) (imag-part z2))))
  (define (mul-complex z1 z2)
    (make-from-mag-ang (* (magnitude z1) (magnitude z2))
                       (+ (angle z1) (angle z2))))
  (define (div-complex z1 z2)
    (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
                       (- (angle z1) (angle z2))))
  (define (equ?-complex z1 z2) 
    (and (= (real-part z1) (real-part z2)) (= (imag-part z1) (imag-part z2))))

  (define (=zero?-complex z)
    (and (= 0 (real-part z)) (= 0 (imag-part z))))

  ;; interface to rest of the system
  (define (tag z) (attach-tag 'complex z))
  (put 'add '(complex complex)
       (lambda (z1 z2) (tag (add-complex z1 z2))))
  (put 'sub '(complex complex)
       (lambda (z1 z2) (tag (sub-complex z1 z2))))
  (put 'mul '(complex complex)
       (lambda (z1 z2) (tag (mul-complex z1 z2))))
  (put 'div '(complex complex)
       (lambda (z1 z2) (tag (div-complex z1 z2))))
  (put 'equ? '(complex complex) equ?-complex)
  (put '=zero? '(complex) =zero?-complex)
  (put 'make-from-real-imag 'complex
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'complex
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

(define (make-complex-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y))
(define (make-complex-from-mag-ang r a)
  ((get 'make-from-mag-ang 'complex) r a))

(install-complex-package)

;=======================================================================

;; representation of terms and term lists
(define (adjoin-term term term-list)
  (if (=zero? (coeff term))
      term-list
      (cons term term-list)))
(define (the-empty-termlist) '())
(define (first-term term-list) (car term-list))
(define (rest-terms term-list) (cdr term-list))
(define (empty-termlist? term-list) (null? term-list))
(define (make-term order coeff) (list order coeff))
(define (order term) (car term))
(define (coeff term) (cadr term))

(define (install-polynomial-package)
  ;; internal procedures
  ;; representation of poly
  (define (make-poly variable term-list)
    (cons variable term-list))
  (define (variable p) (car p))
  (define (term-list p) (cdr p))

  (define (add-terms L1 L2)
    (cond ((empty-termlist? L1) L2)
          ((empty-termlist? L2) L1)
          (else
           (let ((t1 (first-term L1)) (t2 (first-term L2)))
             (cond ((> (order t1) (order t2))
                    (adjoin-term
                     t1 (add-terms (rest-terms L1) L2)))
                   ((< (order t1) (order t2))
                    (adjoin-term
                     t2 (add-terms L1 (rest-terms L2))))
                   (else
                    (adjoin-term
                     (make-term (order t1)
                                (add (coeff t1) (coeff t2)))
                     (add-terms (rest-terms L1)
                                (rest-terms L2)))))))))

  (define (add-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1)
                   (add-terms (term-list p1)
                              (term-list p2)))
        (error "Polys not in same var -- ADD-POLY"
               (list p1 p2))))

  (define (mul-terms L1 L2)
    (if (empty-termlist? L1)
        (the-empty-termlist)
        (add-terms (mul-term-by-all-terms (first-term L1) L2)
                   (mul-terms (rest-terms L1) L2))))
  (define (mul-term-by-all-terms t1 L)
    (if (empty-termlist? L)
        (the-empty-termlist)
        (let ((t2 (first-term L)))
          (adjoin-term
           (make-term (+ (order t1) (order t2))
                      (mul (coeff t1) (coeff t2)))
           (mul-term-by-all-terms t1 (rest-terms L))))))
  
  (define (mul-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1)
                   (mul-terms (term-list p1)
                              (term-list p2)))
        (error "Polys not in same var -- MUL-POLY"
               (list p1 p2))))

  (define (=zero?-poly p)
    (define (all-coefs-zero? terms) 
      (if (empty-termlist? terms)
          true
          (and (=zero? (coeff (first-term terms))) (all-coefs-zero? (rest-terms terms)))))
    
    (all-coefs-zero? (term-list p)))

  (define (neg-terms terms) 
      (if (empty-termlist? terms)
          terms
          (let ((fterm (first-term terms)))
               (adjoin-term (make-term (order fterm) (neg (coeff fterm))) (neg-terms (rest-terms terms))))))

  (define (neg-poly p)
    (make-poly (variable p) (neg-terms (term-list p))))

  (define (sub-terms L1 L2) (add-terms L1 (neg-terms L2)))

  (define (div-terms L1 L2)
    (if (empty-termlist? L1)
        (list (the-empty-termlist) (the-empty-termlist))
        (let ((t1 (first-term L1))
              (t2 (first-term L2)))
          (if (> (order t2) (order t1))
                (list (the-empty-termlist) L1)
                (let ((new-c (div (coeff t1) (coeff t2)))
                      (new-o (- (order t1) (order t2))))
                  (let ((rest-of-result
                        (div-terms 
                            (sub-terms L1 (mul-term-by-all-terms (make-term new-o new-c) L2))
                            L2
                        )))
                    (list (adjoin-term (make-term new-o new-c) (car rest-of-result)) (cadr rest-of-result))
                    ))))))

  (define (div-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (let ((div-result (div-terms (term-list p1) (term-list p2))))
          (list
           (make-poly (variable p1) (car div-result))  ; quotient
           (make-poly (variable p1) (cadr div-result)) ; rest
           ))
        (error "Cannot perform division, different variables")))


  (define (sub-poly p1 p2) (add-poly p1 (neg-poly p2)))
 
  ;; interface to rest of the system
  (define (tag p) (attach-tag 'polynomial p))
  (put 'add '(polynomial polynomial) 
       (lambda (p1 p2) (tag (add-poly p1 p2))))
  (put 'mul '(polynomial polynomial) 
       (lambda (p1 p2) (tag (mul-poly p1 p2))))
  (put '=zero? '(polynomial) =zero?-poly)
  (put 'sub '(polynomial polynomial) (lambda (p1 p2) (tag (sub-poly p1 p2))))
  (put 'neg '(polynomial) (lambda (p) (tag (neg-poly p))))
  (put 'div '(polynomial polynomial) (lambda (p1 p2) (map tag (div-poly p1 p2))))
  (put 'make 'polynomial
       (lambda (var terms) (tag (make-poly var terms))))
  'done)

(install-polynomial-package)

(define (make-polynomial var terms)
  ((get 'make 'polynomial) var terms))

;=======================================================================

(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))
(define (equ? a b) (apply-generic 'equ? a b))
(define (=zero? x) (apply-generic '=zero? x))
(define (neg x) (apply-generic 'neg x))

(define (scheme-number->complex n)
  (make-complex-from-real-imag (contents n) 0))

(put-coercion 'scheme-number 'complex scheme-number->complex)

; This coercion does nothing, but we require it for the apply-generic algorithm to work
(put-coercion 'complex 'complex (lambda (x) x))

;=======================================================================

(define n1 (make-scheme-number 3))
(define n2 (make-scheme-number 0))

(define r1 (make-rational 1 3))
(define r2 (make-rational 0 7))

(define c1 (make-complex-from-real-imag 1 3))
(define c2 (make-complex-from-real-imag 0 0))
(define c3 (make-complex-from-mag-ang 2 6))
(define c4 (make-complex-from-mag-ang 0 6))

(define p1terms
  (adjoin-term (make-term 5 1) (adjoin-term (make-term 0 -1) (the-empty-termlist))))
(define p1 (make-polynomial 'x p1terms))

(define p2terms
  (adjoin-term (make-term 2 1) (adjoin-term (make-term 0 -1) (the-empty-termlist))))
(define p2 (make-polynomial 'x p2terms))



(div p1 p2)


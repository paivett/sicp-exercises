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

(define (any-false? elements) (lambda (e) (eq? e false)))

; Given a list of types (t1, ..., tN), if there is a type tj to which all types in the list can be coerced to, then
; returns a list if functions (f1, ..., fN) where each function coerces type ti to tj.
; If it is not possible to find N such functions for any target type, the functions returns false.
(define (get-coercion-funcs types) 
  (define (get-coercions-iter candidate-types) ; aca hay algo mal
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
          (if (> (length args) 2)
              (let ((coercion-funcs (get-coercion-funcs type-args)))
                   (cond ((all-same-type type-args)
                          (no-method-error op type-args))
                         (coercion-funcs
                          (apply-generic op (map-funcs coercion-funcs args)))
                         (else
                          (no-method-error op type-args))))
              (no-method-error op type-args))))))
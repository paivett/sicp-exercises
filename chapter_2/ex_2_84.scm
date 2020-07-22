
(define (raise-integer x) (make-rational (contents x) 1))
(define (raise-rational q) (make-real (/ (numer q) (denom q)))) ; Suppose that number and denom are exposed as public
(define (raise-real x) (make-complex-from-real-imag (contents x) 0))

; Install the coercion table for the tower
; (put-coercion 'integer 'rational raise-integer)
; (put-coercion 'rational 'real raise-rational)
; (put-coercion 'real 'complex raise-real)

(define tower '(integer rational real complex))

(define (raise obj)
  (let ((type (type-tag obj)))
    (let ((tail-types (memq type tower)))
         (if (and tail-types (> (length tail-types) 1))
             ((get-coercion type (cadr types)) obj)
             (error "Cannot raise type" type)))))


(define (type> t1 t2) (< (length (memq t1 tower)) (length (memq t2 tower))))


(define (highest-type types)
  (reduce (lambda (t1 t2) (if (type> t1 t2) t1 t2)) (car types) types))


(define (raise-to-target target-type obj)
  (if (type> target-type (type-tag obj))
      (raise-to-target target-type (raise obj))
      obj))


(define (no-method-error op types)
  (error "No method for these types" (list op types)))


(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (if (> (length args) 1)
              (let ((target-type (get-highest-type type-args)))
                   (apply-generic op (map (lambda (arg) (raise-to-target target-type arg)) args)))
              (no-method-error op type-args))))))
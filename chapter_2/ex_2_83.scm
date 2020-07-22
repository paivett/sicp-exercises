
(define (raise-integer x) (make-rational (contents x) 1))
(define (raise-rational q) (make-real (/ (numer q) (denom q)))) ; Suppose that number and denom are exposed as public
(define (raise-real x) (make-complex-from-real-imag (contents x) 0))

; Install the coercion table for the tower
(put-coercion 'integer 'rational raise-integer)
(put-coercion 'rational 'real raise-rational)
(put-coercion 'real 'complex raise-real)

(define tower '(integer rational real complex))

(define (raise obj)
  (let ((type (type-tag obj)))
    (let ((tail-types (memq type tower)))
         (if (and tail-types (> (length tail-types) 1))
             ((get-coercion type (cadr types)) obj)
             (error "Cannot raise type" type)))))

; Another solution could be to provide for each package, a raise operation that knows how to create
; an instance of the next type, and then install for each type a raise operation. And the generic raise
; would only be a call to apply-generic, but then if we install a new package that sits in the middle
; of the tower, we should modify the package below the new one, and I don't want to modify a package if
; I add a new one, I prefer the tower to be known and maintained by an external entity that handles the hierarchy.
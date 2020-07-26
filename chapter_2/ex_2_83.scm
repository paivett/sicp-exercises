
(define (raise-integer x) (make-rational (contents x) 1))
(define (raise-rational q) (make-real (/ (numer q) (denom q)))) ; Suppose that number and denom are exposed as public
(define (raise-real x) (make-complex-from-real-imag (contents x) 0))

; Install the generic raise function for every type except the complex
(put 'raise '(integer) raise-integer)
(put 'raise '(rational) raise-rational)
(put 'raise '(real) raise-real)

(define (raise obj)
  (let ((type (type-tag obj)))
    (let ((raise-type-func (get 'raise type)))
         (if raise-type-func
             (raise-type-func obj)
             (error "Cannot raise type" type)))))

; Another solution could be to provide for each package, a raise operation that knows how to create
; an instance of the next type, and then install for each type a raise operation. And the generic raise
; would only be a call to apply-generic, but then if we install a new package that sits in the middle
; of the tower, we should modify the package below the new one, and I don't want to modify a package if
; I add a new one, I prefer the tower to be known and maintained by an external entity that handles the hierarchy.
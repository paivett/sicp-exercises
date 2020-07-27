
; In the scheme-number package
(put 'sine '(scheme-number) (lambda (x) (tag (sin x))))
(put 'cosine '(scheme-number) (lambda (x) (tag (cos x))))
(put 'arctan '(scheme-number) (lambda (x) (tag (atan x))))
(put 'sqroot '(scheme-number) (lambda (x) (tag (sqrt x))))

; In the rational package
(put 'sine '(rational) (lambda (x) (tag (sin (/ (numer x) (denom x))))))
(put 'cosine '(rational) (lambda (x) (tag (cos (/ (numer x) (denom x))))))
(put 'arctan '(rational) (lambda (x) (tag (atan (/ (numer x) (denom x))))))
(put 'sqroot '(rational) (lambda (x) (tag (sqrt (/ (numer x) (denom x))))))


; Also, for the complex package to accept the numbers defined in the packages, all its internal functions
; that deal with numbers must be modified to use the generic operations defined in the interface of the
; packages. For example, we should use the add operator instead of +

(define (install-rectangular-package)
  ; ...
  (define (magnitude z)
    (sqroot (add (mul (real-part z) (real-part z))
             (mul (imag-part z) (imag-part z)))))
  (define (angle z)
    (arctan (imag-part z) (real-part z)))
  (define (make-from-mag-ang r a) 
    (consine (mul r (cosine a)) (mul r (sine a))))
  ; ...
  'done)

(define (install-polar-package)
  ; ...
  (define (real-part z)
    (mul (magnitude z) (cosine (angle z))))
  (define (imag-part z)
    (mul (magnitude z) (sine (angle z))))
  (define (make-from-real-imag x y) 
    (consine (sqroot (add (mul x x) (mul y y)))
          (arctan y x)))
  ; ...
  'done)

(define (install-complex-package)
  ;...
  (define (add-complex z1 z2)
    (make-from-real-imag (add (real-part z1) (real-part z2))
                         (add (imag-part z1) (imag-part z2))))
  (define (sub-complex z1 z2)
    (make-from-real-imag (sub (real-part z1) (real-part z2))
                         (sub (imag-part z1) (imag-part z2))))
  (define (mul-complex z1 z2)
    (make-from-mag-ang (mul (magnitude z1) (magnitude z2))
                       (add (angle z1) (angle z2))))
  (define (div-complex z1 z2)
    (make-from-mag-ang (div (magnitude z1) (magnitude z2))
                       (sub (angle z1) (angle z2))))
  ; ...
  'done)
(define zero (lambda (f) (lambda (x) x)))

(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))

; One definition
(define one (lambda (f) (lambda (x) (f x))))

; (add-1 zero)
; ((lambda (f) (lambda (x) (f ((zero f) x)))))
; But zero is (lambda (f) (lambda (x) x)), then
; ((lambda (f) (lambda (x) (f ((zero f) x)))))
; ((lambda (f) (lambda (x) (f (((lambda (f) (lambda (x) x)) f) x)))))
; ((lambda (f) (lambda (x) (f ((lambda (x) x) x)))))
; ((lambda (f) (lambda (x) (f x))))

(define two (lambda (f) (lambda (x) (f (f x)))))

; Addition
; Returns a function that applies the function "apply n times f" to the result of applying the function "apply m times f" to x
(define (add n m) (lambda (f) (lambda (x) ((n f) ((m f) x)))))
(define tolerance 0.00001)
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

; phi = (1 + sqrt(5)) / 2
; Show that phi is a fixed point of the transformation x -> 1 + 1/x

; Let's show first that phi^2 = 1 + phi
; phi^2 = ((1 + sqrt(5))/2)^2 = ((1 + sqrt(5))^2)/4 = (1 + 2*sqrt(5) + 5)/4 = (6 + 2*sqrt(5))/4 = (3 + sqrt(5)) / 2
; phi + 1 = (1 + sqrt(5) )/2 + 1 = (2 + 1 + sqrt(5))/2 = (3 + sqrt(5)) / 2
; Since both results are the same, then we have that phi^2 = phi + 1
; Now we have the following transformation: x -> 1 + 1/x, which can be rewritten as x -> (x + 1)/x.
; Now, if we replace x = phi we get phi -> (phi + 1)/phi, but using the above equality, then phi -> (1 + phi)/phi -> phi^2 / phi -> phi

(fixed-point (lambda (x) (+ 1 (/ 1 x))) 1.0)
;Value: 1.6180327868852458
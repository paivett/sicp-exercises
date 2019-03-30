(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1)
                 (A x (- y 1))))))

; (A 1 10) = 1024

; (A 2 4) = 65536

; (A 3 3) = 65536

(define (f n) (A 0 n))

; f = 2n

(define (g n) (A 1 n))

; g(1) = 2
; g(3) = A(1, 3) = A(0, A(1, 2)) = 2 A(1, 2) = 2 A(0, A(1, 1)) = 2 2 2 = 8
; g(4) = A(1, 4) = A(0, A(1, 2)) = 2 A(1, 3) = 2 A(0, A(1, 2)) = ... = 16
; g(n) = 2^n

(define (h n) (A 2 n))

;h(1) = A(2, 1) = 2^1
;h(2) = 4 = 2^2
;h(3) = 16 = 2^4
;h(4) = 65536 = 2^16
;h(n) = 2^(h(n-1))

(define (k n) (* 5 n n))
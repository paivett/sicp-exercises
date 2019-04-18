(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))    
; This version takes the remainder on each step, so every time the squared number is less than m.

(define (expmod-p-hacker base exp m)
  (remainder (fast-expt base exp) m))
; This version takes the remainder at the end of the total exponentiation. This causes that the
; remainder is taken to a huge number, and managing huge numbers is much more difficult than
; 64bit numbers.
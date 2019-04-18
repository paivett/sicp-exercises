(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (* (expmod base (/ exp 2) m)
                       (expmod base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))

; This ends up being O(n) because its T(n) function is T(n) = 2T(n/2) + O(1), this is O(n) (Check the master theorem)

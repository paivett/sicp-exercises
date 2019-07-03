(define (no-more? coins) (null? coins))

(define (first-denomination coins) (car coins))

(define (except-first-denomination coins) (cdr coins))

(define (cc amount coin-values)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (no-more? coin-values)) 0)
        (else
         (+ (cc amount
                (except-first-denomination coin-values))
            (cc (- amount
                   (first-denomination coin-values))
                coin-values)))))

(define us-coins (list 1 25 10 5 50))
(define uk-coins (list 100 50 20 10 5 2 1 0.5))

; (count-change 100)
; 292

; The order of the coins does not affect the overall result, because the algorithm never makes
; any asumption that the partial result has been computed with higher denomination coins for computing
; for the current coin
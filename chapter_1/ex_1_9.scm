(define (+ a b)
  (if (= a 0)
      b
      (inc (+ (dec a) b))))

; For example, if we do (+ 5 5), the process would look like

; (+ 5 5)
; (inc (+ (dec 5) 5))
; (inc (+ 4 5))
; (inc (inc (+ (dec 4) 5)))
; (inc (inc (+ 3 5)))
; (inc (inc (inc (+ (dec 3) 5))))
; (inc (inc (inc (+ 2 5))))


; And so forth, so we can conclude that this is a linear recursive process

(define (+ a b)
  (if (= a 0)
      b
      (+ (dec a) (inc b))))

; Now, with this new definition, if we do (+ 5 5) we would get something like

; (+ 5 5)
; (+ (dec 5) (inc 5))
; (+ 4 6)
; (+ (dec 4) (inc 6))
; (+ 3 7)

; And so forth, this is clearly an linear iterative process

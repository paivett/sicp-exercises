(define (a-plus-abs-b a b)
    ((if (> b 0) + -) a b))

; When calling (a-plus-abs-b a b) the execution this expands to
; ((if (> b 0) + -) a b)
; then if b is larger than 0, this will expand to
; (+ a b)
; else, this expands to 
; (- a b)
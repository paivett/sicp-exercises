(define (f g)
  (g 2))

;(f f)

;The object 2 is not applicable.

; What happens is that (f f) yields (f 2) because of the definition of f, and applying again what f means, we get (2 2)
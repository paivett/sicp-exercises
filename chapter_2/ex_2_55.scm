(car ''abracadabra)

; The expression is seen by the interpreter as (car (quote (quote abracadabra)))
; So when evaluating the car function, the first quote is evaluated, yielding
; a list of two symbols: quote and abracadabra. An equivalent would be (car ('quote 'abracadabra))
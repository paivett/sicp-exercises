(define (square-list items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things) 
              (cons (square (car things))
                    answer))))
  (iter items (list)))

; This reverses the list because the result is built appending
; each element in the beggining. This way, the first element of
; the original list will be appended to the empty list, and will end
; last in the result. The second element of the original list will be
; appended to the list containing only one element, and so on.

(define (square-list items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons answer
                    (square (car things))))))
  (iter items (list)))

; This will not produce a list where each element is a pair, the car is the value,
; and the cdr is the remaining list. This generates pairs where the car is the remaining
; elements and the cdr is the value.
(define nil (list))

(define (subsets s)
  (if (null? s)
      (list nil)
      (let ((rest (subsets (cdr s))))
        (append rest (map (lambda (e) (append e (list (car s)))) rest)))))

(define s (list 1 2 3))

(subsets s)
; (() (3) (2 3) (2 3 3) (1 2 3) (1 2 3 3) (1 2 3 2 3) (1 2 3 2 3 3))

; To analyze why this works, we must divide this into two cases
; 
; Base case
; s is the empty set, then the only subset is the empty set itself.
; 
; Recursive case
; s is some subset of the original set (it is the original set the first call).
; At this point, we can think of s as the set (x, ...) where x is some element of s
; and ... represents the rest of the elements (may be empty).
; So, all possible subets can be divided into those that contain x, and those which
; does not contain x.
; The recursive call soted into rest variable builds the set of all subsets that do not
; contain x since we are calling it with cdr.
; So, we must append to all these subsets, all subsets that contain x. And we can build
; those by appending x to every subset that did not contain it (a copy). This is the map call.

(list 'a 'b 'c)
;Value 13: (a b c)

(list (list 'george))
;Value 14: ((george))

(cdr '((x1 x2) (y1 y2)))
;Value 15: ((y1 y2))

(cadr '((x1 x2) (y1 y2)))
;Value 16: (y1 y2)

(pair? (car '(a short list)))
;Value: #f

(define (memq item x)
  (cond ((null? x) false)
        ((eq? item (car x)) x)
        (else (memq item (cdr x)))))

(memq 'red '((red shoes) (blue socks)))
;Value: #f

(memq 'red '(red shoes blue socks))
;Value 17: (red shoes blue socks)
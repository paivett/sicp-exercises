(define (list-equal? a b)
  (cond ((and (null? a) (null? b)) #t)
        ((and (null? a) (not (null? b))) #f)
        ((and (not (null? a)) (null? b)) #f)
        (else (and (equal? (car a) (car b)) (list-equal? (cdr a) (cdr b))))))

(define (equal? a b)
  (if (and (list? a) (list? b))
      (list-equal? a b)
      (eq? a b)))

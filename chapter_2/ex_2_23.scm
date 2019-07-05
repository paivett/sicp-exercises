(define (for-each proc items)
    (define (for-each-inner proc items result)
      (if (null? items)
          result
          (for-each-inner proc (cdr items) (proc (car items)))))

    (for-each-inner proc items #t))

; This could be another simpler implementation, regardless of the output
; (define (for-each proc items) (map proc items))

(for-each (lambda (x) (newline) (display x))
          (list 57 321 88))
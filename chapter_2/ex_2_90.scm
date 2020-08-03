(define (install-polynomial-package)
    ;; internal procedures
    ;; representation of poly
    (define (make-poly variable term-list)
      (cons variable term-list))
    (define (variable p) (car p))
    (define (term-list p) (cdr p))

    ;; representation only of terms, the term list is generic
    (define (make-term order coeff) (list order coeff))
    (define (order term) (car term))
    (define (coeff term) (cadr term))

    (define (add-poly p1 p2) ...)
    ; ...
    (define (mul-poly p1 p2) ...)
    ; ...
    
    ;; interface to rest of the system
    (define (tag p) (attach-tag 'polynomial p))
    (put 'add '(polynomial polynomial) 
        (lambda (p1 p2) (tag (add-poly p1 p2))))
    (put 'mul '(polynomial polynomial) 
        (lambda (p1 p2) (tag (mul-poly p1 p2))))
    (put 'make 'polynomial
        (lambda (var terms) (tag (make-poly var terms))))
    'done )

; Sparse
(define (install-sparse-term-list)
    ; internal definitions
    (define (adjoin-sparse-term term term-list)
    (if (=zero? (coeff term))
        term-list
        (cons term term-list)))
    (define (the-empty-sparse-termlist) '())
    (define (first-sparse-term term-list) (car term-list))
    (define (rest-sparse-terms term-list) (cdr term-list))
    (define (empty-sparse-termlist? term-list) (null? term-list))

    ;; public interface
    (define (tag ts) (attach-tag 'sparse-list ts))
    (put 'adjoin-term 'sparse-list
        (lambda (t ts) (tag (adjoin-sparse-term t ts))))
    (put 'the-empty-termlist 'sparse-list the-empty-sparse-termlist)
    (put 'first-term '(sparse-list) first-sparse-term)
    (put 'rest-terms '(sparse-list)
        (lambda (ts) (tag (rest-sparse-terms ts))))
    (put 'empty-termlist? '(sparse-list) empty-sparse-termlist?)

    'done )


; Dense
(define (install-dense-term-list)
    ; internal definitions
    (define (adjoin-dense-term term term-list)
    (if (= (order term) (length term-list))
        (cons (coeff term) term-list)
        (adjoin-dense-term term (cons 0 term-list))))
    (define (the-empty-dense-termlist) '())
    ; When returning a term, the order is the position of that term, that is, the length of the list
    (define (first-dense-term term-list) (list (- (length term-list) 1) (car term-list)))
    (define (rest--dense-terms term-list) (cdr term-list))
    (define (empty-dense-termlist? term-list) (null? term-list))

    ;; public interface
    (define (tag ts) (attach-tag 'dense-list ts))
    (put 'adjoin-term 'dense-list
        (lambda (t ts) (tag (adjoin-dense-term t ts))))
    (put 'the-empty-termlist 'dense-list the-empty-dense-termlist)
    (put 'first-term '(dense-list) first-dense-term ts)
    (put 'rest-terms '(dense-list)
        (lambda (ts) (tag (rest-dense-terms ts))))
    (put 'empty-termlist? '(dense-list) empty-dense-termlist?)

    'done )

; These two operations allows the user to create sparse or dense lists and then start
; operating on them with the genreic functions
(define sparse-empty-term-list (get 'the-empty-termlist 'sparse-list ))
(define dense-empty-term-list (get 'the-empty-termlist 'dense-list ))
(define (adjoin-term term term-list) ((get 'adjoin-term (type-tag term-list)) term term-list))


(define (install-polynomial-package)
  ;; internal procedures
    (define (sub-terms L1 L2) (add-terms L1 (neg-terms L2)))

    (define (div-terms L1 L2)
    (if (empty-termlist? L1)
        (list (the-empty-termlist) (the-empty-termlist))
        (let ((t1 (first-term L1))
                (t2 (first-term L2)))
            (if (> (order t2) (order t1))
                (list (the-empty-termlist) L1)
                (let ((new-c (div (coeff t1) (coeff t2)))
                    (new-o (- (order t1) (order t2))))
                (let ((rest-of-result
                        (div-terms 
                            (sub-terms L1 (mul-term-by-all-terms (make-term new-c new-o) L2))
                            L2
                        )))
                    (list (adjoin-term (make-term new-c new-o) (car rest-of-result)) (cadr rest-of-result))
                    ))))))

    (define (div-poly p1 p2)
        (if (same-variable? (variable p1) (variable p2))
            (let ((div-result (div-terms (term-list p1) (term-list p2))))
                 (list
                    (make-poly (variable p1) (car div-result))  ; quotient
                    (make-poly (variable p1) (cadr div-result)) ; rest
                ))
            (error "Cannot perform division, different variables")))

    ; Public interface
    (put 'div '(polynomial polynomial) (lambda (p1 p2) (map tag (div-poly p1 p2))))

    'done)

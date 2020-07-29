(define (install-polynomial-package)
  ;; internal procedures
  (define (zero?-poly p)
    (define (all-coefs-zero? terms) 
      (if (empty-termlist? terms)
          true
          (and (zero? (first-term terms)) (all-coefs-zero? (rest-terms terms)))))
    
    (all-coefs-zero? (term-list p)))

  ; Install public interface
  (put 'zero? '(polynomial) zero?-poly)
  ; ...

  'done)
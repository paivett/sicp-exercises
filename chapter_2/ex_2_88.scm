; As the exercise suggests, lets assume that there is a neg generic function that given an arithmentic
; object, it negates it. Let's implement the neg for polynomials and use it to define the substraction.


(define (install-polynomial-package)
  ;; internal procedures
  (define (neg-poly-poly p)
    (define (neg-terms terms) 
      (if (empty-termlist? terms)
          terms
          (let ((fterm (first-term terms)))
               (adjoin-term (make-term (order fterm) (neg (coeff fterm)))) (neg-terms (rest-terms terms)))))
    
    (make-poly (variable p) (neg-terms (terms p))))
  
  (define (sub-poly p1 p2) (add-poly p1 (neg p2)))

  ; Install public interface
  (put 'neg '(polynomial) (lambda (p) (tag (neg-poly p))))
  (put 'sub '(polynomial polynomial) (lambda (p1 p2) (tag (sub-poly p1 p2))))
  ; ...

  'done)
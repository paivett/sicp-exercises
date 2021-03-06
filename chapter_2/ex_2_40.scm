(define nil (list))

(define (prime? n)
    (define (next d)
        (if (= d 2)
            3
            (+ d 2)))
    
    (define (smallest-divisor n)
        (find-divisor n 2))
    
    (define (find-divisor n test-divisor)
        (cond ((> (square test-divisor) n) n)
                ((divides? test-divisor n) test-divisor)
                (else (find-divisor n (next test-divisor)))))
    
    (define (divides? a b)
        (= (remainder b a) 0))

    (= n (smallest-divisor n)))

(define (enumerate-interval low high)
  (if (> low high)
      nil
      (cons low (enumerate-interval (+ low 1) high))))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

; (accumulate append
;             nil
;             (map (lambda (i)
;                    (map (lambda (j) (list i j))
;                         (enumerate-interval 1 (- i 1))))
;                  (enumerate-interval 1 n)))

(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))

(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))

(define (make-pair-sum pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

(define (unique-pairs n) (flatmap
  (lambda (i)
    (map (lambda (j) (list i j))
          (enumerate-interval 1 (- i 1))))
  (enumerate-interval 1 n)))

(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum? (unique-pairs n))))

(define nil (list))

(define (enumerate-interval low high)
  (if (> low high)
      nil
      (cons low (enumerate-interval (+ low 1) high))))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))

(define (unique-pairs n)
  (flatmap (lambda (i) (map (lambda (j) (list i j)) (enumerate-interval 1 (- i 1))))
           (enumerate-interval 1 n)))

(define (unique-triplets n) 
  (flatmap (lambda (i) (map (lambda (tuple) (cons i tuple)) (unique-pairs (- i 1))))
           (enumerate-interval 1 n)))

(define (sum seq) (accumulate + 0 seq))

(define (triplets-that-sum s n)
  (filter (lambda (triplet) (= (sum triplet) s)) (unique-triplets n)))

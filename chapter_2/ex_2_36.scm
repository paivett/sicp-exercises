(define nil (list))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      nil
      (cons (accumulate op init (accumulate cons nil (map car seqs)))
            (accumulate-n op init (accumulate cons nil (map cdr seqs))))))

(define s (list (list 1 2 3) (list 4 5 6)))

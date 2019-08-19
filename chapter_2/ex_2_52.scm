; Part A
(define wave 
  (segments->painter (list (make-segment (make-vect .25 0) (make-vect .35 .5)) 
                           (make-segment (make-vect .35 .5) (make-vect .3 .6)) 
                           (make-segment (make-vect .3 .6) (make-vect .15 .4)) 
                           (make-segment (make-vect .15 .4) (make-vect 0 .65)) 
                           (make-segment (make-vect 0 .65) (make-vect 0 .85)) 
                           (make-segment (make-vect 0 .85) (make-vect .15 .6)) 
                           (make-segment (make-vect .15 .6) (make-vect .3 .65)) 
                           (make-segment (make-vect .3 .65) (make-vect .4 .65)) 
                           (make-segment (make-vect .4 .65) (make-vect .35 .85)) 
                           (make-segment (make-vect .35 .85) (make-vect .4 1)) 
                           (make-segment (make-vect .4 1) (make-vect .6 1)) 
                           (make-segment (make-vect .6 1) (make-vect .65 .85)) 
                           (make-segment (make-vect .65 .85) (make-vect .6 .65)) 
                           (make-segment (make-vect .6 .65) (make-vect .75 .65)) 
                           (make-segment (make-vect .75 .65) (make-vect 1 .35)) 
                           (make-segment (make-vect 1 .35) (make-vect 1 .15)) 
                           (make-segment (make-vect 1 .15) (make-vect .6 .45)) 
                           (make-segment (make-vect .6 .45) (make-vect .75 0)) 
                           (make-segment (make-vect .75 0) (make-vect .6 0)) 
                           (make-segment (make-vect .6 0) (make-vect .5 .3)) 
                           (make-segment (make-vect .5 .3) (make-vect .4 0))
                           (make-segment (make-vect .5 .33) (make-vect .4 0)) ; Random lines
                           (make-segment (make-vect .15 .3) (make-vect .4 0)) ; Random lines
                           (make-segment (make-vect .4 0) (make-vect .25 0)))))

; Part B
(define (corner-split painter n)
  (if (= n 0)
      painter
      (let ((up (up-split painter (- n 1)))
            (right (right-split painter (- n 1))))
        (let ((top-left up)
              (bottom-right right)
              (corner (corner-split painter (- n 1))))
          (beside (below painter top-left)
                  (below bottom-right corner))))))

; Part C
(define (square-limit painter n)
  (let ((combine4 (square-of-four flip-vert rotate180
                                  identity flip-horiz)))
    (combine4 (corner-split painter n))))

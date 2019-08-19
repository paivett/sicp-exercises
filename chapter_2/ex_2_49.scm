(define outline-painter
  (let ((v0 (make-vect 0 0))
        (v1 (make-vect 0 1))
        (v2 (make-vect 1 1))
        (v3 (make-vect 1 0)))
       (segments->painter (list (make-segment v0 v1) (make-segment v1 v2) (make-segment v2 v3) (make-segment v3 v0)))))

(define x-painter
  (let ((v0 (make-vect 0 0))
        (v1 (make-vect 0 1))
        (v2 (make-vect 1 1))
        (v3 (make-vect 1 0)))
       (segments->painter (list (make-segment v0 v2) (make-segment v1 v3)))))

(define diamond-painter
  (let ((v0 (make-vect 0 0.5))
        (v1 (make-vect 0.5 1))
        (v2 (make-vect 1 0.5))
        (v3 (make-vect 0.5 0)))
       (segments->painter (list (make-segment v0 v1) (make-segment v1 v2) (make-segment v2 v3) (make-segment v3 v0)))))


; I copied this one from a solution here: http://community.schemewiki.org/?sicp-ex-2.49
; since I did not wanted to build the wave by trial and error
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
                           (make-segment (make-vect .4 0) (make-vect .25 0)))))

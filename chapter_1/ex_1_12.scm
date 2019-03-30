(define (f row col)
    (if (or (= row 1) (= col 1) (= col row))
        1
        (+ (f (- row 1) (- col 1)) (f (- row 1) col))
    )
)
(define (sum term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ (term a) result))
        )
    )
  (iter a 0)
  )

(define (inc n) (+ n 1))

(define (sum-cubes a b)
  (define (cube n)
    (* n n n))
  (sum cube a inc b)
  )

(print (sum-cubes 1 10))

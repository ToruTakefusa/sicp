(define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a) (accumulate combiner null-value term (next a) next b)))
  )

(define (sum term a next b)
  (accumulate + 0 term a next b)
  )

(define (inc n) (+ n 1))
(define (cube n) (* n n n))

(define (factorial n)
  (define (identity n) n)
  (accumulate * 1 identity 1 inc n)
  )

(print (sum cube 1 inc 10))
(print (factorial 5))

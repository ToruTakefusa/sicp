(define (product term a next b)
  (if (> a b)
      1
      (* (term a) (product term (next a) next b)))
  )

(define(inc n) (+ n 1))

(define (factorial n)
  (define (identity n) n)
  (product identity 1 inc n)
  )

(define (fraction-term n)
  (cond ((= n 1) 2)
        ((even? n) (+ n 2))
        (else (+ n 1))))

(define (even? n)
  (= (remainder n 2) 0))

(define (numerator-term n)
  (if (even? n) (+ n 1) (+ n 2)))

(define (pi n)
  (* 4 (exact->inexact (/ (product fraction-term 1 inc n) (product numerator-term 1 inc n))))
  )

(print (factorial 1))
(print (factorial 2))
(print (factorial 3))
(print (factorial 4))
(print (factorial 5))
(print (pi 1))
(print (pi 3))
(print (pi 10))
(print (pi 30))
(print (pi 100))

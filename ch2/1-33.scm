(define (accumulate filter combiner null-value term a next b)
  (define (iter a result)
    (cond ((> a b) result)
          ((filter a) (iter (next a) (combiner (term a) result)))
          (else (iter (next a) result))))
  (iter a null-value)
  )

(define (prime? n)
  (define (smallest-divisor n)
    (define (find-divisor n test-divisor)
      (cond ((> (square test-divisor) n) n)
            ((divides? test-divisor n) test-divisor)
            (else (find-divisor n (+ test-divisor 1)))
            ))
    (find-divisor n 2))
  (= n (smallest-divisor n)))

(define (divides? a b)
  (= (remainder b a) 0))

(define (square n) (* n n))

(define (inc n) (+ n 1))

(define (identity n) n)

(define (square-prime a b)
  (accumulate prime? + 0 square a inc b))

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(define (gcd-filter a b)
  (= (gcd a b) 1))

(define (gcd-product n)
  (define (filter a) (gcd-filter a n))
  (accumulate filter * 1 identity 1 inc n)
  )

(print (square-prime 2 4))
(print (square-prime 2 6))
(print (square-prime 2 8))
(print (gcd-product 5))

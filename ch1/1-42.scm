(define (square n)(* n n))
(define (inc n) (+ n 1))

(define (compose f g)
  (lambda(n) (f (g n))))

(print ((compose square inc) 6))

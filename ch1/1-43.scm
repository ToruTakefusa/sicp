(define (square n) (* n n))

(define (compose f g)
  (lambda (n) (f (g n))))

(define (repeat f n)
    (if (< n 1)
        (lambda (x) x)
        (compose f (repeat f (- n 1)))))


(print ((repeat square 2) 5))

(define (make-interval a b) (cons a b))
(define (lower-bound interval)
  (min (car interval) (cdr interval)))
(define (upper-bound interval) (max (car interval)(cdr interval)))

(print (lower-bound (make-interval 1 3)))
(print (upper-bound (make-interval 1 3)))
(print (lower-bound (make-interval 3 1)))
(print (upper-bound (make-interval 3 1)))

(define (make-accumulator sum)
  (lambda (value)
    (begin (set! sum (+ sum value))
           sum)))

(define A (make-accumulator 5))
(print (A 10))
(print (A 10))

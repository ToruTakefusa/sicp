(define x 0)
(define before 0)

(define (f n)
  (set! before x)
  (set! x n)
  before)

(print (+ (f 0) (f 1)))
(print (+ (f 1) (f 0)))

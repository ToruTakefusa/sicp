(define (iterative-improve good-enough? improve)
  (lambda (x)
    (define (iter n)
      (if (> n good-enough?)
          n
          (iter (improve n))))
  (iter x)))

(define (sqrt x)
  ((iterative-improve
   (lambda(y) (< (abs (- (square y ) x)) 0.001))
   (lambda(y) (/ (+ guess (/ x guess) ) 2)))
   1.0))



(print (sqrt 9))


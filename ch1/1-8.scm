(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve  guess x)
                 x)))

(define (improve guess x)
  (/ (+ (/ x (square guess)) (* 2.0 guess)) 3.0))

(define (good-enough? guess x)
  ( < (abs (- (cube guess) x )) 0.0001))

(define (cube x) (* x x x))
(define (sqrt x)
  (sqrt-iter 1.0 x))

(define (square x) (* x x))

(print (sqrt 9))

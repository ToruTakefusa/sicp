(define (sqrt-iter guess old_guess x)
  (if (good-enough? guess old_guess x)
	  guess
	  (sqrt-iter (improve guess x)
				 guess
				 x)))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

(define (good-enough? guess guess_before x)
  (< (abs (- (square guess) (square guess_before))) 0.001))

(define (sqrt x)
  (sqrt-iter 1.0 0 x))

(print (sqrt 100))

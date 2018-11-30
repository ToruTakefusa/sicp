(define (sqrt-iter guess x)
  (if (good-enough? guess x) guess
	  (sqrt-iter (better-improve x guess) x)))

(define (improve guess x)
  (better-improve x guess))

(define (average x y)
  (/ (+ x y) 2))

(define (better-improve x y)
  (/ (+ (/ x (square y)) (* 2 y)) 3))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

(define (square x)
		 (* x x))

(define (sqrt x)
  (sqrt-iter 1.0 x))

(print (sqrt 10))

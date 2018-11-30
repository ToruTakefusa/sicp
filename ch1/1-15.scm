(define (cube x) (* x x x))
(define (p x count)
  (print count)
  (- (* 3 x)(* 4 (cube x))))

(define (sine angle count)
  (if (not (> (abs angle) 0.1))
	  angle
	  (p (sine(/ angle 3.0) (+ count 1)) count)))
(print (sine 12.15 1))

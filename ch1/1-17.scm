(define (* a b)
  (cond ((= b 0) 0)
		((even? b) (double (* a (halve b))))
		(else (+ a (* a (- b 1))))))

(define (double n)
  (+ n n))

(define (halve n)
  (/ n 2))

(define (even? n)
  (= (remainder n 2) 0))

(print (* 1 0))
(print (* 2 2))
(print (* 3 2))
(print (* 4 5))
(print (* 10 10))

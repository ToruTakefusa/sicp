(define (func n)
  (if (< n 3)
	  n
	  (+ (func (- n 1)) (* 2 (func (- n 2))) (* 3 (func (- n 3))))))

(define (f-iter a b c n count)
  (if (= n 0)
	  a
	  (cond ((< count 3) (f-iter count a b (- n 1) (+ count 1)))
			(else (f-iter (+ a (* 2 b) (* 3 c)) a b (- n 1) (+ count 1))))))

(define (func2 n)
  (f-iter 1 0 0 n 0))

(print (func 10))
(print (func2 11))

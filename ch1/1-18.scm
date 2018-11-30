(define (product-iter a b n)
  (cond ((= b 0) n)
		((even? b) (product-iter (double a) (halve b) n))
		(else (product-iter a (- b 1) (+ a n)))))

(define (double n)
  (+ n n))

(define (halve n)
  (/ n 2))

(define (even? n)
  (= (remainder n 2) 0))

(define (product a b)
  (product-iter a b 0))

(print (product 0 0))
(print (product 1 2))
(print (product 2 2))
(print (product 3 5))
(print (product 10 10))

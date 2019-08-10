(define (expt b n)
  (define (expt-iter a b n)
    (cond ((= n 0) a)
    ((even? n) (expt-iter a (square b) (/ n 2)))
    (else (expt-iter (* a b) b (- n 1))))
    )
  (expt-iter 1 b n)
  )

(define (square n) (* n n))

(define (even? n)
  (= (remainder n 2) 0))


(print (expt 2 0))
(print (expt 2 1))
(print (expt 2 2))
(print (expt 2 3))
(print (expt 2 4))

(print (expt 3 0))
(print (expt 3 1))
(print (expt 3 2))
(print (expt 3 3))
(print (expt 3 4))

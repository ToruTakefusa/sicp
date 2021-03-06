(define (times a b)
  (cond ((= b 0) 0)
        ((= b 1) a)
        ((even? b) (times (double a) (halve b)))
        (else (+ a (times a (- b 1))))
        )
  )

(define (double n) (* 2 n))
(define (halve n) (/ n 2))
(define (even n) (= (remainder n 2) 0))

(print (times 1 0))
(print (times 1 1))
(print (times 2 0))
(print (times 2 1))
(print (times 2 2))
(print (times 2 3))
(print (times 2 4))
(print (times 2 5))
(print (times 2 6))
(print (times 3 0))
(print (times 3 1))
(print (times 3 2))
(print (times 3 3))
(print (times 3 4))
(print (times 3 5))

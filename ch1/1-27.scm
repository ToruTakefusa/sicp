(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m)) m))
        (else (remainder (* base (expmod base (- exp 1) m)) m))
        )
  )

(define (full-fermat-prime? n)
  (define (full-fermat-prime-iter a n)
    (if (= n a) #t
        (if (= (expmod a n n) a) (full-fermat-prime-iter (+ a 1) n)
            #f)
        )
    )
  (full-fermat-prime-iter 1 n)
  )

(print (full-fermat-prime? 2))
(print (full-fermat-prime? 3))
(print (full-fermat-prime? 10))
(print (full-fermat-prime? 561))
(print (full-fermat-prime? 1105))
(print (full-fermat-prime? 1729))
(print (full-fermat-prime? 2465))
(print (full-fermat-prime? 2821))
(print (full-fermat-prime? 6601))

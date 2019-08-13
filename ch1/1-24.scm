(define (time-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))

(define (runtime)
  (use srfi-11)
  (let-values (((a b) (sys-gettimeofday)))
    (+ (* a 1000000) b)
    )
  )

(define (start-prime-test n start-time)
  (if (prime? n)
  (report-prime (- (runtime) start-time)))
  )

(define (report-prime elapsed-time)
  (display "***")
  (display elapsed-time)
  )


(define (divides? a b)
  (= (remainder b a) 0)
  )

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)(remainder (square (expmod base (/ exp 2) m))
                               m))
        (else (remainder (* base (expmod base (- exp 1) m)) m))
        )
  )

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (use srfi-27)
  (try-it (+ 1 (random-integer (- n 1))))
  )

(define (fast-prime? n times)
  (cond ((= times 0) #t)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else #f)
        )
  )

(define (prime? n)
  (fast-prime? n 100)
  )

(define (even? n) (= (remainder n 2) 0))

(define (prime-test start end)
  (define(prime-test-iter start end)
    (if (<= start end) (time-prime-test start))
    (if (<= start end) (prime-test-iter (+ 2 start) end))
    )
  (prime-test-iter start end)
  )

(print (prime-test 1001 1021))
(print (prime-test 10001 10051))
(print (prime-test 100001 100061))
(print (prime-test 1000001 1000071))



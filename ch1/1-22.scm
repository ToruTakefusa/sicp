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

(define (smallest-divisor n)
  (find-divisor n 2)
  )

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))
        )
  )

(define (divides? a b)
  (= (remainder b a) 0)
  )

(define (prime? n)
  (= n (smallest-divisor n))
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



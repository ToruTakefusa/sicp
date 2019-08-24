(define (cc amount coin-values)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (no-more? coin-values)) 0)
        (else
         (+ (cc amount
                (except-first-denomination coin-values))
            (cc (- amount
                   (first-denomination coin-values))
                coin-values)))))
(define (first-denomination list)
  (car list))

(define (except-first-denomination list)
   (cdr list))

(define (no-more? list)
  (null? list))

(define us-coins (list 50 25 10 5 1))

(print (cc 100 us-coins))
(print (cc 100 (list 50 25 5 10 1)))
(print (cc 100 (list 5 1 50 25 10)))

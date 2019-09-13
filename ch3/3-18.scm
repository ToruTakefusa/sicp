(define (last-pair x)
  (if (null? (cdr x))
      x
      (last-pair (cdr x))))

(define (make-cycle x)
  (set-cdr! (last-pair x) x)
  x)

(define z (make-cycle (list `a `b `c)))
(print (car z))
(print (cadddr z))
(print (eq? (car z) (cadddr z)))

(define foo (list `a `b `c `a))
(print (caddr foo))
(print (eq? (car foo) (cadddr foo)))

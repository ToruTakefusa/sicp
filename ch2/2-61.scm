(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((= x (car set)) set)
        ((< x (car set)) (cons x set))
        (else (cons (car set) (adjoin-set x (cdr set))))))

(print (adjoin-set 1 (list 3 4 5 6)))
(print (adjoin-set 3 (list 1 4 5 6)))
(print (adjoin-set 6 (list 1 2 4 5)))

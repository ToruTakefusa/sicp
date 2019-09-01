(define (element-of-set? x set)
  (cond ((null? set) #f)
        ((equal? x (car set)) #t)
        (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
      (cons x set))

(define (union-set set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        (else (union-set (cdr set1) (cons (car set1) set2)))))

(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2 )) `())
        ((element-of-set? (car set1) set2)
         (cons (car set1) (intersection-set (cdr set1) set2)))
        (else (intersection-set (cdr set1) set2))))

(print (element-of-set? 1 (list 4 5 6)))
(print (adjoin-set 1 (list 4 5 6)))
(print (union-set (list 1 3 4) (list 5 4 6)))
(print (intersection-set (list 1 3 4 4 1) (list 1 5 5 4 1 1 4)))

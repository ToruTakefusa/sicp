(define (union-set set1 set2)
    (cond ((null? set1) set2)
          ((null? set2) set1)
          ((and (null? set1) (null? set2)) nil)
          ((= (car set1) (car set2))
           (cons (car set1) (union-set (cdr set1) (cdr set2))))
          ((< (car set1) (car set2))
           (cons (car set1) (union-set (cdr set1) set2)))
          ((> (car set1) (car set2))
           (cons (car set2) (union-set set1 (cdr set2))))))

(define nil `())

(print (union-set (list 1 2 3 5) (list 1 3 4)))

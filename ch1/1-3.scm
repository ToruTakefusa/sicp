(define (square x) (* x x))
(define (sum-of-squares x y)
  (+ (square x) (square y)))


(define  (bigger-sum-of-squares a b c)
  (cond ((and (< a b) (< a c)) (sum-of-squares b c))
        ((and (< b a) (< b c)) (sum-of-squares a c))
        (else (sum-of-squares a b))
        )
)

(print (bigger-sum-of-squares 1 2 3))
(print (bigger-sum-of-squares 3 2 1))
(print (bigger-sum-of-squares 3 3 3))

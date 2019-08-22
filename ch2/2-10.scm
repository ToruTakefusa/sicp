(define (make-interval a b) (cons a b))
(define (lower-bound interval)
  (min (car interval) (cdr interval)))
(define (upper-bound interval)
  (max (car interval)(cdr interval)))
(define (add-interval a b)
  (make-interval (+ (lower-bound a) (lower-bound b))
                 (+ (upper-bound a) (upper-bound b))))
(define (sub-interval a b)
  (add-interval a
                (make-interval (- (upper-bound b))
                               (- (lower-bound b)))))
(define (mul-interval x y)
  (if (or (< (* (lower-bound x) (upper-bound x)) 0)
          (< (* (lower-bound y) (upper-bound y)) 0))
      (print "mul-interval should not spans zero")
      (let ((p1 (* (lower-bound x) (lower-bound y)))
            (p2 (* (lower-bound x) (upper-bound y)))
            (p3 (* (upper-bound x) (lower-bound y)))
            (p4 (* (upper-bound x) (upper-bound y))))
        (make-interval (min p1 p2 p3 p4)
                       (max p1 p2 p3 p4)))))

(print (lower-bound (make-interval 1 3)))
(print (upper-bound (make-interval 1 3)))
(print (lower-bound (make-interval 3 1)))
(print (upper-bound (make-interval 3 1)))
(print (mul-interval (make-interval -1 1) (make-interval 0 4)))
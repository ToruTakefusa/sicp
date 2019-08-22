(define (make-interval a b)
  (cons a b))

(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))

(define (make-center-percent center percentage)
  (make-interval (- center (* center percentage))
                 (+ center (* center percentage))))

(define (upper-bound interval)
  (max (car interval) (cdr interval)))

(define (lower-bound interval)
  (min (car interval) (cdr interval)))

(define (percent interval)
  (/ (- (upper-bound interval) (center interval)) (center interval)))

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (lower-bound y))))
(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (div-interval x y)
  (mul-interval x (make-interval (/ 1.0 (upper-bound y))
                                 (/ 1.0 (lower-bound y)))))

(define (par1 r1 r2)
  (div-interval (mul-interval r1 r2)
                (add-interval r1 r2)))

(define (par2 r1 r2)
  (let ((one (make-interval 1 1)))
    (div-interval one (add-interval (div-interval one r1)
                                    (div-interval one r2)))))

(print (par1 (make-center-percent 10 0.1) (make-center-percent 50 0.1)))
(print (par2 (make-center-percent 10 0.1) (make-center-percent 50 0.1)))
(print (par1 (make-interval 5 15) (make-center-percent 5 15)))
(print (par2 (make-center-percent 5 15) (make-center-percent 5 15)))
(print (div-interval (make-interval 5 15) (make-interval 5 15)))
(print (div-interval (make-interval 1 1) (make-interval 5 15)))

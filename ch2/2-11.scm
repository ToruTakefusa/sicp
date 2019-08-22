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

(print (make-center-percent 10 0.1))
(print (percent (make-interval 9.0 11.0)))

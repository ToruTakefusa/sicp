(define dx 0.000001)

(define (smooth f)
  (lambda (x) (* (/ 1 3) (+ (f (- x dx)) (f x) (f (+ x dx))))))

(define (repeat f n)
  (define (compose f g) (lambda (n) (f (g n))))
  (if (< n 1)
      (lambda (x) x)
      (compose f (repeat f (- n 1)))))

(define (n-foild-smooth f n)
  (repeat smooth n) f)





(define (cubic a b c)
  (lambda (x)
    (+ (cube x)
       (* a (square x))
       (* b x)
       c)))

(define (cube x) (* x x x))
(define (square x) (* x x))

(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess))

(define (newtons-transform g)
  (lambda (x)
    (- x (/ (g x) ((deriv g) x)))))

(define (deriv g)
  (lambda (x)
    (/ (- (g (+ x dx)) (gx))
       dx)))

(define tolerance 0.00001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2) tolerance)))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))


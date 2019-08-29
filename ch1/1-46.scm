(define (iterate-improve improve close-enough?)
  (define (iter guess)
    (let ((next (improve guess)))
      (if (good-enough? guess next)
          next
          (iter next))))
  (lambda(x) (iter x)))

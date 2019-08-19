(define (cont-frac n d k)
  (define (iter i result)
    (cond ((< i 1) result)
          ((= i k) (iter (- i 1) (/ (n k) (d k))))
          (else (iter (- i 1) (/ (n k) (+ (d k) result))))))
  (iter k 0)
  )

(define (tan-cf x k)
  (cont-frac (lambda (i) (if (= i 1) x (- (* x x))))
             (lambda (i) (- (* i 2) 1))
             k))

(print (exact->inexact(tan-cf 1 100)))

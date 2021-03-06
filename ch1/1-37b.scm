(define (cont-frac n d k)
  (define (iter i)
    (cond ((< i 1) 0)
          (else (/ (d k) (+ (d k) (iter (- i 1)))))))
  (iter k)
  )

(print (cont-frac (lambda (i) 1.0)
                  (lambda (i) 1.0)
                  10))
(print (cont-frac (lambda (i) 1.0)
                  (lambda (i) 1.0)
                  11))

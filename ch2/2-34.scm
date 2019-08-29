(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms)
                (+ (* x higher-terms) this-coeff))
              0
              coefficient-sequence))

(define nil `())

(print (horner-eval 2 (list 1 3 0 5 0 1)))

(define y (list (list 1 2) (list 3 4)))
(define z (cons (list 1 2) (list 3 4)))

(print y)

(print (car y))
(print (cdr y))
(print (car (cdr y)))

(print z)
(print (car z))
(print (cdr z))
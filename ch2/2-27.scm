(define (deep-reverse items)
  (define (iter items result)
    (cond ((null? items) result)
          ((pair? (car items)) (iter
                                (cdr items)
                                (cons (iter (car items) nil) result)))
          (else (iter (cdr items) (cons (car items) result)))))
  (iter items nil))

(define nil `())

(define x (list (list 1 2) (list 3 4)))
(print (deep-reverse x))
(print (deep-reverse (list 1 3 (list 5 7))))
(print (deep-reverse (list 1 2 3 (list 4 5))))

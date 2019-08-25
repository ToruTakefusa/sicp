(define (reverse items)
  (define (iter items result)
    (if (null? items)
        result
        (iter (cdr items) (cons (car items) result))))
  (iter items nil))

(define (fringe items)
  (define (iter items result)
    (cond ((null? items)  result)
          ((pair? (car items)) (iter
                                (cdr items)
                                (append (iter (car items) nil) result)))
          (else (iter (cdr items) (append (cons (car items) nil) result)))))
  (reverse(iter items nil)))

(define nil `())

(define x (list (list 1 2) (list 3 4)))
(print (fringe x))
(print (fringe (list 1 3 (list 5 7))))
(print (fringe (list 1 2 3 (list 4 5))))
(print (fringe (list x x)))

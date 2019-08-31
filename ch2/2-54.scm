(define (equal? a b)
  (cond ((not (or (and (not (pair? a)) (not (pair? b)))
                  (and (pair? a) (pair? b))))
         #f)
        ((or (eq? a b)
             (and (equal? (car a) (car b)) (equal? (cdr a) (cdr b))))
         #t)
        (else #f)))

(print (equal? `(this is a list) `(this is a list)))
(print (equal? `(this is a list) `(this (is a) list)))

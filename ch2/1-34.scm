(define (f g) (g 2))

(print (f square))
(print (f (lambda (z) ( * z (+ z 1)))))
(print (f f))

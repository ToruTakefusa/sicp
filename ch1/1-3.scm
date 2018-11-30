(define (square x)
  * x x)

(define (sum_of_square a b)
  + (square a) (square b))

(define (sum_of_square_bigger_two a b c)
  (cond (and (>= a b) (>= b c))(sum_of_square a b)
		(and (>= b a) (>= a c))(sum_of_square b a)
		(and (>= c b) (>= b a))(sum_of_square c b)
		))
  

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      nil
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))

(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(define (matrix-*-vector m v)
  (map (lambda(row)
         (dot-product row v)) m))

(define nil `())

(define (transpose mat)
  (accumulate-n cons nil mat))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (row)
           (map (lambda (col) (dot-product row col))
                cols))
         m)))

(print (dot-product (list 1 2 3) (list 4 5 6)))
(print (matrix-*-vector (list (list 1 2 3) (list 4 5 6))
                        (list 1 2 3)))
(print (transpose (list (list 1 2) (list 3 4))))
(print (transpose (list (list 1 2 3) (list 4 5 6) (list 7 8 9))))
(print (matrix-*-matrix (list (list 1 2) (list 3 4)) (list (list 5 6) (list 7 8))))

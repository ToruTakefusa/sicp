(define list1 (list  (list 1 2 3) (list 4 5 6) (list 7 8 9) (list 10 11 12)))

(print list1)
(print (car list1))
(print (map car list1))
(print (map cdr list1))

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
(define nil `())

(print (accumulate-n + 0 list1))

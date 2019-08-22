(define (list-ref items n)
  (if (= n 0)
      (car items)
      (list-ref (cdr items) (- n 1))))

(define (length items)
  (define (length-iter a count)
    (if (null? a)
        count
        (length-iter (cdr a) (+ 1 count))))
  (length-iter items 0))

(define (last-pair list)
  (define (last-pair-iter n length list1)
    (if (= n (- length 1))
        list1
        (last-pair-iter (+ n 1) length (cdr list1))))
  (last-pair-iter 0 (length list) list))

(define nil `())


(print (list (list-ref (list 1 2 3 4) 3)))
(print (length (list 1 2 3 4)))
(print (last-pair (list 23 72 149 34)))

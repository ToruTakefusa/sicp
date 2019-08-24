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
  (define (iter n length list1)
    (if (= n (- length 1))
        list1
        (iter (+ n 1) length (cdr list1))))
  (iter 0 (length list) list))

(define (append list1 list2)
  (if (null? list1)
      list2
      (cons (car list1) (append (cdr list1) list2))))

(define (reverse list)
  (define (reverse-iter n result)
    (if (< n 0)
        result
        (reverse-iter (- n 1)
                      (append result (cons (list-ref list n) nil)))))
  (reverse-iter (- (length list) 1) nil))

(define nil `())

(define (same-parity first . items)
  (let ((yes? (if (odd? first)
                  odd?
                  even?)))
    (define (iter list result)
      (if (null? list)
          (reverse result)
          (iter (cdr list) (if (yes? (car list))
                                (cons (car list) result)
                                result))))
    (iter items (list first))))

(define (even? n) (= (remainder n 2) 0))
(define (odd? n) (= (remainder n 2) 1))

(print (same-parity 1 2 3 4 5 6 7))
(print (same-parity 2 3 4 5 6 7))

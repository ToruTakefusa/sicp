(define list1 (list 1 3 (list 5 7) 9))
(print list1)
(print (cdr (cdr list1)))
(print (car (cdr (cdr list1))))
(print (cdr (car (cdr (cdr list1)))))
(print (car (cdr (car (cdr (cdr list1))))))

(define list2 (list (list 7)))
(print list2)
(print (car (car list2)))

(define list3 (list 1 (list 2 (list 3 (list 4 (list 5 (list 6 7)))))))
(print list3)

(print (cdr list3))
(print (car (cdr list3)))
(print (car (cdr
             (car (cdr
                   (car (cdr
                         (car (cdr
                               (car (cdr
                                     (car (cdr list3)))))))))))))

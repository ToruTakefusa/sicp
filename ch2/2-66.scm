(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right) (list entry left right))


(define (lookup given-key tree-of-records)
  (cond ((null? tree-of-records) #f)
        ((= given-key (entry tree-of-records)) (entry tree-of-records))
        ((< given-key (entry tree-of-records))
         (lookup given-key (left-branch tree-of-records)))
        ((> given-key (entry tree-of-records))
         (lookup given-key (right-branch tree-of-records)))))

(define nil `())

(define tree1
  (make-tree 7
             (make-tree 3
                        (make-tree 1 nil nil)
                        (make-tree 5 nil nil))
             (make-tree 9
                        nil
                        (make-tree 11 nil nil))))


(print (lookup 5 tree1))
(print (lookup 111 tree1))

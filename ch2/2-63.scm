(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
  (list entry left right))

(define (tree->list-1 tree)
  (if (null? tree)
      `()
      (append (tree->list-1 (left-branch tree))
              (cons (entry tree)
                    (tree->list-1 (right-branch tree))))))

(define (tree->list-2 tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
        result-list
        (copy-to-list (left-branch tree)
                      (cons (entry tree)
                            (copy-to-list (right-branch tree)
                                          result-list)))))
  (copy-to-list tree `()))

(define nil `())

(define tree1
  (make-tree 7
             (make-tree 3
                        (make-tree 1 nil nil)
                        (make-tree 5 nil nil))
             (make-tree 9
                        nil
                        (make-tree 11 nil nil))))

(define tree2
  (make-tree 3
             (make-tree 1 nil nil)
             (make-tree 7
                        (make-tree 5 nil nil)
                        (make-tree 9
                                   nil
                                   (make-tree 11 nil nil)))))

(define tree3
  (make-tree 5
             (make-tree 3
                        (make-tree 1 nil nil)
                        nil)
             (make-tree 9
                        (make-tree 7 nil nil)
                        (make-tree 11 nil nil))))

(print tree1)
(print (tree->list-1 tree1))
(print (tree->list-2 tree1))
(print tree2)
(print (tree->list-1 tree2))
(print (tree->list-2 tree2))
(print tree3)
(print (tree->list-1 tree3))
(print (tree->list-2 tree3))

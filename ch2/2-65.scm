(define (union-set set1 set2)
    (cond ((null? set1) set2)
          ((null? set2) set1)
          ((and (null? set1) (null? set2)) nil)
          ((= (car set1) (car set2))
           (cons (car set1) (union-set (cdr set1) (cdr set2))))
          ((< (car set1) (car set2))
           (cons (car set1) (union-set (cdr set1) set2)))
          ((> (car set1) (car set2))
           (cons (car set2) (union-set set1 (cdr set2))))))

(define nil `())

(define (intersection-set set1 set2)
  (if (or (null? set1) (null? set2))
      nil
      (let ((x1 (car set1)) (x2 (car set2)))
        (cond ((= x1 x2)
               (cons x1
                     (intersection-set (cdr set1) (cdr set2))))
              ((< x1 x2)
               (intersection-set (cdr set1) set2))
              ((> x1 x2)
               (intersection-set set1 (cdr set2)))))))

(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right) (list entry left right))

(define (tree->list tree)
  (if (null? tree)
      nil
      (append(tree->list (left-branch tree))
             (cons (entry tree)
                   (tree->list (right-branch tree))))))


(define (list->tree elements)
  (car (partial-tree elements (length elements))))

(define (partial-tree elts n)
  (if (= n 0)
      (cons nil elts)
      (let ((left-size (quotient (- n 1) 2)))
        (let ((left-result (partial-tree elts left-size)))
          (let ((left-tree (car left-result))
                (non-left-elts (cdr left-result))
                (right-size (- n (+ left-size 1))))
            (let ((this-entry (car non-left-elts))
                  (right-result (partial-tree (cdr non-left-elts) right-size)))
                  (let ((right-tree (car right-result))
                        (remaining-elts (cdr right-result)))
                    (cons (make-tree this-entry left-tree right-tree)
                          remaining-elts))))))))


(define (union-set-binary-tree tree1 tree2)
  (cond ((null? tree1) tree2)
        ((null? tree2) tree1)
        (else (list->tree (union-set (tree->list tree1) (tree->list tree2))))))

(define (intersection-binary-tree tree1 tree2)
  (cond ((null? tree1) tree2)
        ((null? tree2) tree1)
        (else (list->tree (intersection-set
                           (tree->list tree1) (tree->list tree2))))))
(define tree1
  (make-tree 7
             (make-tree 3
                        (make-tree 1 nil nil)
                        (make-tree 5 nil nil))
             (make-tree 9
                        nil
                        (make-tree 11 nil nil))))
(define tree2
  (make-tree 7
             (make-tree 2
                        (make-tree 1 nil nil)
                        (make-tree 5 nil nil))
             (make-tree 10
                        nil
                        (make-tree 11 nil nil))))

(print (union-set-binary-tree tree1 tree2))
(print (intersection-binary-tree tree1 tree2))



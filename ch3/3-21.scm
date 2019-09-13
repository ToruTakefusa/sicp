(define (front-ptr queue) (car queue))
(define (rear-ptr queue) (cdr queue))
(define (set-front-ptr! queue item) (set-car! queue item))
(define (set-rear-ptr! queue item) (set-cdr! queue item))
(define (empty-queue? queue) (null? (front-ptr queue)))
(define (make-queue) (cons `() `()))

(define (front-queue queue)
  (if (empty-queue? queue)
      (error "FRONT called with an empty queue" queue)
      (car (front-ptr queue))))

(define (insert-queue! queue item)
  (let ((new-pair (cons item `())))
    (cond ((empty-queue? queue)
           (set-front-ptr! queue new-pair)
           (set-rear-ptr! queue new-pair)
           queue)
          (else
           (set-cdr! (rear-ptr queue) new-pair)
           (set-rear-ptr! queue new-pair)
           queue))))

(define (delete-queue! queue)
  (cond ((empty-queue? queue)
         (error "DELETE! called with an empty queue" queue))
        (else
         (set-front-ptr! queue (cdr (front-ptr queue)))
         queue)))

(define (print-queue queue)
  (define (iter rest)
    (cond ((null? rest) (newline))
          (else
           (print (car rest))
           (iter (cdr rest)))))
  (iter (front-ptr q2)))

(define q1 (make-queue))
(print (insert-queue! q1 `a))
(print (insert-queue! q1 `b))
(print (delete-queue! q1))
(print (delete-queue! q1))

(define q2 (make-queue))
(print-queue q2)
(print-queue (insert-queue! q2 `a))
(print-queue (insert-queue! q2 `b))
(print-queue (delete-queue! q2))
(print-queue (delete-queue! q2))




(define (make-monitored f)
  (define how-many-calls? 0)
  (define (mf message)
    (cond ((eq? message `how-many-calls?) how-many-calls?)
          ((eq? message `reset-count) (set! how-many-calls? 0))
          (else (set! how-many-calls? (+ how-many-calls? 1))
                (f message))))
 mf)

(define s (make-monitored sqrt))

(print (s 100))
(print (s `how-many-calls?))
(print (s 25))
(print (s `how-many-calls?))
(print (s `reset-count))
(print (s `how-many-calls?))

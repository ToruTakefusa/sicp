(define-syntax delay
  (syntax-rules ()
    ((_ exp) (memo-proc (lambda () exp)))))
(define (memo-proc proc)
  (let ((already-run? #f) (result #f))
    (lambda()
      (if (not already-run?)
          (begin (set! result (proc))
                 (set! already-run? #t)
                 result)
          result))))
(define-syntax cons-stream
  (syntax-rules ()
    ((_ a b) (cons a (delay b)))))

(define (stream-filter pred stream)
  (cond ((stream-null? stream) the-empty-stream)
        ((pred (stream-car stream))
         (cons-stream (stream-car stream)
                      (stream-filter pred (stream-cdr stream))))
        (else (stream-filter pred (stream-cdr stream)))))
(define (stream-car stream) (car stream))
(define (stream-cdr stream) (force (cdr stream)))
(define (force delayed-object)
  (delayed-object))
(define the-empty-stream `())
(define stream-null? null?)
(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))
(define (stream-map proc . argstreams)
  (if (stream-null? (car argstreams))
      the-empty-stream
      (cons-stream (apply proc (map stream-car argstreams))
                   (apply stream-map (cons proc (map stream-cdr argstreams))))))
(define (stream-for-each proc s)
  (if (stream-null? s)
      `done
      (begin (proc (stream-car s))
             stream-for-each proc (stream-cdr s))))
(define (display-stream s)
  (stream-for-each display-line s))
(define (display-line x)
  (newline)
  (display x))
(define (stream-enumerate-interval low high)
  (if (> low high)
      the-empty-stream
      (cons-stream
       low
       (stream-enumerate-interval (+ low 1) high))))
(define (show x)
  (display-line x)
  x)

(define (add-streams s1 s2)
  (stream-map + s1 s2))

(define (mul-streams s1 s2)
  (stream-map * s1 s2))

(define factorials (cons-stream 1
                                (mul-streams factorials
                                            (add-streams ones integers))))

(define ones (cons-stream 1 ones))
(define integers (cons-stream 1 (add-streams ones integers)))
(define (partial-sums s)
  (cons-stream (stream-car s)
               (add-streams (stream-cdr s) (partial-sums s))))

(define foo (partial-sums integers))
(print (stream-car foo))
(print (stream-car (stream-cdr foo)))
(print (stream-car (stream-cdr (stream-cdr foo))))
(print (stream-car (stream-cdr (stream-cdr (stream-cdr foo)))))

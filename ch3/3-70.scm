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
(define (scale-stream stream factor)
  (stream-map (lambda(x) (* x factor) stream)))
(define factorials (cons-stream 1
                                (mul-streams factorials
                                            (add-streams ones integers))))
(define ones (cons-stream 1 ones))
(define integers (cons-stream 1 (add-streams ones integers)))
(define (integrate-series s)
  (mul-streams s (stream-map / ones integers)))

(define (invert-unit-series s)
  (cons-stream 1
               (mul-series s
                           (scale-stream (invert-unit-series s) -1))))
(define (mul-series s1 s2)
  (cons-stream (* (stream-car s1) (stream-car s2))
               (add-streams (scale-stream (stream-cdr s2) (stream-car s1))
                            (mul-series (stream-cdr s1) s2))))
(define (div-series s1 s2)
  (if (= (stream-car s2) 0)
      (error" constant term s2 can't be 0")
      (mul-series s1 (invert-unit-series s2))))
(define (stream-limit s stolerance)
  (let ((now (stream-car s))
        (next (stream-car (stream-cdr s))))
    (if (< (abs (- now next)) tolerance)
        next
        (stream-limit (stream-cdr s) tolerance))))
(define (partial-sums s)
  (add-streams s (cons-stream 0 (partial-sums s))))
(define (pi-summands n)
  (cons-stream (/ 1.0 n)
               (stream-map - (pi-summands (+ n 2)))))
(define (pairs s t)
  (cons-stream (list (stream-car s) (stream-car t))
               (interleave
                (interleave
                 (stream-map (lambda(x) (list (stream-car s) x))
                             (stream-cdr t))
                 (pairs (stream-cdr s) (stream-cdr t)))
                (stream-map (lambda(x) (list x (stream-car t)))
                            (stream-cdr s)))))
(define (interleave s1 s2)
  (if (stream-null? s1)
      s2
      (cons-stream (stream-car s1)
                   (interleave s2 (stream-cdr s1)))))
(define (merge-weight s1 s2 weight)
  (cond ((stream-null? s1) s2)
        ((stream-null? s2) s1)
        (else
         (let ((s1car (stream-car s1))
               (s2car (stream-car s2)))
           (cond ((< (weight s1car) (weight s2car)
                     (cons-stream s1car (merge-weight (stream-cdr s1) s2))))
                 ((> (weight s1car) (weight s2car)
                     (cons-stream s2car (merge-weight s1 (stream-cdr s2)))))
                 (else
                  (cons-stream s1car (merge-weight (stream-cdr s1)
                                                   (stream-cdr s2)))))))))
(define (weighted-pairs s t weight)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (merge-weight (stream-map (lambda (x) (list (stream-car s x)))(stream-cdr t))
              (weighted-pairs (stream-cdr s) (stream-cdr t) weight) weight)))

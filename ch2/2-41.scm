(define (filter predicate sequence)
  (cond ((null? sequence) nil)
        ((predicate (car sequence))
         (cons (car sequence) (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (enumerate-interval low high)
  (if (> low high)
      nil
      (cons low (enumerate-interval (+ low 1) high))))


(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))

(define (make-triple triple)
  (list (car triple) (cadr triple) (caddr triple)))

(define (unique-triples n)
  (flatmap (lambda(i)
             (flatmap (lambda (j)
                    (map (lambda(k) (list i j k))
                        (enumerate-interval 1 (- j 1))))
                    (enumerate-interval 1 (- i 1))))
       (enumerate-interval 1 n)))

(define nil `())

(define (n-sum-triples n s)
  (define (n-sum? triples)
    (= (+ (car triples) (cadr triples) (caddr triples)) s))

  (map make-triple
       (filter n-sum?
               (unique-triples n))))

(print (unique-triples 6))
(print (n-sum-triples 6 6))

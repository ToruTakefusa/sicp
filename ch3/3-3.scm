(define (make-account balance secret-password)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amnount))
    balance)
  (define (dispatch password m)
    (cond ((not (eq? password secret-password)) (lambda(x) "Incorrect Password"))
          ((eq? m `withdraw) withdraw)
          ((eq? m `deposit) deposit)
          (else (error "Unknown request -- MAKE-ACCOUNT" m))))
  dispatch)

(define acc (make-account 100 `secret-password))

(print ((acc `secret-password `withdraw) 40))
(print ((acc `some-other-password `deposit) 50))

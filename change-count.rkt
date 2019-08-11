#lang typed/racket

(: coins (Listof Integer))
(define coins '(50 25 10 5 1))

(: cash Integer)
(define cash 100)

(: change-count (-> Integer (Listof Integer) Integer))
(define (change-count cash coins)
  (cond
    [(= cash 0) 1]
    [(< cash 0) 0]
    [(empty? coins) 0]
    [(= (length coins) 1) 1]
    [else (+ (change-count cash (cdr coins))
             (change-count (- cash (car coins)) coins))]
    ))

(module+ test

  (require typed/rackunit typed/rackunit/text-ui)

  (check-equal? (change-count cash coins) 292)

  )

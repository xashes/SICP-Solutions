#lang racket

(define (sum func next a b)
  (cond
    [(> a b) 0]
    [else
     (+ (func a)
        (sum func next (next a) b))]))


(module+ test

  (require rackunit rackunit/text-ui)

  (check-equal? (sum (lambda (x) (+ x 1)) add1 0 2) 6)

  )

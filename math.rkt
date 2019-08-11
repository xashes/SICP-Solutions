#lang racket

(provide (contract-out
          [gcd (-> integer? integer? integer?)]))

(define (gcd a b)
  (if (zero? b)
      a
      (gcd b (remainder a b))))


(module+ test

  (require rackunit rackunit/text-ui)

  (check-equal? (gcd 6 9) 3)

  )

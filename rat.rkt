#lang racket

(require "math.rkt")

(provide (contract-out
          [rat (-> integer? (and/c integer? (not/c zero?)) pair?)]))

;; rat : Int Int -> Rat
;; Rat ::= (Int . Int)
(define (rat numer denom)
  (let ([g (gcd denom numer)])
    (cons (/ numer g) (/ denom g))))

;; numer : Rat -> Int
(define (rat-numer rat)
  (car rat))

;; denom : Rat -> Int
(define (rat-denom rat)
  (cdr rat))

;; print-rat
(define (print-rat rat)
  (format "~a/~a" (rat-numer rat) (rat-denom rat))
  )

;; op-rat : Rat Rat -> Rat
(define (add-rat r1 r2)
  (rat (+ (* (rat-numer r1)
             (rat-denom r2))
          (* (rat-numer r2)
             (rat-denom r1)))
       (* (rat-denom r1)
          (rat-denom r2))))

(define (sub-rat r1 r2)
  (rat (- (* (rat-numer r1)
             (rat-denom r2))
          (* (rat-numer r2)
             (rat-denom r1)))
       (* (rat-denom r1)
          (rat-denom r2))))

(define (mul-rat r1 r2)
  (rat (* (rat-numer r1) (rat-numer r2))
       (* (rat-denom r1) (rat-denom r2))))

(define (div-rat r1 r2)
  (rat (* (rat-numer r1) (rat-denom r2))
       (* (rat-numer r2) (rat-denom r1))))

(define (equal-rat? r1 r2)
  (= (* (rat-numer r1) (rat-denom r2))
     (* (rat-numer r2) (rat-denom r1))))

(module+ test

  (require rackunit rackunit/text-ui)

  (let ([r1 (rat 1 2)]
        [r2 (rat 3 5)]
        [r3 (rat 2 15)]
        )
    (check-equal? (rat-numer r1) 1)
    (check-equal? (rat-denom r1) 2)

    (check-equal? (add-rat r1 r2) (rat 11 10))
    (check-equal? (add-rat r2 r3) (rat 11 15))
    (check-equal? (sub-rat r1 r2) (rat -1 10))
    (check-equal? (mul-rat r1 r2) (rat 3 10))
    (check-equal? (div-rat r1 r2) (rat 5 6))
    (check-true (equal-rat? (rat 1 2) (rat 3 6)))
    )

  )

#lang typed/racket

(: triangle-element (-> Integer Integer Integer))
(define (triangle-element row col)
  (if (or (zero? col)
          (= row col))
      1
      (+ (triangle-element (sub1 row) (sub1 col))
         (triangle-element (sub1 row) col))
      ))

(: triangle-row (-> Integer (Listof Integer)))
(define (triangle-row row)
  (for/list ([col (in-range (add1 row))])
    (triangle-element row col)
    ))

(: triangle (-> Integer (Listof (Listof Integer))))
(define (triangle row)
  (for/list ([r (in-range (add1 row))])
    (triangle-row r)))

(module+ test

  (require typed/rackunit typed/rackunit/text-ui)

  (check-equal? (triangle-element 2 1) 2)
  (check-equal? (triangle-element 3 1) 3)
  (check-equal? (triangle-element 4 3) 4)

  (check-equal? (triangle-row 2) '(1 2 1))
  (check-equal? (triangle 2) '((1) (1 1) (1 2 1)))

  )

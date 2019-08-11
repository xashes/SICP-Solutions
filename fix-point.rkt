#lang racket
(require math/statistics)

(define tolerance 0.00001)
(define (fix-point f guess)
  (define (close-enough? guess next)
    (< (abs (- next guess)) tolerance))
  (let ([next (f guess)])
    (if (close-enough? guess next)
        next
        (fix-point f next)))
  )

(define (gold-split)
  (fix-point (lambda (x) (+ 1 (/ 1 x))) 1.2))

(gold-split)

(define (sqrt-fix x)
  (fix-point (lambda (y) (mean (list y (/ x y)))) 1.0)
  )

(sqrt-fix 2)
(sqrt-fix 9)

#lang racket

(define (cons x y)
  (lambda (f) (f x y)))

(define (car p)
  (p (lambda (x y) x)))

(define p (cons 2 6))
(display p)
(newline)
(display (car p))

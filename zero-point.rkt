#lang racket

(define (search func a b)
  (define (good-enough? m n)
    (< (abs (- m n)) 0.001))
  (let* ([m (/ (+ a b) 2)]
        [tv (func m)])
    (if (good-enough? a b)
        m
        (cond [(> tv 0)
                (search func a tv)]
              [(< tv 0)
               (search func tv b)]
              [else m]))))

(define (zero-point func a b)
  (let ([fa (func a)]
        [fb (func b)])
    (cond
      [(and (< fa 0) (> fb 0)) (search func a b)]
      [(and (< fb 0) (> fa 0)) (search func b a)]
      [else (error "f(a) and f(b) are not opposite sign" a b)]
      )))


(module+ test

  (require rackunit rackunit/text-ui)

  ;; (check-equal? (zero-point (lambda (x) (- x 1)) 0 8) 1)
  ;; (check-equal? (zero-point (lambda (x) (+ x 1)) -2 8) 1)

  )

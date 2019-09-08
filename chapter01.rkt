#lang racket
(require plot)

(module+ test
  (require rackunit rackunit/text-ui)
  )

;; example
;; coin exchange
(define/contract (count-change amount coin-lst)
  (-> integer? (listof integer?) integer?)
  (cond
    [(zero? amount) 1]
    [(< amount 0) 0]
    [(empty? coin-lst) 0]
    [else (+ (count-change amount (rest coin-lst))
             (count-change (- amount (first coin-lst))
                           coin-lst))]
    )
  )
(module+ test
  (check-equal? (count-change 6 '(5 1))
                2)
  (check-equal? (count-change 100 '(50 25 10 5 1))
                292)
  )

;; exercise 1.11
(define/contract (f1.11 n)
  (-> integer? integer?)
  (if (< n 3)
      n
      (+ (f1.11 (sub1 n))
         (* 2 (f1.11 (- n 2)))
         (* 3 (f1.11 (- n 3)))))
  )

;; iterate version
(define/contract (f1.11-iter n)
  (-> integer? integer?)
  (if (< n 3)
      n
      (let iter-helper ([n-3 0]
                        [n-2 1]
                        [n-1 2]
                        [i n])
        (if (= i 2)
            n-1
            (iter-helper n-2
                         n-1
                         (+ n-1
                            (* 2 n-2)
                            (* 3 n-3))
                         (sub1 i)))))
  )

(module+ test
  (define (plot-f1.11)
    (plot (list (lines (for/list ([i (in-range -3 10)])
                         (vector i (f1.11 i))
                         )
                       #:label "f1.11")
                (lines (for/list ([i (in-range -3 10)])
                         (vector i (f1.11-iter i))
                         )
                       #:label "f1.11-iter"
                       #:color 'blue
                       #:style 'short-dash))))
  (for ([i (in-range -3 6)])
    (check-equal? (f1.11 i) (f1.11-iter i)))
  )
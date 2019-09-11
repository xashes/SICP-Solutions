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

;; ierative version
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

;; ierative version using for/fold
(define/contract (f1.11-iter2 n)
  (-> integer? integer?)
  (if (< n 3)
      n
      (for/fold ([fn-3 0]
                 [fn-2 1]
                 [fn-1 2]
                 #:result fn-1)
                ([i (in-range (- n 2))])
        (values
         fn-2
         fn-1
         (+ fn-1
            (* 2 fn-2)
            (* 3 fn-3)))))
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
  (for ([i (in-range -3 6)])
    (check-equal? (f1.11-iter i) (f1.11-iter2 i)))
  )


;; exercise 1.12
;; yang hui triangle
(define/contract (yh-triangle row col)
  (-> positive-integer? positive-integer? positive-integer?)
  (if (or (= col 1)
          (= row col))
      1
      (+ (yh-triangle (sub1 row) (sub1 col))
         (yh-triangle (sub1 row) col)))
  )
(module+ test
  (check-equal? (yh-triangle 1 1) 1)
  (check-equal? (yh-triangle 3 3) 1)
  (check-equal? (yh-triangle 3 2) 2)
  (check-equal? (yh-triangle 4 1) 1)
  (check-equal? (yh-triangle 4 3) 3)
  )

;; 1.2.4 expt
;; recursive version
(define/contract (expt-rec b n)
  (-> real? nonnegative-integer? real?)
  (if (zero? n)
      1
      (* b (expt-rec b (sub1 n))))
  )
(module+ test
  (for ([b (in-range -1 1 0.5)]
        [n (in-range 4)])
    (check-equal? (expt-rec b n)
                  (expt b n)))
  )

;; ierative version
(define/contract (expt-iter b n)
  (-> real? nonnegative-integer? real?)
  (let iter ([counter n]
             [product 1])
    (if (zero? counter)
        product
        (iter (sub1 counter) (* product b))))
  )
(module+ test
  (for ([b (in-range -1 1 0.5)]
        [n (in-range 4)])
    (check-equal? (expt-iter b n)
                  (expt b n)))
  )

;; for/fold version
(define/contract (expt-for b n)
  (-> real? nonnegative-integer? real?)
  (for/fold ([product 1])
            ([i (in-range n)])
    (* b product)))
(module+ test
  (for ([b (in-range -1 1 0.5)]
        [n (in-range 4)])
    (check-equal? (expt-for b n)
                  (expt b n)))
  )

;; fast recursive version
(define/contract (fast-expt-rec b n)
  (-> real? nonnegative-integer? real?)
  (cond
    [(zero? n) 1]
    [(even? n) (sqr (fast-expt-rec b (/ n 2)))]
    [(odd? n) (* b (fast-expt-rec b (sub1 n)))])
  )
(module+ test
  (for ([b (in-range -1 1 0.5)]
        [n (in-range 4)])
    (check-equal? (fast-expt-rec b n)
                  (expt b n)))
  )

;; fast iterative version
(define/contract (fast-expt-iter b n)
  (-> real? nonnegative-integer? real?)
  (let iter ([base b]
             [counter n]
             [result 1])
    (cond
      [(zero? counter) result]
      [(even? counter) (iter (sqr base) (/ counter 2) result)]
      [(odd? counter) (iter base (sub1 counter) (* result base))]))
  )
(module+ test
  (for ([b (make-list 8 2)]
        [n (in-range 8)])
    (check-equal? (fast-expt-iter b n)
                  (expt b n)))
  )

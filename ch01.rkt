#lang racket
(require rackunit)

(define (count-change amount kinds-of-coins)
  (define (first-denomination)
    (cond ([= kinds-of-coins 1] 1)
          ([= kinds-of-coins 2] 5)
          ([= kinds-of-coins 3] 10)
          ([= kinds-of-coins 4] 25)
          ([= kinds-of-coins 5] 50)
          ))
  (cond ([= amount 0] 1)
        ([< amount 0] 0)
        ([= kinds-of-coins 0] 0)
        (else
         (+ (count-change (- amount (first-denomination)) kinds-of-coins)
            (count-change amount (- kinds-of-coins 1)))))
  )

(check-equal? (count-change 100 5) 292)

(define (f-recur n)
  (cond [(< n 3) n]
        [else (+ (f-recur (- n 1))
                 (* 2
                    (f-recur (- n 2)))
                 (* 3
                    (f-recur (- n 3))))]
        ))

(check-equal? (f-recur 0) 0)
(check-equal? (f-recur 3) 4)
(check-equal? (f-recur 4) 11)
(check-equal? (f-recur 5) 25)

(define (f n)
  (define (f-iter f-3 f-2 f-1 n)
    (cond [(< n 2) n]
          [(= n 2) f-1]
          [else (f-iter f-2 f-1 (+ f-1
                                   (* 2 f-2)
                                   (* 3 f-3)) (- n 1))]
          )
    )
  (f-iter 0 1 2 n))

(check-equal? (f 0) 0)
(check-equal? (f 3) 4)
(check-equal? (f 4) 11)
(check-equal? (f 5) 25)

(define (jiaxian-triangle row col)
  (if (or (= col 1) (= col row))
      1
      (+ (jiaxian-triangle (- row 1) (- col 1))
         (jiaxian-triangle (- row 1) col))))

(check-equal? (jiaxian-triangle 1 1) 1)
(check-equal? (jiaxian-triangle 4 3) 3)
(check-equal? (jiaxian-triangle 5 3) 6)
(check-equal? (jiaxian-triangle 5 5) 1)

(define (factorial-recursive n)
  (if (= n 1)
      1
      (* n
         (factorial-recursive (- n 1)))))

(check = (factorial-recursive 1) 1)
(check = (factorial-recursive 2) 2)
(check = (factorial-recursive 3) 6)
(check = (factorial-recursive 6) 720)

(define (factorial-iter n)
  (define (iter product counter max-count)
    (if (> counter max-count)
        product
        (iter (* product counter)
              (+ counter 1)
              max-count)))
  (iter 1 1 n))

(check = (factorial-iter 1) 1)
(check = (factorial-iter 2) 2)
(check = (factorial-iter 3) 6)
(check = (factorial-iter 6) 720)

(define (fib n)
  (cond [(= n 0) 0]
        [(= n 1) 1]
        [else (+ (fib (- n 1))
                 (fib (- n 2)))]))

(check-equal? (fib 0) 0)
(check-equal? (fib 1) 1)
(check-equal? (fib 2) 1)
(check-equal? (fib 3) 2)

(define (fib-iter n)
  (define (iter f-2 f-1 n)
    (if (= n 0)
        f-2
        (iter f-1 (+ f-2 f-1) (- n 1))))
  (iter 0 1 n))

(check-equal? (fib-iter 0) 0)
(check-equal? (fib-iter 1) 1)
(check-equal? (fib-iter 2) 1)
(check-equal? (fib-iter 3) 2)

(define (expt-recur b n)
  (if (= n 0)
      1
      (* b (expt-recur b (- n 1)))))

(check-equal? (expt-recur 2 3) 8)
(check-equal? (expt-recur -2 3) -8)

(define (expt-iter b n)
  (define (iter product base counter)
    (if (= counter 0)
        product
        (iter (* product base)
              base
              (- counter 1))))
  (iter 1 b n))

(check-equal? (expt-iter 2 3) 8)
(check-equal? (expt-iter -2 3) -8)
(check-equal? (expt-iter -1 0) 1)

(define (fast-expt b n)
  (define (square b)
    (* b b))
  (define (even? n)
    (= (remainder n 2) 0))
  (cond [(= n 0) 1]
        [(even? n) (square (fast-expt b (/ n 2)))]
        [else (* b (fast-expt b (- n 1)))]))

(check-equal? (fast-expt 2 3) 8)
(check-equal? (fast-expt -2 10) 1024)
(check-equal? (fast-expt -1 0) 1)

(define (fast-multi a b)
  (define (double a)
    (+ a a))
  (define (halve a)
    (/ a 2))
  (define (even? a)
    (= (remainder a 2) 0))
  (cond [(= b 0) 0]
        [(even? b) (double (fast-multi a (halve b)))]
        [else (+ a (fast-multi a (- b 1)))])
  )

(check = (fast-multi 5 6) 30)
(check = (fast-multi 5 7) 35)
(check = (fast-multi -9 8) -72)

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(check = (gcd 3 9) 3)
(check = (gcd 128 68) 4)

(define (prime? n)
  (= (smallest-divisor n) n)
  )
(define (smallest-divisor n)
  (define (find-divisor n test-divisor)
    (cond [(> (expt test-divisor 2) n) n]
          [(= (remainder n test-divisor) 0) test-divisor]
          [else (find-divisor n (+ test-divisor 1))])
    )
  (find-divisor n 2))
(define (find-divisor n)
  (filter (lambda (i) (= (remainder n i) 0)) (range 2 (+ 1 (round (sqrt n))))))

(define (expmod base e m)
  (remainder (expt base e) m))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (random 1 n))
  )

(define (fermat-prime? n times)
  (cond [(= times 0) #t]
        [(fermat-test n) (fermat-prime? n (- times 1))]
        [else #f]))

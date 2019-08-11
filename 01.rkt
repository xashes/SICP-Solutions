#lang racket
(require rackunit)
(require math/statistics)

;; exercise 1.3 定义一个过程，以三个数为参数，返回其中较大的两数的平方和
(define (square-sum-of-larger-two a b c)
  (cond
    [(and (< a b) (< a c)) (+ (sqr b) (sqr c))]
    [(and (< b a) (< b c)) (+ (sqr a) (sqr c))]
    [else (+ (sqr a) (sqr b))]
    ))

(check-equal? (square-sum-of-larger-two 0 0 0) 0)
(check-equal? (square-sum-of-larger-two 2 3 4) 25)
(check-equal? (square-sum-of-larger-two 3 1 4) 25)
(check-equal? (square-sum-of-larger-two 3 4 3) 25)

;; exercise 1.7 改进牛顿法求平方根
(define (good-enough? old-guess new-guess)
  (< (abs (- 1 (/ old-guess new-guess)))
     0.001))

(define (sqrt-nt n)
  (define (improve n guess)
    (mean (list guess (/ n guess))))

  (define (sqrt-iter n guess)
    (let ([new-guess (improve n guess)])
      (if (good-enough? guess new-guess)
          guess
          (sqrt-iter n new-guess)))
    )
  (define guess 1.0)
  (sqrt-iter n guess))

(check-= (sqrt-nt 9) 3.0 0.1)


;; exercise 1.8 牛顿法求立方根
(define (cube-root n)
  (define (improve n guess)
    (mean (list guess guess (/ n (sqr guess)))))
  (define (cube-root-iter n guess)
    (let ([new-guess (improve n guess)])
      (if (good-enough? guess new-guess)
          guess
          (cube-root-iter n new-guess))))
  (cube-root-iter n 1.0)
  )

(check-equal? (round (cube-root 27)) 3.0)

(define (croot n)
  (define (improve n guess)
    (mean (list guess guess (/ n (sqr guess)))))
  (let croot-iter ([cube n] [guess 1.0])
    (let ([new-guess (improve cube guess)])
      (if (good-enough? guess new-guess)
          guess
          (croot-iter cube new-guess)))))

(check-equal? (round (croot 27)) 3.0)

;; 1.2.2
;; 以迭代计算过程计算Fibnacci number
(define (fib n)
  (define (fib-iter a b n)
    (if (= n 0)
        a
        (fib-iter b (+ a b) (- n 1))))
  (fib-iter 0 1 n))

(check-equal? (fib 0) 0)
(check-equal? (fib 1) 1)
(check-equal? (fib 2) 1)
(check-equal? (fib 6) 8)

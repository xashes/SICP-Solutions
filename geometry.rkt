#lang typed/racket

(require math)

(struct point ([x : Real] [y : Real]) #:transparent)
(struct segment ([start : point] [end : point]) #:transparent)

(: segment-midpoint (-> segment point))
(define (segment-midpoint sg)
  (let* ([start (segment-start sg)]
         [end (segment-end sg)]
         [x1 (point-x start)]
         [y1 (point-y start)]
         [x2 (point-x end)]
         [y2 (point-y end)]
         )
    (point (mean `(,x1 ,x2))
           (mean `(,y1 ,y2)))
    ))

(module+ test

  (require typed/rackunit typed/rackunit/text-ui)

  (let* ([p1 (point 0 0)]
         [p2 (point 2 2)]
         [s1 (segment p1 p2)])
    (check-equal? (segment-midpoint s1) (point 1 1))
    )

  )

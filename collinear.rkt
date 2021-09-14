#lang scheme
(define (collinear? x1 y1 z1 x2 y2 z2)
  (define k (cond ; calculate coeff.
              ((not (= x2 0)) (/ x1 x2))
              ((not (= y2 0)) (/ y1 y2))
              ((not (= z2 0)) (/ z1 z2)))) ; vectors are not zero vectors, so one of conditions is true
  (if (and (= x1 (* k x2)) (= y1 (* k y2)) (= z1 (* k z2))) #t #f)) ; check v1 = k * v2
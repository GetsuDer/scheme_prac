#lang scheme
(define (task-03-2020 lst)
  (define (average_geome sublst)
    (let loop ((mult 1) (len 0) (curr sublst))
      (if (null? curr)
          (expt mult (/ 1 len))
          (loop (* mult (car curr)) (+ len 1) (cdr curr)))))
  (expt (foldl (lambda (x y) (+ y (* x x))) 0 (map average_geome lst)) (/ 1 2))
  )
 
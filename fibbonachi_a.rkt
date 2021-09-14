#lang scheme
(define (odd-fib-list n)
  (define (odd a) (if (= (remainder a 2) 1) #t #f))
  (define (reverse lst)
    (define (loop lst result)
      (if (null? lst) result
          (loop (cdr lst) (cons (car lst) result))))
    (loop lst '()))
  (if (not (and (natural? n) (> n 0)))
      `()
      (cons 1
      (reverse
        (let loop ((i (- n 1)) (result `()) (first 0) (second 1))
        (
         if (= i 0)
            result
            (loop
             (- i 1)
             (cons (if (odd (+ first second)) (+ first second) (+ first second second)) result)
             (if (odd (+ first second)) second (+ first second))
             (if (odd (+ first second)) (+ first second) (+ first second second))
             )
            
         )
        )
       )
      )
      )
  )
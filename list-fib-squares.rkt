#lang scheme
(define (list-fib-squares-a n)
  (define res
    (cons 1
    (reverse
        (let loop ((i (- n 1)) (result `()) (first 0) (second 1))
        (
         if (= i 0)
            result
            (loop
             (- i 1)
             (cons (+ first second) result)
             second
             (+ first second))
            )
          )
        )
    )
    )
  (map (lambda (x) (* x x))
       res)
  )

(define (list-fib-squares-b n)
  (define result
    (cons 1
          (reverse
           (let loop ((i (- n 1)) (result `()) (first 0) (second 1))
             (
              if (= i 0)
                 result
                 (loop
                  (- i 1)
                  (cons (+ first second) result)
                  second
                  (+ first second))
                 )
             )
           )
          )
    )
  (foldr (lambda (curr rest) (cons (* curr curr) rest)) `() result)
  )

(define (process lst)
  (define first_multiplied (foldl (lambda (curr rest) (if (null? curr) rest (* curr rest))) 1 (car lst)))
  (reverse
  (let loop ((result `()) (curr lst))
    (if (null? curr)
        result
        (loop (if (> (foldl (lambda (curr rest) (if (null? curr) rest (+ curr rest))) 0 (car curr)) first_multiplied) (cons (car curr) result) result)
              (cdr curr))
        )
    )))

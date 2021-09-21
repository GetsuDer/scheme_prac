#lang scheme
(require racket/vector)

; tree lib
(define empty-tree #())
(define make-tree vector)
(define (tree-data tree) (vector-ref tree 0))
(define (tree-left tree) (vector-ref tree 1))
(define (tree-right tree) (vector-ref tree 2))
(define (empty-tree? t) (equal? t #()))

(define  (print-tree-by-level-desc tree)
  (define (print-parentless-list lst)
    (cond ((null? lst) (void))
          (else
           (printf "~a " (car lst))
           (print-parentless-list (cdr lst))
           )
          )
    )

  (define (merge lst1 lst2)
    (cond ((and (null? lst1) (null? lst2)) `())
          ((null? lst1) lst2)
          ((null? lst2) lst1)
          (else (cons (append (car lst1) (car lst2)) (merge (cdr lst1) (cdr lst2))))
          )
    )
  
  (define (print-tree lst-of-lsts)
    (cond ((null? lst-of-lsts) (void))
           (else
            (print-parentless-list (car lst-of-lsts))
            (newline)
            (print-tree (cdr lst-of-lsts))
            )
           )
    )

  (define (print-tree-inner-cps tail cc)
    (cond ((empty-tree? tail) (cc `()))
            (else
             (print-tree-inner-cps
              (tree-right tail)
              (lambda (y)
                (print-tree-inner-cps
                 (tree-left tail)
                 (lambda (z) (cc (if (null? (merge y z)) (list (list (tree-data tail))) (list (list (tree-data tail)) (merge y z)))))
                 )
                )
              )
             )
            )
    )
           
  ; (lambda (y) (cc (list (tree-data tail)) (merge y (print-tree-inner (tree-left tail))
  ; (lambda (z) (cc (list (tree-data tail)) (merge y z)
  ; (lambda (y) (print-tree-inner (tree-left tail) (lambda (z) (cc (list (tree-data tail) (merge y z))
  (print-tree
   (reverse
    (print-tree-inner-cps tree (lambda (x) x))
    )
   )
  (print-tree-inner-cps tree (lambda (x) x))
  )
#lang scheme
(require racket/vector)

; tree lib
(define empty-tree #())
(define make-tree vector)
(define (tree-data tree) (vector-ref tree 0))
(define (tree-left tree) (vector-ref tree 1))
(define (tree-right tree) (vector-ref tree 2))
(define (empty-tree? t) (equal? t #()))

; lecture 4, task 1
(define (task-4-2021 tree h)
  (cond ((= h 0) (empty-tree? tree))
        ((= h 1) (and (= (vector-length tree) 3) (empty-tree? (tree-left tree)) (empty-tree? (tree-right tree))))
        ((< h 0) #f)
        (else (or (and (task-4-2021 (tree-left tree) (- h 1))
                       (task-4-2021 (tree-right tree) (- h 2)))
                  (and (task-4-2021 (tree-left tree) (- h 2))
                       (task-4-2021 (tree-right tree) (- h 1))))))) 
#lang scheme

(require racket/class)

(define 2tree<%> (interface() isEmpty? printTree))

(define Empty2tree%
  (class* object% (2tree<%>)
    (super-new)
    (define/public (isEmpty?) #t)
    (define/public (printTree) (void))
    )
  )

(define Nonempty2tree%
  (class* object% (2tree<%>)
    (super-new)
    (init-field tag data)
    (field (left (new Empty2tree%))
           (right (new Empty2tree%)))
    (define/public (isEmpty?) #f)
    (define/public (printTree) (begin
                                 (println tag)
                                 (send right printTree)
                                 (send left printTree)))
    (define/public (get-tag) tag)
    (define/public (get-data) data)
    (define/public (set-tag! new-tag) (set! tag new-tag))
    (define/public (set-data! new-data) (set! data new-data))
    (define/public (get-left) left)
    (define/public (get-right) right)
    (define/public (set-left! new-left) (set! left new-left))
    (define/public (set-right! new-right) (set! right new-right))
    )
  )

(define etree (new Empty2tree%))
(println "is empty tree empty?")
(send etree isEmpty?)
(send etree printTree)

(println "is unempty tree empty?")
(define tree (new Nonempty2tree% (tag 4) (data "data")))
(send tree isEmpty?)
(send tree printTree)
(send tree get-tag)
(send tree get-data)
(send tree set-tag! 0)
(send tree set-data! "new data")
(send tree get-tag)
(send tree get-data)
(send tree get-left)
(send tree get-right)
(send tree printTree)

(define ltree (new Nonempty2tree% (tag 1) (data "left data")))
(define rtree (new Nonempty2tree% (tag 2) (data "right data")))

(println "adding left list")
(send tree set-left! ltree)
(send tree get-left)
(send tree get-right)
(send tree printTree)

(println "adding right list")
(send tree set-right! rtree)
(send tree get-left)
(send tree get-right)
(send tree printTree)
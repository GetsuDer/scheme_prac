#lang scheme
(require racket/mpair)

;  Если можно обойтись без макроса - лучше обойтись без макроса.
; Здесь без макроса обойтись можно => уместнее будет функция
(define (rot-left! mlst)
  (let loop ((curr mlst) (first (mlist-ref mlst 0)))
       (cond ((null? (mcdr curr))
              (set-mcar! curr first))
             (else
              (set-mcar! curr (mcar (mcdr curr)))
              (loop (mcdr curr) first))
             )
       )
  )

(define-syntax rot-left!-syntax
  (syntax-rules ()
    ((rot-left! mlst)
     (let loop ((curr mlst) (first (mlist-ref mlst 0)))
       (cond ((null? (mcdr curr))
              (set-mcar! curr first))
             (else
              (set-mcar! curr (mcar (mcdr curr)))
              (loop (mcdr curr) first))
             )
       )
     )
    )
  )
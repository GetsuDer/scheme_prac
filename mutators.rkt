#lang scheme/base
(require scheme/mpair)

(define (make-queue)
  (mcons `queue (mcons `() `())))

(define (queue? q)
  (and (mpair? q) (eq? `queue (mcar q)) (mpair? (mcdr q))))

(define (empty-queue? q)
  (and (queue? q) (null? (mcar (mcdr q)))))

(define (front-queue q)
  (if (and (queue? q) (not (empty-queue? q)))
      (mcar (mcdr (mcdr q)))
      "empty queue"
      )
  )
(define (delete-queue! q)
  (cond ((and (queue? q) (not (empty-queue? q)))
         ; если только один элемент, то обнулить очередь
         (cond ((eq? (mcdr (mcdr (mcar (mcdr q)))) `())
                (set-mcdr! q (mcons `() `())))
               (else
                (set-mcdr! (mcdr (mcar (mcdr (mcdr (mcdr q))))) `())
                (set-mcdr! (mcdr q) (mcar (mcdr (mcdr (mcdr q))))))))
        (else q))
  )

(define (insert-queue! q e)
  (cond ((queue? q)
         (cond ((empty-queue? q)
                (set-mcdr! q (mcons (mcons e (mcons `() (mcar (mcdr q)))) (mcdr (mcdr q))))
                ; добавление первого элемента, сделать, чтобы и в начале, и в конце добавилось
                (set-mcdr! q (mcons (mcar (mcdr q)) (mcar (mcdr q))))
                )
               (else
                ; добавление не-первого элемента, добавить второму элементу ссылку назад на первый
                (set-mcar! (mcdr (mcar (mcdr q))) (mcons e (mcons `() (mcar (mcdr q)))))
                (set-mcdr! q (mcons (mcar (mcdr (mcar (mcdr q)))) (mcdr (mcdr q))))
                )
               )
         )
        (else q)
        )
  )
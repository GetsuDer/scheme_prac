#lang scheme/base

; В мемоизированной версии если ответ еще не был получен, он все равно будет считаться.
; То есть, если нужно посчитать много разных значений, то выигрыша не будет, будет проигрыш
; по памяти за счет места, занимаемого таблицей, и проигрыш по времени за счет работы с таблицей и поиска в ней.
; Чтобы получить выигрыш (понятно, что только по времени), в запросах должно быть довольно много повторов.
;

(define (even-deficient n)
  (define (sum_nat_div num)
    (let loop ((sum 0) (curr 1))
      (cond ((= num curr) (+ sum num))
            ((= 0 (remainder num curr)) (loop (+ sum curr) (+ curr 1)))
            (else (loop sum (+ curr 1)))
            )
      )
    )
  (let loop ((curr 2) (num n))
    (cond ((< (sum_nat_div curr) (* 2 curr))
           (if (= num 1) curr (loop (+ curr 2) (- num 1))))
          (else (loop (+ curr 2) num))
          )
    )
  )

(define (memo-even-deficient n)
  (let ((table (make-hash `())))
    (let ((prev-result (hash-ref table n #f)))
        (if prev-result prev-result
            (let ((result (even-deficient n)))
              (hash-set! table n result)
              result)
            )
        )
    )
  )
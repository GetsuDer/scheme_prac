; "Доктор". Осень 2021
#lang scheme
; В учебных целях используется базовая версия Scheme

(require racket/vector)
; подключаем функции для работы с векторами
 
; основная функция, запускающая "Доктора"
; параметр name -- имя пациента
(define (visit-doctor name)
  (printf "Hello, ~a!\n" name)
  (print '(what seems to be the trouble?))
  (doctor-driver-loop name)
)

; цикл диалога Доктора с пациентом
; параметр name -- имя пациента
(define (doctor-driver-loop name)
    (newline)
    (print '**) ; доктор ждёт ввода реплики пациента, приглашением к которому является **
    (let ((user-response (read)))
      (cond 
	    ((equal? user-response '(goodbye)) ; реплика '(goodbye) служит для выхода из цикла
             (printf "Goodbye, ~a!\n" name)
             (print '(see you next week)))
            (else (print (reply user-response)) ; иначе Доктор генерирует ответ, печатает его и продолжает цикл
                  (doctor-driver-loop name)
             )
       )
      )
)

; генерация ответной реплики по user-response -- реплике от пользователя 
(define (reply user-response)
      (case (random 2) ; с равной вероятностью выбирается один из двух способов построения ответа
          ((0) (qualifier-answer user-response)) ; 1й способ
          ((1) (hedge))  ; 2й способ
      )
)
			
; 1й способ генерации ответной реплики -- замена лица в реплике пользователя и приписывание к результату нового начала
(define (qualifier-answer user-response)
        (append (pick-random-vector '#((you seem to think that)
                                       (you feel that)
                                       (why do you believe that)
                                       (why do you say that)
                                       (you said that)
                                       (you are sure that)
                                       (does you parents know that)
                                       )
                )
                (change-person user-response)
        )
 )

; случайный выбор одного из элементов вектора vctr
(define (pick-random-vector vctr)
  (vector-ref vctr (random (vector-length vctr)))
)

; замена лица во фразе			
(define (change-person phrase)
        (many-replace-v3 '((am are)
                        (are am)
                        (i you)
                        (me you)
                        (mine yours)
                        (my your)
						(myself yourself)
                        (you i)
                        (your my)
                        (yours mine)
						(yourself myself)
						(we you)
						(us you)
						(our your)
						(ours yours)
						(ourselves yourselves)
						(yourselves ourselves)
						(shall will))
                      phrase)
 )
  
; осуществление всех замен в списке lst по ассоциативному списку replacement-pairs
(define (many-replace replacement-pairs lst)
        (cond ((null? lst) lst)
              (else (let ((pat-rep (assoc (car lst) replacement-pairs))) ; Доктор ищет первый элемент списка в ассоциативном списке замен
                      (cons (if pat-rep (cadr pat-rep) ; если поиск был удачен, то в начало ответа Доктор пишет замену
                                (car lst) ; иначе в начале ответа помещается прежнее начало списка без изменений
                            )
                            (many-replace replacement-pairs (cdr lst)) ; рекурсивно производятся замены в хвосте списка
                        )
                     )
               )
         )
  )

;ex 2
(define (many-replace-v2 replacement-pairs lst)
  (let loop ((result `()) (others lst))
             (if (null? others)
                    result
                    (let ((pat-rep (assoc (car others) replacement-pairs)))
                      (loop (cons (if pat-rep (cadr pat-rep) (car others)) result) (cdr others))
                      )
                    )
    )
  )

;ex 3
(define (many-replace-v3 replacement-pairs lst)
  (map (lambda (word) (let ((pat-rep (assoc word replacement-pairs))) (if pat-rep (cadr pat-rep) word))) lst)
   )
; 2й способ генерации ответной реплики -- случайный выбор одной из заготовленных фраз, не связанных с репликой пользователя
(define (hedge)
       (pick-random-vector '#((please go on)
                              (many people have the same sorts of feelings)
                              (many of my patients have told me the same thing)
                              (please continue)
                              (this is interesting)
                              (is this why you come here?)
                              (i am listening))
         )
)
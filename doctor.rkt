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
  (doctor-driver-loop-v2 name)
)

; block 2 ex 5
(define (ask-patient-name)
 (begin
  (println '(next!))
  (println '(who are you?))
  (print '**)
  (car (read))
 ) 
)

(define (visit-doctor-v2 time-to-goodbye max-workflow)
  (let loop ((more-clients max-workflow))
    (
     cond ((> more-clients 0)
           (let ask ((curr-name (ask-patient-name)))
             (cond ((equal? curr-name time-to-goodbye) `(time to go home))
                (else (printf "Hello, ~a!\n" curr-name)
                      (print '(what seems to be the trouble?))
                      (doctor-driver-loop-v2 curr-name)
                      (loop (- more-clients 1))))))
          (else `(time to go home))
  )
  )
  )

(define (visit-doctor-v3 time-to-goodbye max-workflow)
  (let loop ((more-clients max-workflow))
    (
     cond ((> more-clients 0)
           (let ask ((curr-name (ask-patient-name)))
             (cond ((equal? curr-name time-to-goodbye) `(time to go home))
                (else (printf "Hello, ~a!\n" curr-name)
                      (print '(what seems to be the trouble?))
                      (doctor-driver-loop-v3 curr-name)
                      (loop (- more-clients 1))))))
          (else `(time to go home))
  )
  )
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
             (print `(see you next week)))
            (else (print (reply user-response)) ; иначе Доктор генерирует ответ, печатает его и продолжает цикл
                  (doctor-driver-loop name)
             )
       )
      )
)

; ex 4
(define (doctor-driver-loop-v2 name)
  (let loop ((prev-responses #()))
     (newline)
     (print '**) ; доктор ждёт ввода реплики пациента, приглашением к которому является **
     (let ((user-response (read)))
       (cond
         ((equal? user-response '(goodbye)) ; реплика '(goodbye) служит для выхода из цикла
          (printf "Goodbye, ~a!\n" name)
          (print '(see you next week))
          (newline))
         (else (print (reply-v2 user-response prev-responses)) ; иначе Доктор генерирует ответ, печатает его и продолжает цикл
               (loop (vector-append (vector user-response) prev-responses))
             )
       )
      )
    )
  )

(define (doctor-driver-loop-v3 name)
  (let loop ((prev-responses #()) (all-keywords (get-all-possible-keywords)))
     (newline)
     (print '**) ; доктор ждёт ввода реплики пациента, приглашением к которому является **
     (let ((user-response (read)))
       (cond
         ((equal? user-response '(goodbye)) ; реплика '(goodbye) служит для выхода из цикла
          (printf "Goodbye, ~a!\n" name)
          (print '(see you next week))
          (newline))
         (else (print (reply-v2 user-response prev-responses)) ; иначе Доктор генерирует ответ, печатает его и продолжает цикл
               (loop (vector-append (vector user-response) prev-responses) all-keywords)
             )
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

(define (reply-v2 user-response prev-responses)
  (case (random 3)
    ((0) (qualifier-answer user-response))
    ((1) (hedge))
    ; next is reply, not reply-v2 to exclude history-answer variant
    ; it is possible just repeat random call while not history-answer is chosen, but current variant is faster
    ((2) (if (vector-empty? prev-responses) (reply user-response) (history-answer prev-responses)))
    )
  )

(define (reply-v3 user-response prev-responses all-keywords)
  (case (random 4)
        ((0) (qualifier-answer user-response))
    ((1) (hedge))
    ; next is reply, not reply-v2 to exclude history-answer variant
    ; it is possible just repeat random call while not history-answer is chosen, but current variant is faster
    ((2) (if (vector-empty? prev-responses) (reply user-response) (history-answer prev-responses)))
    ((3) (if (check-for-keywords user-response all-keywords) (answer-by-keyword user-response all-keywords) (reply-v2 user-response prev-responses)))
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

; block 1 ex 2
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

;block 1 ex 3
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

;block 1 ex 4 3rd way to generate an answer - remember one of the previous user responses
(define (history-answer prev-responses)
  (append `(earlier you said that) (change-person (pick-random-vector prev-responses)))
  )

; block 2 ex 6 answer by keywords
(define keywords_structure '#(
  ( ; начало данных 1й группы
    (depressed suicide exams university) ; список ключевых слов 1й группы
    ( ; список шаблонов для составления ответных реплик 1й группы 
	  (when you feel depressed, go out for ice cream)
          (depression is a disease that can be treated)
	)
  ) ; завершение данных 1й группы
  ( ; начало данных 2й группы ...
    (mother father parents brother sister uncle ant grandma grandpa son daughter)
    (
	  (tell me more about your * , i want to know all about your *)
          (why do you feel that way about your * ?)
	)
  )
  (
    (university scheme lections seminars lectors students tasks)
	(
	  (your education is important)
	  (how many time do you spend to learning ?)
	)
  )
  (
   (night sleep nightmare bed)
       (
        (sleep disorder is an important sign, that something is wrong)
        (how often do you have problems with dreaming?)
        )
  )
  (
   (food breakfast lunch dinner starving vomiting nausea)
   (
    (you should eat properly to be a healty person)
    (how often do you have problems with eating?)
    (does anybody else know about that?)
    )
   )
))

; one more strategy
(define (answer-by-keyword phrase keywords)
  ; returns list of all keywords of phrase and its length (length . lst)
  (define (get-all-keywords phrase all-keywords)
    (foldl (lambda (res curr) (if (assoc curr all-keywords) (cons (+ 1 (car res)) (cons (curr (cdr res)))) (res)))
         (list 0 `()) phrase)
  )

  ; gets all answers and their number (from possible) (length. answers)
  (define (get-possible-answers-by-keyword keyword)
    (let loop ((result (list 0 `())) (i (- (vector-length keywords_structure) 1)))
      (if (< i 0) result
          (let ((more (assoc keyword (car (vector-ref keywords_structure i)))))
            (cons (+ (length more) (car result)) (append more (cdr result)))
            )
          )
      )
    )

  ; gets random answer from all possible
  (define (get-answer-by-keyword keyword)
    (pick-random-list (get-possible-answers-by-keyword keyword))
    )

  (define (pick-random-list length-lst)
    (list-ref (cdr length-lst) (random (car length-lst)))
    )

  (define (get-keyword-from-phrase phrase all-keywords)
    (pick-random-list (get-all-keywords phrase all-keywords)))
  (let get-ans ((keyword (get-keyword-from-phrase phrase keywords)))
    (many-replace-v3 (list (list `* keyword)) (get-answer-by-keyword keyword))
  )
  )

(define (check-for-keywords phrase all-keywords)
  (ormap (lambda (x) (assoc x all-keywords)) phrase)
  )

(define (get-all-possible-keywords)
      (let loop ((result `()) (i (- (vector-length keywords_structure) 1)))
      (if (< i 0) result
          (append (car (vector-ref keywords_structure i)) result))
        )
  )
   
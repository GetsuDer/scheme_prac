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

; ex 7 (last version of visit-doctor)
(define (visit-doctor-v4 time-to-goodbye max-workflow)
  (let loop ((more-clients max-workflow))
    (
     cond ((> more-clients 0)
           (let ask ((curr-name (ask-patient-name)))
             (cond ((equal? curr-name time-to-goodbye) `(time to go home))
                (else (printf "Hello, ~a!\n" curr-name)
                      (print '(what seems to be the trouble?))
                      (doctor-driver-loop-v4 curr-name)
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
  (let loop ((prev-responses #()))
     (newline)
     (print '**) ; доктор ждёт ввода реплики пациента, приглашением к которому является **
     (let ((user-response (read)))
       (cond
         ((equal? user-response '(goodbye)) ; реплика '(goodbye) служит для выхода из цикла
          (printf "Goodbye, ~a!\n" name)
          (print '(see you next week))
          (newline))
         (else (print (reply-v3 user-response prev-responses)) ; иначе Доктор генерирует ответ, печатает его и продолжает цикл
               (loop (vector-append (vector user-response) prev-responses))
             )
       )
      )
    )
  )

; ex 7
(define (doctor-driver-loop-v4 name)
  (let loop ((prev-responses #()))
     (newline)
     (print '**) ; доктор ждёт ввода реплики пациента, приглашением к которому является **
     (let ((user-response (read)))
       (cond
         ((equal? user-response '(goodbye)) ; реплика '(goodbye) служит для выхода из цикла
          (printf "Goodbye, ~a!\n" name)
          (print '(see you next week))
          (newline))
         (else (print (reply-v4 user-response prev-responses)) ; иначе Доктор генерирует ответ, печатает его и продолжает цикл
               (loop (vector-append (vector user-response) prev-responses))
             )
       )
      )
    )
  )

; ex 7
; strategies description
; strategy structure: #(pred-func, weight, reply-func)
(define strategies_structure
 `#(
    ((lambda (user-response prev-responses) #t)
      1
      (lambda (user-response prev-responses) (hedge))
      )

    ((lambda (user-response prev-responses) #t)
      2
      (lambda (user-response prev-responses) (qualifier-answer user-response))
      )

    ((lambda (user-response prev-responses) (not (vector-empty? prev-responses)))
      4
      (lambda (user-response prev-responses) (history-answer prev-responses))
      )

    ((lambda (user-response prev-responses) (check-for-keywords user-response))
      6
      (lambda (user-response prev-responses) (answer-by-keyword user-response))
      )
    )
  )

(define (get-strategy-pred strat)
  (car strat)
  )

(define (get-strategy-weight strat)
  (cadr strat)
  )

(define (get-strategy-reply-func strat)
  (caddr strat)
  )

; генерация ответной реплики по user-response -- реплике от пользователя 
(define (reply user-response)
      (case (random 2) ; с равной вероятностью выбирается один из двух способов построения ответа
          ((0) (qualifier-answer user-response)) ; 1й способ
          ((1) (hedge))  ; 2й способ
      )
)

(define (reply-v2 user-response prev-responses)
  (case (random (if (vector-empty? prev-responses) 2 3)
    ((0) (qualifier-answer user-response))
    ((1) (hedge))
    ((2) (history-answer prev-responses))
    )
    )
  )

(define (reply-v3 user-response prev-responses)
  (case (random 4)
    ((0) (qualifier-answer user-response))
    ((1) (hedge))
    ((2) (if (vector-empty? prev-responses) (hedge) (history-answer prev-responses)))
    ((3)
     (if (check-for-keywords user-response) (answer-by-keyword user-response) (hedge)))
    )
  )

;ex 7
(define (reply-v4 user-response prev-responses)
  (define (get-common-strategies-weight strategies)
    (let loop ((sum 0) (i (- (vector-length  strategies) 1)))
    (if (< i 0)
        sum
        (loop (+ sum (get-strategy-weight (vector-ref strategies i))) (- i 1))
        )
      )
    )
  
  (define (choose-strategy strategies)
    (let ((rand_ind (random (get-common-strategies-weight strategies))))
      (let loop ((more_weight rand_ind) (i 0))
        (let* ((strat (vector-ref strategies i)) (weight (get-strategy-weight strat)))
          (if (< more_weight weight)
              (get-strategy-reply-func strat)
              (loop (- more_weight weight) (+ i 1))
              )
          )
        )
      )
    )
  (let* ((good-strategies (vector-filter (lambda (x) ((eval (get-strategy-pred x)) user-response prev-responses)) strategies_structure))
         (curr-strat (choose-strategy good-strategies)))
    ((eval curr-strat) user-response prev-responses)
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
                    (reverse result)
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
    (depressed suicide exams university nightmare starving) ; список ключевых слов 1й группы
    ( ; список шаблонов для составления ответных реплик 1й группы 
	  (when you feel depressed, go out for ice cream)
          (depression is a disease that can be treated)
          (you are important, you should remember it)
          (does your family know about it?)
	)
  ) ; завершение данных 1й группы
  ( ; начало данных 2й группы ...
    (mother father parents brother sister uncle ant grandma grandpa son daughter)
    (
	  (tell me more about your * )
          (i want to know all about your *)
          (why do you feel that way about your * ?)
          (your family is important, but also is you)
          (how old is your * ?)
	)
  )
  (
    (university scheme lectures lecture seminars lectors students tasks nightmare)
	(
	  (your education is important)
	  (how many time do you spend to learning ?)
          (what is the hardest part for you with * ?)
          (what is your main problem with * ?)
	)
  )
  (
   (night sleep nightmare bed)
       (
        (sleep disorder is an important sign that something is wrong)
        (how often do you have problems with dreaming?)
        (did you have the same problems in your childhood ?)
        (is your bed comfortable enough ?)
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

(define all-keywords
  (let loop ((result `()) (i (- (vector-length keywords_structure) 1)))
    (if (< i 0)
        result
        (loop (append (car (vector-ref keywords_structure i)) result) (- i 1))
        )
    )
  )

; one more strategy
(define (answer-by-keyword phrase)
  ; returns list of all keywords of phrase and its length (list length lst)
  (define get-all-keywords
    (foldl (lambda (curr res) (if (member curr all-keywords) (cons (+ 1 (car res)) (cons curr (cdr res))) res))
         (cons 0 `()) phrase)
    )

  ; gets all answers and their number (from possible) (list length answers)
  (define (get-possible-answers-by-keyword keyword)
    (let loop ((result (cons 0 `())) (i (- (vector-length keywords_structure) 1)))
      (cond ((< i 0) result)
            (else (let ((curr-keywords (car (vector-ref keywords_structure i))) (curr-templates (cadr (vector-ref keywords_structure i))))
                    (cond ((member keyword curr-keywords)
                           (loop (cons (+ (length curr-templates) (car result)) (append curr-templates (cdr result))) (- i 1)))
                          (else (loop result (- i 1)))
                          )
                    )
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
  
  (let get-ans ((keyword (pick-random-list get-all-keywords)))
    (many-replace-v3 (list (list `* keyword)) (get-answer-by-keyword keyword))
    )
  )

(define (check-for-keywords phrase)
  (ormap (lambda (x) (member x all-keywords)) phrase)
  )
   
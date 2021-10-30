; "Весна". Осень 2021
#lang scheme

(require racket/vector)
; подключаем функции для работы с векторами
 
(define test-text "Это длинный текст, в котором много знаков препинания - точки, запятые и тд. Тут есть предложения, оканчивающиеся так! И так? Ну - вот : еще могу так;")
; На вход в качестве реплики пациента принимается строка.
; Затем строка разбивается на предложения по точкам,
; а предложения - на слова по пробелам (+ отделяются запятые).
; То есть внутренним представлением реплики пользователя является список предложений, а предложения являются списками слов.
; Формат ввода - из знаков препинания поддерживаются запятые, тире, двоеточия и точки с запятой, а также круглые скобки
; Конец предложения символизирует точка, восклицательный или вопросительный знак, а также конец текста.
(define (text-to-inner text)
  (define len (string-length text))
  (let loop ((i 0) (res `()) (last_sentence `()) (last_word ""))
    (cond ((= len i) ; реплика закончилась
           (reverse ; чтобы предложения были в исходном порядке
            (if (not (eq? "" last_word)) ; предложение оборвалось, кончилось не знаком препинания
                (cons
                 (reverse (cons last_word last_sentence))
                 res)
                (if (null? last_sentence)
                    res
                    (cons (reverse last_sentence) res)
                    )
                )
            )
           )
          ((eq? (string-ref text i) #\space) ; кончилось слово
           (loop
            (+ i 1) ; следующая буква
            res ; без обновления предложений
            (if (eq? last_word "") ; множественные пробелы
                last_sentence
                (cons last_word last_sentence)) ; иначе добавить прочтенное слово к последнему предложению
            "") ; новое слово
           )
          ((or (eq? (string-ref text i) #\.)
               (eq? (string-ref text i) #\?)
               (eq? (string-ref text i) #\!)); символ конца предложения и, возможно, слова
           (loop
            (+ i 1)
            (if (eq? last_word "") ; многоточие?
                (cons (reverse (cons (substring text i (+ i 1)) last_sentence)) res)
                (cons (reverse (cons (substring text i (+ i 1)) (cons last_word last_sentence))) res))
            `() ; новое предложение начинается
            "" ; новое слово начинается
            )
           )
          ((or (eq? (string-ref text i) #\,)
               (eq? (string-ref text i) #\))
               (eq? (string-ref text i) #\()
               (eq? (string-ref text i) #\-)
               (eq? (string-ref text i) #\:)
               (eq? (string-ref text i) #\;)); знаки препинания
           ; закончить последнее собранное слово (если оно не пусто, то есть не отделено пробелом),
           ; добавить его и знак препинания
           (loop
            (+ i 1) ; следующая буква
            res ; без одновления предложений
            (if (eq? "" last_word)
                (cons (substring text i (+ i 1)) last_sentence) ; мусорная или отделенная пробелом запятая
                (cons (substring text i (+ i 1)) (cons last_word last_sentence))) ; отделить последнее слово, добавить его и запятую
            "")                
           )
          (else ; просто буква
           (loop
            (+ i 1)
            res
            last_sentence
            (string-append last_word (substring text i (+ i 1)))
            )
           )
          )
    )
  )

; Собрать из внутреннего представления отдельного предложения строку
(define (sentence-to-text inner)
  (let loop ((cur inner) (res ""))
    (if (null? cur)
        res
        (loop (cdr cur) (string-append (if (or (eq? "" res) (member (car cur) (list "." "," ";" ":" "-" "?" "!")))
                                           res
                                           (string-append res " ")) (car cur)))
        )
    )
  )
   
; получить имя пациента, как первое слово из введенной строки
(define (ask-patient-name)
 (begin
  (println "next!")
  (println "who are you?")
  (print '**)
  (caar (text-to-inner (read-line))) ; первое слово первого предложения - имя пациента
 ) 
)

; функция для запуска доктора
; параметры - слово, являющееся триггером конца работы,
; и максимальное число пациентов для приема 
(define (visit-doctor time-to-goodbye max-workflow)
  (let loop ((more-clients max-workflow))
    (
     cond ((> more-clients 0)
           (let ask ((curr-name (ask-patient-name)))
             (cond ((equal? curr-name time-to-goodbye) (printf "Time to go home\n"))
                (else (printf "Hello, ~a!\n" curr-name)
                      (print "what seems to be the trouble?")
                      (doctor-driver-loop curr-name)
                      (loop (- more-clients 1))))))
          (else (printf "Time to go home\n"))
          )
    )
  )
  
; ex 7
; strategies description
; strategy structure: `(pred-func, weight, reply-func)

(define (create-strat pred-func weight reply-func)
  (list pred-func weight reply-func))

(define (add-strat strat strat-struct)
  (vector-append (vector strat) strat-struct))

(define strategies_structure
  (add-strat (create-strat (lambda (user-response prev-responses) #t) ; ответ не связан с репликой, возможен всегда
                         1
                         (lambda (user-response prev-responses) (hedge))
                         )
             (add-strat (create-strat (lambda (user-response prev-responses) #t) ; ответ возможен для любой реплики, то есть всегда
                         2
                         (lambda (user-response prev-responses) (qualifier-answer user-response))
                         )
                        (add-strat (create-strat (lambda (user-response prev-responses) (not (vector-empty? prev-responses))) ; нужен непустой вектор предыдущих реплик
                                                 4
                                                 (lambda (user-response prev-responses) (history-answer prev-responses))
                                                 )
                                   (add-strat (create-strat (lambda (user-response prev-responses) (check-for-keywords user-response)) ; проверяет все предложения
                                                            6
                                                            (lambda (user-response prev-responses) (answer-by-keyword user-response)) ; выделяет все ключевые слова (из всех предложений)
                                                            )
                                              #()
                                              )
                                   )
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


; ex 7
(define (doctor-driver-loop name)
  (let loop ((prev-responses #()))
     (newline)
     (print '**) ; доктор ждёт ввода реплики пациента, приглашением к которому является **
     (let ((user-response (text-to-inner (read-line))))
       (cond
         ((equal? (caar user-response) "goodbye") ; реплика, начинающаяся с goodbye, служит для выхода из цикла
          (printf "Goodbye, ~a!\n" name)
          (printf "See you next week.\n")
         )
         (else (print (reply user-response prev-responses strategies_structure)) ; иначе Доктор генерирует ответ, печатает его и продолжает цикл
               (loop (vector-append (list->vector user-response) prev-responses)) ; добавить предложения из реплики пациента в архив реплик
             )
       )
      )
    )
  )

;Выбор случайной стратегии с учетом веса и применимости
(define (reply user-response prev-responses all-strats)
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
  (let* ((good-strategies (vector-filter (lambda (x) ((get-strategy-pred x) user-response prev-responses)) all-strats))
         (curr-strat (choose-strategy good-strategies)))
    (curr-strat user-response prev-responses)
    )
  )

; 1й способ генерации ответной реплики -- замена лица в реплике пользователя и приписывание к результату нового начала
(define (qualifier-answer user-response)
        (string-append (pick-random-vector '#("you seem to think that "
                                       "you feel that "
                                       "why do you believe that "
                                       "why do you say that "
                                       "you said that "
                                       "you are sure that "
                                       "does you parents know that "
                                       )
                )
                (sentence-to-text (change-person (list-ref user-response (random (length user-response))))) ; из реплики пользователя
                ; выбирается случайное предложение
        )
 )

; случайный выбор одного из элементов вектора vctr
(define (pick-random-vector vctr)
  (vector-ref vctr (random (vector-length vctr)))
)

; замена лица во фразе			
(define (change-person phrase)
        (many-replace '(("am" "are")
                        ("are" "am")
                        ("i" "you")
                        ("me" "you")
                        ("mine" "yours")
                        ("my" "your")
                        ("myself" "yourself")
                        ("you" "i")
                        ("your" "my")
                        ("yours" "mine")
                        ("yourself" "myself")
                        ("we" "you")
                        ("us" "you")
                        ("our" "your")
                        ("ours" "yours")
                        ("ourselves" "yourselves")
                        ("yourselves" "ourselves")
                        ("shall" "will"))
                      phrase)
 )
  
; осуществление всех замен в списке lst по ассоциативному списку replacement-pairs
(define (many-replace replacement-pairs lst)
  (map (lambda (word) (let ((pat-rep (assoc word replacement-pairs))) (if pat-rep (cadr pat-rep) word))) lst)
   )

; 2й способ генерации ответной реплики -- случайный выбор одной из заготовленных фраз, не связанных с репликой пользователя
(define (hedge)
       (pick-random-vector '#("please go on"
                              "many people have the same sorts of feelings"
                              "many of my patients have told me the same thing"
                              "please continue"
                              "this is interesting"
                              "is this why you come here?"
                              "i am listening")
         )
)

; 3й способ генерации ответной реплики -- цитата одного из предыдущих предложений
(define (history-answer prev-responses)
  (string-append "earlier you said that " (sentence-to-text (change-person (pick-random-vector prev-responses))))
  )


; 4й способ генерации ответной реплики -- случайный выбор фразы из одного из заготовленных наборов,
; в соответствии с наличием в реплике пользователя ключевых слов
; Структура для работы с ключевыми словами
(define keywords_structure '#(
  ( ; начало данных 1й группы
    ("depressed" "suicide" "exams" "university" "nightmare" "starving") ; список ключевых слов
    ( ; список шаблонов для составления ответных реплик
	  (when you feel depressed, go out for ice cream)
          (depression is a disease that can be treated)
          (you are important, you should remember it)
          (does your family know about it?)
	)
  )
  (
    ("mother" "father" "parents" "brother" "sister" "uncle" "ant" "grandma" "grandpa" "son" "daughter") ; список ключевых слов
    ( ; список шаблонов для составления ответных реплик
	  ("tell" "me" "more" "about" "your" "*" ".")
          ("i" "want" "to" "know" "all" "about" "your" "*" ".")
          ("why" "do" "you" "feel" "that" "way" "about" "your" "*" "?")
          ("your" "family" "is" "important" "," "but" "also" "is" "you" ".")
          ("how" "old" "is" "your" "*" "?")
	)
  )
  (
    ("university" "scheme" "lectures" "lecture" "seminars" "lectors" "students" "tasks" "nightmare") ; список ключевых слов
	( ; список шаблонов для составления ответных реплик
	  ("your" "education" "is" "important" ".")
	  ("how" "many" "time" "do" "you" "spend" "to" "learning" "?")
          ("what" "is" "the" "hardest" "part" "for" "you" "with" "*" "?")
          ("what" "is" "your" "main" "problem" "with" "*" "?")
          )
        )
  (
   ("night" "sleep" "nightmare" "bed") ; список ключевых слов
       ( ; список шаблонов для составления ответных реплик
        ("sleep" "disorder" "is" "an" "important" "sign" "that" "something" "is" "wrong" ".") 
        ("how" "often" "do" "you" "have" "problems" "with" "dreaming" "?")
        ("did" "you" "have" "the" "same" "problems" "in" "your" "childhood" "?")
        ("is" "your" "bed" "comfortable" "enough" "?")
        )
  )
  (
   ("food" "breakfast" "lunch" "dinner" "starving" "vomiting" "nausea") ; список ключевых слов
   ( ; список шаблонов для составления ответных реплик
    ("you" "should" "eat" "properly" "to" "be" "a" "healty" "person")
    ("how" "often" "do" "you" "have" "problems" "with" "eating" "?")
    ("does" "anybody" "else" "know" "about" "that" "?")
    )
   )
))

; Общий список ключевых слов (для проверки их наличия в фразе), с повторами
(define all-keywords
  (let loop ((result `()) (i (- (vector-length keywords_structure) 1)))
    (if (< i 0)
        result
        (loop (append (car (vector-ref keywords_structure i)) result) (- i 1))
        )
    )
  )

; Сама стратегия
(define (answer-by-keyword phrase)
  ; Вычленить из текста список всех ключевых слов
  (define get-all-keywords
    (foldl (lambda (curr1 res1)
             (let ((tmp_res (foldl (lambda (curr2 res2) (if (member curr2 all-keywords)
                                                            (cons (+ 1 (car res2)) (cons curr2 (cdr res2)))
                                                            res2))
                                   (cons 0 `())
                                   curr1)))
               (cons (+ (car tmp_res) (car res1)) (append (cdr tmp_res) (cdr res1)))))
           (cons 0 `())
           phrase)
    )

  ; По данному ключевому слову получить список всех возможных ответов
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

  ; Выбрать случайный ответ
  (define (get-answer-by-keyword keyword)
    (pick-random-list (get-possible-answers-by-keyword keyword))
    )

  ; Выбрать случайный элемент списка
  (define (pick-random-list length-lst)
    (list-ref (cdr length-lst) (random (car length-lst)))
    )

  ; Применить стратегию
  (let get-ans ((keyword (pick-random-list get-all-keywords)))
    (sentence-to-text (many-replace (list (list "*" keyword)) (get-answer-by-keyword keyword)))
    )
  )

; Проверить наличие в реплике пользователя ключевых слов
; Заметим, что реплика состоит из предложений, и проверяем каждое предложение
(define (check-for-keywords phrase)
  (ormap (lambda (y) (ormap (lambda (x) (member x all-keywords)) y)) phrase))
   
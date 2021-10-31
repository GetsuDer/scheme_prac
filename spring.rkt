; "Весна". Осень 2021
#lang scheme

(require racket/vector)
; подключаем функции для работы с векторами

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
          ((char-whitespace? (string-ref text i)) ; кончилось слово
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
          ((or (char-alphabetic? (string-ref text i))
               (and (char<=? #\0 (string-ref text i))
                    (char<=? (string-ref text i) #\9))) ; просто буква или цифра
           (loop
            (+ i 1)
            res
            last_sentence
            (string-append last_word (if (and (eq? "" last_word) (null? last_sentence))
                                         (string (char-upcase (string-ref text i)))
                                         (string (char-downcase (string-ref text i)))))
            )
           )
          (else ; какой-то некорректный символ (ну или нереализованный)
           (loop ; просто пропустим
            (+ i 1)
            res last_sentence
            last_word
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

; Конкретное значение N
(define N 3)

; Информация о N-граммах будет храниться в hash-table info
(define info
  (make-hash))

; записать обработанные данные о N-граммах в файл name
; формат записи: ключ \n число значений \n *число значений раз* слово \n число употреблений 
(define (dump-info name)
  (let ((out (open-output-file name #:mode `text #:exists `replace)))
    (hash-for-each info (lambda (key value)
                          (begin
                            (fprintf out "~a~n" key)
                            (fprintf out "~a~n" (length value))
                            (let loop ((rest value))
                              (cond ((not (null? rest))
                                     (fprintf out "~a~n" (caar rest))
                                     (fprintf out "~a~n" (cdar rest))
                                     (loop (cdr rest))
                                     )
                                    )
                              )
                            )
                          )
                   )
    (close-output-port out)
    )
  )

; прочитать обработанные данные из файла dumpname
; формат записи: ключ \n число значений \n *число значений раз* слово \n число употреблений
(define (read-info-from-dump dumpname)
  (let ((in (open-input-file dumpname #:mode `text)))
    (let loop1 ((line (read-line in)))
      (cond ((not (eof-object? line))
             (let ((key line) (val-num (string->number (read-line in))))
               (let loop2 ((i val-num) (res `()))
                 (cond ((= i 0) (hash-set! info key res))
                       (else
                        (loop2 (- i 1) (cons (cons (read-line in) (string->number (read-line in))) res))
                        )
                       )
                 )
               )
             (loop1 (read-line in))
             )
            )
      )
    (close-input-port in)
    )
  )

; Соединить первые n элементов списка (строк) в одну строку, переведя в нижний регистр
; Проверка длины списка лежит на вызывающей функции
(define (join-list-n lst n)
  (let loop ((res "") (more n) (rest lst))
    (cond ((= 0 more) (string-downcase res))
          (else (loop (string-append (if (eq? res "") res (string-append res " ")) (car rest)) (- more 1) (cdr rest)))
          )
    )
  )

; Если val присутствует в первых элементов пар списка lst, увеличить соответствующий счетчик.
; Иначе - добавить пару (val, 1)
(define (update-hash-list lst val)
  (let loop ((rest lst) (res `()) (contains #f))
    (cond ((null? rest) (if contains res (cons (cons val 1) res)))
          (else
           (cond ((string=? (caar rest) val)
                  (loop
                   (cdr rest)
                   (cons (cons val (+ 1 (cdar rest))) res) ; элемент найден
                   #t
                   ))
                 (else
                  (loop
                   (cdr rest)
                   (cons (car rest) res)
                   contains
                   ))
               )
           )
          )
    )
)

; Выделить из текста во внутреннем представлении text N-граммы и дополнить таблицу info
(define (process-text text)
  (let loop1 ((rest-text text)) ; по каждому предложению в тексте
    (cond ((not (null? rest-text))
           (let loop2 ((rest-sentence (car rest-text)) (rest-len (length (car rest-text)))) ; по каждому слову предложения
             (cond ((not (< rest-len N)) ; если от текущего слова можно построить N-грамму
                    (let ((key (join-list-n rest-sentence (- N 1))) (val (list-ref rest-sentence (- N 1))))
                      (if (hash-has-key? info key)
                          (let ((oldval (hash-ref info key)))
                            (hash-set! info key (update-hash-list oldval val))
                            )
                          (hash-set! info key (list (cons val 1)))
                          )
                      )
                    (loop2 (cdr rest-sentence) (- rest-len 1))
                    )
                   )
             )
           (loop1 (cdr rest-text))
           )
          )
    )
  )

; Считать текст из файла, перевести во внутреннее представление и вызвать process-text
(define (process-file filename)
  (let loop ((in (open-input-file filename #:mode `text)))
    (let ((line (read-line in)))
      (cond ((not (eof-object? line))
             (process-text (text-to-inner line))
             (loop in)
             )
            )
      )
    (close-input-port in)
    )
  )

; получить имя пациента, как первое слово из введенной строки
; если получечное представление пусто, спрашивать, пока не ответят корректно
(define (ask-patient-name)
 (begin
  (printf "Next!\n")
  (printf "Who are you?\n")
  (print '**)
  (let loop ((in (text-to-inner (read-line))))
    (cond ((null? in) (printf "Please, introduce yourself\n**") (loop (text-to-inner (read-line))))
          (else (caar in) ; первое слово первого предложения - имя пациента
        )
          )
    )
  )
  )

; функция для запуска доктора
; параметры - слово, являющееся триггером конца работы,
; и максимальное число пациентов для приема 
(define (visit-doctor time-to-goodbye_in max-workflow)
  (define time-to-goodbye
    (let loop ((i 0) (res ""))
      (if (= i (string-length time-to-goodbye_in))
          res
          (loop
           (+ i 1)
           (string-append res (string (if (= i 0)
                                          (char-upcase (string-ref time-to-goodbye_in 0))
                                          (char-downcase (string-ref time-to-goodbye_in i)))))
           )
          )
      )
    )
  (let loop ((more-clients max-workflow))
    (
     cond ((> more-clients 0)
           (let ask ((curr-name (ask-patient-name)))
             (cond
               ((equal? curr-name time-to-goodbye) (printf "Time to go home\n"))
               (else (printf "Hello, ~a!\n" curr-name)
                      (printf "What seems to be the trouble?\n")
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
     (print '**) ; доктор ждёт ввода реплики пациента, приглашением к которому является **
     (let ((user-response (text-to-inner (read-line))))
       (cond
         ((null? user-response) (printf "I will wait for your answer.\n") (loop prev-responses))
         ((equal? (caar user-response) "Goodbye") ; реплика, начинающаяся с goodbye, служит для выхода из цикла
          (printf "Goodbye, ~a!\n" name)
          (printf "See you next week.\n")
         )
         (else (printf (string-append (reply user-response prev-responses strategies_structure) "\n")) ; иначе Доктор генерирует ответ, печатает его и продолжает цикл
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
  (let ((prev_ans (sentence-to-text (change-person (list-ref user-response (random (length user-response)))))))
    (string-append (pick-random-vector '#("You seem to think that "
                                       "You feel that "
                                       "Why do you believe that "
                                       "Why do you say that "
                                       "You said that "
                                       "You are sure that "
                                       "Does you parents know that "
                                       )
                                       )
                   (string-append (string (char-downcase (string-ref prev_ans 0))) (substring prev_ans 1))
                 ; из реплики пользователя
                ; выбирается случайное предложение
        )
    )
  )

; случайный выбор одного из элементов вектора vctr
(define (pick-random-vector vctr)
  (vector-ref vctr (random (vector-length vctr)))
)

; замена лица во фразе			
(define (change-person phrase)
        (many-replace '(("am" "are") ("Am" "Are")
                        ("are" "am") ("Are" "Am")
                        ("i" "you") ("I" "You")
                        ("me" "you") ("Me" "You")
                        ("mine" "yours") ("Mine" "Yours")
                        ("my" "your") ("My" "Your")
                        ("myself" "yourself") ("Myself" "Yourself")
                        ("you" "i") ("You" "I")
                        ("your" "my") ("Your" "My")
                        ("yours" "mine") ("Yours" "Mine")
                        ("yourself" "myself") ("Yourself" "Myself")
                        ("we" "you") ("We" "You")
                        ("us" "you") ("Us" "You")
                        ("our" "your") ("Our" "Your")
                        ("ours" "yours") ("Ours" "Yours")
                        ("ourselves" "yourselves") ("Ourselves" "Yourselves")
                        ("yourselves" "ourselves") ("Yourselves" "Ourselves")
                        ("shall" "will") ("Shall" "Will")
                        )
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
  (let ((prev_ans (sentence-to-text (change-person (pick-random-vector prev-responses)))))
  (string-append "earlier you said that "
                 (string-append (string (char-downcase (string-ref prev_ans 0))) (substring prev_ans 1))
                 )
    )
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
   
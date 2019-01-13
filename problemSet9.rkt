;; Mike Huynh  Problem Set 9  11/24/17
"Problem 1"
(define (make-checking x)
  
  (define init x)
  
  (let ((balance x)
        (transaction '()))
    
    (define (bal)
      balance)
    
    (define (deposit f)
      (set! balance (+ balance f))
      (set! transaction (cons f transaction)))
    
    (define (withdraw f)
      (set! balance (- balance f))
      (set! transaction (cons (* -1 f) transaction)))

    (define (print-statement)
      (display (string-append "beginning balance: " (number->string init) "\n"))
      (let ((lst (reverse transaction)))
        (define (recur thelst)
          (cond ((null? thelst) thelst)
                (else (display (string-append "transaction: "
                                              (if (< (car thelst) 0)
                                                  "check amount: "
                                                  "deposit amount: ")
                                              (number->string (car thelst))
                                              "\n"))
                      (recur (cdr thelst)))))
        (recur lst))
      (string-append "balance: " (number->string balance) "\n"))
      
    (lambda (method)
      (cond ((eq? method 'balance) bal)
            ((eq? method 'write-check) withdraw)
            ((eq? method 'deposit) deposit)
            ((eq? method 'print-statement) print-statement)
            (else 'undefined-operation)))))

"test1"
(define checking (make-checking 100))
((checking 'write-check) 10)
((checking 'write-check) 10)
((checking 'deposit) 100)
((checking 'write-check) 10)
(display ((checking 'print-statement)))
((checking 'balance))

(newline)

"test2"
(define check (make-checking 100))
((check 'deposit) 10)
((check 'write-check) 200)
(display ((check 'print-statement)))

(newline)

"Problem 2"
(define (make-clock hour minute)
  (let ((t (+ (* 60 hour) minute)))

    (define (tick)
      (set! t (+ t 1)))

    (define (time)
      (let ((hr (floor (/ t 60)))
            (min (if (< (modulo t 60) 10)
                     (string-append "0" (number->string (modulo t 60)))
                     (number->string (modulo t 60)))))
        (cond ((= hr 0) (string-append "12:" min " AM"))
              ((= hr 24) (string-append "12:" min " AM"))
              ((> hr 12) (string-append (number->string (- hr 12)) ":" min " PM"))
              (else (string-append (number->string hr) ":" min " AM")))))

    (define (military)
      (let ((hr (if (< (floor (/ t 60)) 10)
                    (string-append "0" (number->string (floor (/ t 60))))
                    (number->string (floor (/ t 60)))))
            (min (if (< (modulo t 60) 10)
                     (string-append "0" (number->string (modulo t 60)))
                     (number->string (modulo t 60)))))
        (cond ((= (string->number hr) 24) (string-append "00:" min))
              (else (string-append hr ":" min)))))
      

    (lambda (method)
      (cond ((eq? method 'tick) tick)
            ((eq? method 'time) time)
            ((eq? method 'military) military)
            (else 'undefined-operation)))))

"test"
(define clock (make-clock 23 59))
(define get-time (clock 'time))
(define get-mil (clock 'military))
(display (get-time))
(get-mil)
((clock 'tick))
(display (get-time))
(display (get-mil))


(newline)

"Problem 3"
(define (make-book title author)
  (define (get-author)
    author)
  (define (get-title)
    title)
  (lambda (method)
    (cond ((eq? method 'get-title) get-title)
          ((eq? method 'get-author) get-author)
          (else 'undefined-operation))))
(newline)

"Problem 4"
(define (make-library)
  (let ((library-list '()))
    
    (define (add book)
      (set! library-list (cons book library-list)))
    
    (define (find-author author)
      (let ((lst '()))
        (define (helper a b)
          (cond ((null? a) lst)
                ((eq? (((car a) 'get-author)) b) (set! lst
                                                       (cons (cons (((car a) 'get-author))
                                                              (((car a) 'get-title)))
                                                       lst))
                                                 (helper (cdr a) b))
                (else (helper (cdr a) b))))
        (helper library-list author)))

    (define (find-title title)
      (let ((lst '()))
        (define (helper a b)
          (cond ((null? a) lst)
                ((eq? (((car a) 'get-title)) b) (set! lst
                                                       (cons (cons (((car a) 'get-author))
                                                                   (((car a) 'get-title)))
                                                       lst))
                                                (helper (cdr a) b))
                (else (helper (cdr a) b))))
        (helper library-list title)))

    (lambda (method)
      (cond ((eq? method 'add) add)
            ((eq? method 'find-title) find-title)
            ((eq? method 'find-author) find-author)
            (else 'undefined-operation)))))

"test"
(define mylib (make-library))
((mylib 'add) (make-book "Harry Potter and the Philosopher's Stone" "Rowling"))
((mylib 'add) (make-book "Harry Potter and the Chamber of Secrets" "Rowling"))
((mylib 'add) (make-book "Harry Potter and the Prisoner of Azkaban" "Rowling"))
((mylib 'add) (make-book "Harry Potter and the Goblet of Fire" "Rowling"))
((mylib 'add) (make-book "Harry Potter and the Order of the Phoenix" "Rowling"))
((mylib 'add) (make-book "Harry Potter and the Half-Blood Prince" "Rowling"))
((mylib 'add) (make-book "Harry Potter and the Deathly Hallows" "Rowling"))
((mylib 'add) (make-book "The Hunger Games" "Suzanne Collins"))
((mylib 'add) (make-book "Catching Fire" "Suzanne Collins"))
((mylib 'add) (make-book "Mockingjay" "Suzanne Collins"))
((mylib 'add) (make-book "The Magician" "Michael Scott"))
((mylib 'add) (make-book "The Magician" "W . Somerset Maugham"))

((mylib 'find-title) "The Magician")
((mylib 'find-author) "Suzanne Collins")
((mylib 'find-author) "Rowling")
(newline)

"Problem 5"
(define (make-track title)
  (let ((song title)
        (artist "unknown")
        (album "unknown"))
    
    (define (get-title)
      title)
    (define (get-artist)
      artist)
    (define (get-album)
      album)
    
    (define (set-title x)
      (set! title x))
    (define (set-artist x)
      (set! artist x))
    (define (set-album x)
      (set! album x))
    
    (lambda (method)
      (cond ((eq? method 'get-title) get-title)
            ((eq? method 'get-artist) get-artist)
            ((eq? method 'get-album) get-album)
            ((eq? method 'set-title) set-title)
            ((eq? method 'set-artist) set-artist)
            ((eq? method 'set-album) set-album)
            (else 'undefined-operation)))))

"test"
(define track (make-track "awesome"))
((track 'get-title))
((track 'get-artist))
((track 'get-album))
((track 'set-title) "gumball")
((track 'get-title))
(newline)

"Problem 6"
(define (make-music-library)
  (let ((tracklist '()))

    (define (add track)
      (set! tracklist (cons track tracklist)))

    (define (find-by-artist artist)
      (let ((lst '()))
        (define (helper a b)
          (cond ((null? a) lst)
                ((eq? (((car a) 'get-artist)) b) (set! lst
                                                       (cons (list (((car a) 'get-title))
                                                                   (((car a) 'get-artist))
                                                                   (((car a) 'get-album)))
                                                       lst))
                                                 (helper (cdr a) b))
                (else (helper (cdr a) b))))
        (helper tracklist artist)))

    (define (find-by-title title)
      (let ((lst '()))
        (define (helper a b)
          (cond ((null? a) lst)
                ((eq? (((car a) 'get-title)) b) (set! lst
                                                       (cons (list (((car a) 'get-title))
                                                                   (((car a) 'get-artist))
                                                                   (((car a) 'get-album)))
                                                       lst))
                                                 (helper (cdr a) b))
                (else (helper (cdr a) b))))
        (helper tracklist title)))

    (define (find-by-album album)
      (let ((lst '()))
        (define (helper a b)
          (cond ((null? a) lst)
                ((eq? (((car a) 'get-album)) b) (set! lst
                                                       (cons (list (((car a) 'get-title))
                                                                   (((car a) 'get-artist))
                                                                   (((car a) 'get-album)))
                                                       lst))
                                                 (helper (cdr a) b))
                (else (helper (cdr a) b))))
        (helper tracklist album)))

    (lambda (method)
      (cond ((eq? method 'add) add)
            ((eq? method 'find-by-artist) find-by-artist)
            ((eq? method 'find-by-title) find-by-title)
            ((eq? method 'find-by-album) find-by-album)
            (else 'undefined-operation)))))
      

    
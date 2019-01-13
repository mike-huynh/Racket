;; Mike Huynh  Problem Set 8  11/4/17

(define (string->symbol-list str)
  ;; takes a string, returns list of symbols
  ;; spaces are used to delimit strings representing symbols
  ;; newlines are turned into the symbol 'newline in the output list
  (define (string->symbol-list-aux char-lst)
    (cond ((null? char-lst) '())
          ((eq? (car char-lst) #\space)
           (string->symbol-list-aux (cdr char-lst)))
          ((eq? (car char-lst) #\newline)
           (cons 'newline (string->symbol-list-aux (cdr char-lst))))
          (else
           (cons (get-first-symbol char-lst)
                 (string->symbol-list-aux (remove-first-symbol char-lst))))))
  (string->symbol-list-aux (string->list str)))

(define (get-first-symbol char-lst)
  ; given a list of characters not starting with space or newline
  ; returns a symbol made from the characters up to a space newline or end of list
  (define (gfs-iter char-lst sofar)
    (cond ((or (null? char-lst)
               (eq? (car char-lst) #\space)
               (eq? (car char-lst) #\newline))
           (string->symbol (list->string (reverse sofar))))
          (else
           (gfs-iter (cdr char-lst) (cons (car char-lst) sofar)))))
  (gfs-iter char-lst '()))

(define (remove-first-symbol char-lst)
  ; given a list of characters not starting with space or newline
  ; returns the list with all characters up to a space, newline,
  ; or end of list removed
  (cond ((or (null? char-lst)
             (eq? (car char-lst) #\space)
             (eq? (car char-lst) #\newline))
         char-lst)
        (else
         (remove-first-symbol (cdr char-lst)))))

(define (symbol-list->string sym-lst)
  ; given a list of symbols, turns it into a string
  ; by adding space characters between strings representing
  ; symbols, and adds newline characters wherever there is
  ; the 'newline symbol
  (cond ((null? sym-lst) "")
        ((eq? (car sym-lst) 'newline)
         (string-append (list->string (list #\newline)) (symbol-list->string (cdr sym-lst))))
        (else (string-append (symbol->string (car sym-lst))
                             " "
                             (symbol-list->string (cdr sym-lst))))))

(define (surfin-bird) 
  "a well a everybodys heard about the bird
bird bird bird b-birds the word
a well a bird bird bird the bird is the word
a well a bird bird bird well the bird is the word
a well a bird bird bird b-birds the word
a well a bird bird bird well the bird is the word
a well a bird bird b-birds the word
a well a bird bird bird b-birds the word
a well a bird bird bird well the bird is the word
a well a bird bird b-birds the word
a well a dont you know about the bird
well everybody knows that the bird is the word
a well a bird bird b-birds the word
a well a

a well a everybodys heard about the bird
bird bird bird b-birds the word
a well a bird bird bird b-birds the word
a well a bird bird bird b-birds the word
a well a bird bird b-birds the word
a well a bird bird bird b-birds the word
a well a bird bird bird b-birds the word
a well a bird bird bird b-birds the word
a well a bird bird bird b-birds the word
a well a dont you know about the bird
well everybodys talking about the bird
a well a bird bird b-birds the word
a well a bird

surfin bird
bbbbbbbbbbbbbbbbbb aaah

pa pa pa pa pa pa pa pa pa pa pa pa pa pa pa pa
pa pa pa pa pa pa pa pa pa pa pa pa pa pa ooma mow mow
papa ooma mow mow

papa ooma mow mow papa ooma mow mow
papa ooma mow mow papa ooma mow mow
ooma mow mow papa ooma mow mow
papa ooma mow mow papa ooma mow mow
papa ooma mow mow papa ooma mow mow
oom oom oom oom ooma mow mow
papa ooma mow mow papa oom oom oom
oom ooma mow mow papa ooma mow mow
ooma mow mow papa ooma mow mow
papa a mow mow papa ooma mow mow
papa ooma mow mow ooma mow mow
papa ooma mow mow ooma mow mow
papa oom oom oom oom ooma mow mow
oom oom oom oom ooma mow mow
ooma mow mow papa ooma mow mow
papa ooma mow mow ooma mow mow
well dont you know about the bird
well everybody knows that the bird is the word
a well a bird bird b-birds the word

papa ooma mow mow papa ooma mow mow
papa ooma mow mow papa ooma mow mow
papa ooma mow mow papa ooma mow mow")

; see what (string->symbol-list (surfin-bird) produces


;; extra functions using bit sequences
;; the strings here are strings of 0s and 1s
;; a bit-sequence is simply an integer
;; a 1 is added to the start of each bit-sequence
;; as (number->string x 2) drops leading zeroes

(define (bit-sequence->string num)
  (define (rem-first string)
    (substring string 1 (string-length string)))
  (rem-first (number->string num 2)))

(define (string->bit-sequence  str)
  (string->number (string-append "1" str) 2))

;; try (bit-sequence->string 1729) and then invert it with string->bit-sequence

(define (log2 x)
  (/ (log x)(log 2)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
"Problem 1"
(define (word-frequencies str)
  
  ;; taken from lab
  (define (num-occurs sym lst)
      (define (occur-help sym lst base)
        (cond ((null? lst) base)
              ((eq? sym (car lst)) (occur-help sym (cdr lst) (+ 1 base)))
              (else (occur-help sym (cdr lst) base))))
      (occur-help sym lst 0))
  
  ;; remove all func from lab
  (define (remove-all x l)
      (if (null? l)
          l
          (if (equal? x (car l))
              (remove-all x (cdr l))
              (cons (car l) (remove-all x (cdr l))))))

  (define (convert-string str)
    (if (string? str)
        (string->symbol-list str)
        str))

  (let ((x (convert-string str)))
    (if (null? x)
        x
        (cons (cons (car x) (num-occurs (car x) x))
              (word-frequencies (remove-all (car x) x))))))

"Test"
"(word-frequencies (surfin-bird))"
(word-frequencies (surfin-bird))
(newline)

"Problem 2a"
;; Taken all heap functions from Lab 9 and copy pasta'd it here
(define (create-heap vw-pair left right)
  (list vw-pair left right))

(define (h-min heap)
  (car heap))

(define (value heap)
  (car (h-min heap)))

(define (weight heap)
  (cdr (h-min heap)))

(define (left heap)
  (cadr heap))

(define (right heap)
  (caddr heap))

(define (make-internal-node 0-tree 1-tree)
  (create-heap 'internal 0-tree 1-tree))

(define (make-leaf-node symbol weight)
  (list (cons symbol weight) '() '()))

(define (internal? x)
  (eq? (car x) 'internal))

;; Taken from lab 9
(define (insert vw-pair heap)
  (if (null? heap)
      (create-heap vw-pair '() '())
      (let ((value (car (h-min heap)))
            (child (max (cdr vw-pair) (cdr (h-min heap))))
            (root (min (cdr vw-pair) (cdr (h-min heap)))))
        (if (= child (cdr vw-pair))
            (create-heap (cons value root)
                         (right heap)
                         (insert (cons (car vw-pair) child) (left heap)))
            (create-heap (cons (car vw-pair) root)
                         (right heap)
                         (insert (cons value child) (left heap)))))))

;; Taken from lab 9
(define (insert-list-of-pairs vw-pair-list heap)
  (if (null? vw-pair-list)
      heap
      (insert-list-of-pairs (cdr vw-pair-list) (insert (car vw-pair-list) heap))))

;; Taken from lab 9
(define (remove-min heap)
  ;; helper which removes the root and combines both the left and right heaps
  (define (new-heap heap1 heap2)
    
    (cond ((null? heap1) heap2)
          ((null? heap2) heap1)
          ((< (weight heap1) (weight heap2))
           (create-heap (h-min heap1)
                        heap2
                        (new-heap (left heap1) (right heap1))))
          (else (create-heap (h-min heap2)
                             heap1
                             (new-heap (left heap2) (right heap2))))))

  (new-heap (left heap) (right heap)))

;; Taken from lab 9
(define (get-in-order heap)
  (cond ((null? heap) heap)
        ((null? (remove-min heap)) (list (h-min heap)))
        ((= (weight heap) (weight (remove-min heap))) (cons (h-min heap) (get-in-order (remove-min heap))))
        ((< (weight heap) (weight (remove-min heap))) (cons (h-min heap) (get-in-order (remove-min heap))))
        (else (cons (get-in-order (remove-min heap)) (h-min heap)))))

(define (heapsort pair-list)
  (get-in-order (insert-list-of-pairs pair-list '())))



(define (combine-htree-pairs hp1 hp2)
  (if (null? hp1)
      hp2
      (if (null? hp2)
          hp1
          (cons (make-internal-node (car hp1) (car hp2)) (+ (cdr hp1) (cdr hp2))))))

(define hpair1 (cons (create-heap 'doo '() '()) 21))
(define hpair2 (cons (create-heap 'da '() '()) 12))

"Test"
"(combine-htree-pairs hpair1 hpair2)"
(combine-htree-pairs hpair1 hpair2)  ;; output ((internal (doo () ()) (da () ())) . 33)
(newline)

"Problem 2b"
(define (build-huffman sf-list)
  
  (define (convert-leaf list)
    (cons (create-heap (value list) '() '()) (weight list)))

  (define (huffman list)
    (cond ((null? list) list)
          ((null? (cdr list)) list)
          ((null? (cdr (cdr list))) (if (list? (value (cdr list)))
                                        (make-internal-node (value (cdr list)) (value list))
                                        (make-internal-node (value list) (create-heap (value (cdr list)) '() '()))))                                                 
          (else (huffman (heapsort (cons (combine-htree-pairs (convert-leaf list)
                                                          (convert-leaf (cdr list)))
                                     (cddr list)))))))
  
 (huffman (heapsort sf-list)))

"Test"
"(build-huffman '((steve . 5) (argon . 5) (michael . 5) (ollie . 5)))"
(build-huffman '((steve . 5) (argon . 5) (michael . 5) (ollie . 5)))  ;(internal (internal (ollie () ()) (michael () ())) (internal (steve () ()) (argon () ())))
"(build-huffman '((ron . 57) (doo . 21) (da . 12)))"
(build-huffman '((ron . 57) (doo . 21) (da . 12)))  ;(internal (internal (da () ()) (doo () ())) (ron () ()))
(newline)

"Problem 3"
(define (get-encoding-list huff)
  (define (encode-helper huff encode)
    (if (eq? (car huff) 'internal)
        (append (encode-helper (left huff) (append encode (list #\0)))
                (encode-helper (right huff) (append encode (list #\1))))
        (list (cons (car huff) (list->string encode)))))
  (encode-helper huff (list)))

"Test"
"(get-encoding-list (build-huffman '((ron . 57) (doo . 21) (da . 12))))"
(get-encoding-list (build-huffman '((ron . 57) (doo . 21) (da . 12)))) ;((da . "00") (doo . "01") (ron . "1"))
(newline)

"Problem 4"
(define (encode string list)
  (define (encoded-symbol symbol strings)
    (cond ((null? strings) null)
          ((eq? symbol (caar strings)) (cdar strings))
          (else (encoded-symbol symbol (cdr strings)))))

  (define (combine-strings stringlist)
    (if (null? stringlist)
        ""
        (string-append (car string) (combine-strings (cdr string)))))

  (let* ((tree (build-huffman list))
         (encoded-strings (get-encoding-list tree))
         (newlist (map (lambda (symbol) (encoded-symbol symbol encoded-strings))
                       (string->list string))))
    (join-strings newlist)))

"Test"
(encode "doo doo doo da da da da"
        '((da . "00") (doo . "01") (ron . "1")))


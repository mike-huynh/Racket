;; Mike Huynh  Problem Set 7  10/28/17

(define (make-tree value left right) (list value left right))
(define (value tree) (car tree))
(define (left tree) (cadr tree))
(define (right tree) (caddr tree))

"Problem 1"
;; (nvalue T) function
(define (nvalue T)
  ;; cond to check each node is eq to +, *, -, or /
  (cond ((eq? (value T) #\+) (+ (nvalue (left T)) (nvalue (right T))))
        ((eq? (value T) #\*) (* (nvalue (left T)) (nvalue (right T))))
        ((eq? (value T) #\-) (- (nvalue (left T))))
        ((eq? (value T) #\/) (/ 1 (nvalue (left T))))
        (else (value T))))

;; example function taken from problem set 7
(define example (list #\+ (list #\*
                                  (list 4 '() '())
                                  (list 5 '() '()))
                      (list #\+
                            (list #\/ (list 6 '() '()) '())
                            (list 7 '() '()))))

"Test"
"(nvalue example)"
(nvalue example)  ;; output 27 1/6
(newline)

"Problem 2a"
;; prepare function taken from problem set
(define (prepare x)
  (cond ((number? x) (number->string x))
        ((char? x) (string x))))

;; creating a map function to clean code up a bit
(define (bst-map T x)
  (if (null? T)
      T
      (make-tree (x (value T))
                 (bst-map (left T) x)
                 (bst-map (right T) x))))

;; (prefix T) function
(define (prefix T)
  ;; helper function
  (define (pre-help x)
    (if (null? x)
        ""
        (string-append (value x)
                       (pre-help (left x))
                       (pre-help (right x)))))
  (pre-help (bst-map T prepare)))

"Test"
"(prefix example)"
(prefix example)  ;; output "+*45+/67"
(newline)

"Problem 2b"
;;(postfix T) function
(define (postfix T)
  ;; helper function
  (define (post-help x)
    (if (null? x)
        ""
        (string-append (post-help (left x))
                       (post-help (right x))
                       (value x))))
  (post-help (bst-map T prepare)))

"Test"
"(postfix example)"
(postfix example)  ;; output "45*6/7++"
(newline)

"Problem 2c"
;; (infix T) function
(define (infix T)

  (cond ((eq? (value T) #\+) (string-append "(" (infix (left T)) "+"
                                            (infix (right T)) ")"))
        ((eq? (value T) #\*) (string-append "(" (infix (left T)) "*"
                                            (infix (right T)) ")"))
        ((eq? (value T) #\-) (string-append "-(" (infix (left T)) ")"))
        ((eq? (value T) #\/) (string-append "1/(" (infix (left T)) ")"))
        (else (number->string (value T)))))

"Test"
"(infix example)"
(infix example)  ;; output "((4*5)+(1/(6)+7))"
(newline)

"Problem 3a"
;; (bst-element? item bs-tree) function
(define (bst-element? item bs-tree)
  (cond ((null? bs-tree) #f)
        ;((not (string? (value bs-tree))) (bst-element? item (bst-map bs-tree prepare)))
        ((string=? item (value bs-tree)) #t)
        ((string<? item (value bs-tree)) (if (null? (left bs-tree))
                                             #f
                                             (bst-element? item (left bs-tree))))
        (else (if (null? (right bs-tree))
                  #f
                  (bst-element? item (right bs-tree))))))

;; testtree taken from lab 8
(define testtree (make-tree "hayward"
                            (make-tree "geoff"
                                       (make-tree "dog" '() '())
                                       (make-tree "guy" '() '()))
                            (make-tree "zebra" '() '())))

"Test"
"(bst-element? zebra testtree)"
(bst-element? "zebra" testtree)
(newline)

"Problem 3b"
;; insert function
(define (bst-insert item bs-tree)
  (cond ((null? bs-tree) (make-tree item '() '()))
        ((string=? item (value bs-tree)) bs-tree)
        ((string<? item (value bs-tree)) (make-tree (value bs-tree)
                                                    (bst-insert item (left bs-tree))
                                                    (right bs-tree)))
        (else (make-tree (value bs-tree)
                         (left bs-tree)
                         (bst-insert item (right bs-tree))))))

"Test"
"(bst-insert zigzag testtree)"
(bst-insert "zigzag" testtree)
(newline)

"Problem 3c"
(define (bst-smallest bs-tree)
  (cond ((null? bs-tree) 'undefined)
        ((null? (left bs-tree)) (value bs-tree))
        (else (bst-smallest (left bs-tree)))))

"Test"
"(bst-smallest testtree)"
(bst-smallest testtree)  ;; output dog
(newline)

"Problem 3d"
(define (bst-largest bs-tree)
  (cond ((null? bs-tree) 'undefined)
        ((null? (right bs-tree)) (value bs-tree))
        (else (bst-largest (right bs-tree)))))

"Test"
"(bst-largest testtree)"
(bst-largest testtree)  ;; output zebra
(newline)

"Problem 3e"
(define (bst-equal? bst1 bst2)
  (cond ((and (null? bst1) (null? bst2)) #t)
        ((string=? (value bst1) (value bst2)) (and (bst-equal? (left bst1) (left bst2))
                                                   (bst-equal? (right bst1) (right bst2))))
        (else #f)))

"Test"
"(bst-equal? testtree testtree)"
(bst-equal? testtree testtree)  ;; true
(newline)

"Problem 3f"
(define (bst-subset? bst1 bst2)
  (cond ((null? bst1) #t)
        ((null? bst2) #f)
        (else (and (bst-element? (value bst1) bst2)
                   (bst-subset? (left bst1) bst2)
                   (bst-subset? (right bst1) bst2)))))

"Test"
"(bst-subset? testtree testtree)"
(bst-subset? testtree testtree)  ;; true
(newline)

"Problem 3g"
(define (bst-set-equal? bst1 bst2)
  (and (bst-subset? bst1 bst2)
       (bst-subset? bst2 bst1)))

"Test"
"(bst-set-equal? testtree testtree)"
(bst-set-equal? testtree testtree)  ;; true
(newline)

"Problem 4a"
(define (bst-delete-min bst)
  (cond ((null? bst) bst)
        ((null? (left bst)) (right bst))
        (else (make-tree (value bst) (bst-delete-min (left bst)) (right bst)))))

"Test"
"(bst-delete-min testtree)"
(bst-delete-min testtree)  ;; removes dog which was smallest
(newline)

"Problem 4b"
(define (bst-delete-max bst)
  (cond ((null? bst) bst)
        ((null? (right bst)) (left bst))
        (else (make-tree (value bst) (left bst) (bst-delete-max (right bst))))))

"Test"
"(bst-delete-max testtree)"
(bst-delete-max testtree)  ;; removes zebra which was largest
(newline)

"Problem 4c"
(define (bst-delete val bst)
  (cond ((null? bst) bst)
        ((string<? val (value bst)) (make-tree (value bst)
                                               (bst-delete val (left bst))
                                               (right bst)))
        ((string>? val (value bst)) (make-tree (value bst)
                                               (left bst)
                                               (bst-delete val (right bst))))
        (else (cond ((and (null? (right bst)) (and (null? (left bst)))) '())
                    ((and (null? (right bst)) (not (null? (left bst)))) (left bst))
                    ((and (null? (left bst)) (not (null? (right bst)))) (right bst))
                    (else (#f)))))) ;; take the largest from the left subtree to replace the removed node
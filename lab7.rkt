;;  Mike Huynh  Lab 7  10/17/17

"Problem 1"
;; Better-equals function which checks if equal
(define (better-equal? a b)
  (if (null? a)
      #f
      (if (null? b)
          #f
          (cond ((if (symbol? a)
                     (if (symbol? b)
                         (eq? a b)
                         #f)
                     #f) #t)
        
                ((if (number? a)
                     (if (number? b)
                         (= a b)
                         #f)
                     #f) #t)
        
                ((if (list? a)
                     (if (list? b)
                         (better-equal? (car a) (car b))
                         (better-equal? (cdr a) (cdr b)))
                     #f) #t)
        
                (else #f)))))

"Test"
(better-equal? 1.0 1) ;; #t
(better-equal? '(a 1.0) '(a 1)) ;; #t
(better-equal? '() '(()))
(newline)

"Porblem 2"
;; Remove all function
(define (remove-all x l)
  (if (null? l)
      l
      (if (equal? x (car l))
          (remove-all x (cdr l))
          (cons (car l) (remove-all x (cdr l))))))

(remove-all 'a '(1 2 a (a b c) a b))
(remove-all '() '(1 2 a () (a b ()) a b))

(newline)

"Problem 3a"
(define (dot-prod x y)
  (cond ((null? x) 0)
        (else (+ (* (car x) (car y))
                 (dot-prod (cdr x) (cdr y))))))
"Test"
(dot-prod '(3 4 6 7) '(2 3 4 5))
(newline)

"Problem 3b"
(define (dot-prod-with-map x y)
  (apply + (map * x y)))
(dot-prod-with-map '(3 4 6 7) '(2 3 4 5))
(newline)

"Problem 4"
(define (element-of-set? x set)
   (cond ((null? set) #f)
         ((equal? x (car set)) #t)
         (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
  (if (element-of-set? x set)
      set
      (cons x set)))

(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((element-of-set? (car set1) set2)
         (cons (car set1)
               (intersection-set (cdr set1) set2)))
        (else (intersection-set (cdr set1) set2))))

(define (union-set set1 set2)
  (cond ((null? set1) set2)
        ((element-of-set? (car set1) set2)
         (union-set (cdr set1) set2))
        (else
         (cons (car set1) (union-set (cdr set1) set2)))))
"Test"
"(union-set )"
(newline)

"Problem 5"
(define (has-duplicates? lst) 
  (cond ((null? lst) #f) 
        ((not (not (member (car lst) (cdr lst)))) #t)
        (else (has-duplicates? (cdr lst)) )))

"Test"
"(has-duplicates? '(1 2 3 4 2)"
(has-duplicates? '(1 2 3 4 2)) ;; Output #t
(newline)

"Problem 6"
(define (num-zeroes lst)
  (define (helper x y)
    (cond ((null? x) y)
          ((list? (car x)) (+ y (helper (car x) 0)         
                                (helper (cdr x) 0)))       
          ((better-equal? (car x) 0) (helper (cdr x) (+ 1 y))) 
          (else
           (helper (cdr x) y))))
  (helper lst 0))

"Test"
"(num-zeroes '(0 0 1 (0 1 0.0)))"
(num-zeroes '(0 0 1 (0 1 0.0)))  ;; Output 4
(newline)

"Problem 7"


(define (nested-reverse lst)
  ;;append function taken from lecture slides
  (define (append list1 list2)
    (cond ((null? list1) list2)
          ((list? (car list1)) (cons (nested-reverse (car list1))
                                       list2))
          (else (append (cdr list1) (cons (car list1) list2)))))
  
  (append lst '()))

"Test"
"(nested-reverse '(1 2 3 (4 (5 6))))"
(nested-reverse '(1 2 3 (4 (5 6))))  ;; output (((6 5) 4) 3 2 1)
;; Mike Huynh  Problem Set 6  10/21/17

"Problem 1a"
;; (remove-if f elements) function
(define (remove-if f elements)
  
  (if (null? elements)                 ;; checks if list is null
      elements                         ;; returns empty list
      (if (f (car elements))           ;; checks if function is true or false
          (remove-if f (cdr elements)) ;; removes the element if true
          (cons (car elements)         ;; else creates a list of elements that were false
                (remove-if f (cdr elements))))))

"Test"
"(remove-if odd? (list 1 2 3 4 5 6 7 8 9))"
(remove-if odd? (list 1 2 3 4 5 6 7 8 9))  ;; output (2 4 6 8)
(newline)

"Problem 1b"
;; (nested-remove v elements) function
(define (nested-remove v elements)

  (if (null? elements)                         ;; checks if list is empty
      elements                                 ;; return empty list
      (if (list? (car elements))               ;; checks if first element is a list
          (if (equal? v (car elements))        ;; if true, checks if both are equal
              (nested-remove v (cdr elements)) ;; if true, remove the list and recall function
              (cons (nested-remove v (car elements))
                    (nested-remove v (cdr elements)))) ;; else, recall the function to check the nested list and generate a list
          (if (equal? v (car elements))        ;; if false, checks if both are equal
              (nested-remove v (cdr elements)) ;; if true, remove element and recall function
              (cons (car elements)
                    (nested-remove v (cdr elements))))))) ;; else create a list that don't satisy the req.

"Test"
"(nested-remove 'b '(b 2 (a b)))"
(nested-remove 'b '(b 2 (a b)))  ;; output (2 (a))
"(nested-remove '(a b) '(1 2 (a b)))"
(nested-remove '(a b) '(1 2 (a b)))  ;; output (1 2)
(newline)

"Problem 2a"
;; (explode x) function
(define (explode x)
  ;; helper function
  (define (helper x lst)
    (cond ((< x 10) (cons x lst)) ;; if the number is less than 10, print the number in a list
          (else (helper (floor (/ x 10))
                        (cons (modulo x 10) lst))))) ;; if > 10, recall helper function
                                                     ;;divide num by 10 and create a list of the modulo of the number 
    
  (helper x '()))

"Test"
"(explode 12345)"
(explode 12345) ;; output (1 2 3 4 5)
(newline)

"Problem 2b"
;; (implode l) function
(define (implode l)
  ;; helper function for base 10
  (define (acc lst x)
    (if (null? lst)               ;; checks if list is null
        x                         ;; return number if true
        (acc (cdr lst) (+ x 1)))) ;; otherwise +1 and recall helper
  (if (null? l)                   ;; checks if list is null
      0                           ;; 0 and prints out the number
      (+ (* (car l) (expt 10 (acc l -1)))
         (implode (cdr l))))) ;; multiplies the car of the list to a base 10 with the helper
                              ;;then recursively calls the function and adds the total

"Test"
"(implode '(1 2 3 4 5))"
(implode '(1 2 3 4 5)) ;; output 12345
(newline)

"Problem 2c"
;; (has-property x) function
(define (has-property x)
  
  ;; sum function
  (define (sum lst)
    (if (null? lst)
        0
        (+ (car lst) (sum (cdr lst)))))
  
  ;; reverse number function
  (define (reverse-implode num)
    (let a ((b (explode num)) (n 0)) ;; expands the number into a list
      (if (null? b)                  ;; and then reverse implode it
          0
          (+ (* (expt 10 n) (car b))
             (a (cdr b) (+ n 1))))))
          
  (= (* (sum (explode x)) (reverse-implode (sum (explode x)))) x))

"Test"
"(has-property 1458)"
(has-property 1458) ;; #t
(newline)

"Problem 2d"
;; find funciton from lab 4
(define (find sequence test n)
  (define (help s k nt)
    (if (= nt n)
        (s (- k 1))
        (if (test (s k))
            (help s (+ k 1) (+ nt 1))
            (help s (+ k 1) nt))))
  (help sequence 1 0))

"Test"
"(find abs has-property 2)"
(find abs has-property 2) ;;  output 81
"(find abs has-property 4)"
(find abs has-property 4)  ;; output 1729
(newline)

"Problem 3a"
;; (superset? a b) function
(define (superset? a b)
  ;; helper function which recursives checks if one element is in a list
  (define (member-helper? x y)
    (if (null? y)
        #t                                   
        (if (null? x)
            #f
            (if (equal? (car x) (car y))
                #t
                (member-helper? (cdr x) y)))))

  (if (null? (cdr b))
      (member-helper? a b)
      (if (member-helper? a b)
          (superset? a (cdr b))
          #f)))

"Test"
"(superset? '(1 2 3 4) '(2 1 4))"
(superset? '(1 2 3 4) '(2 1 4))
(newline)

"Problem 3b"
;; (set-difference a b) function
(define (set-difference a b)
  ;; element function from lab
  (define (element? x lst)
    (cond ((null? lst) #f)              ;; if list is null return #f
          ((eq? x (car lst)) #t)        ;; return #t if its an element
          (#t (element? x (cdr lst)))))

  (cond ((null? a) a)                                      ;; if list is empty return empty list
        ((element? (car a) b) (set-difference (cdr a) b))  ;; if an element is in the list, remove and recall
        (else (cons (car a) (set-difference (cdr a) b))))) ;; else create a new list and recall

"Test"
"(set-difference '(1 2 3 4 5) '(1 2 3))"
(set-difference '(1 2 3 4 5) '(1 2 3))
(newline)

"Problem 3c"
;; (cross-product a b) function
(define (cross-product a b)
  ;; helper function
  (define (cross-prod-help x y z)
    (cond ((null? x) x)                             ;; checks if the list is null
          ((null? y) (cross-prod-help (cdr x) z z)) ;; refreshes the list and recalls again
          (else (cons (list (car x) (car y))
                      (cross-prod-help x (cdr y) z))))) 
    (cross-prod-help a b b))

"Test"
"(cross-product '(1 2) '(4 5))"
(cross-product '(1 2) '(4 5))
(newline)

"Problem 4"
;; (nestless lst) function
(define (nestless lst)
  ;; append function taken from lecture slides
  (define (append list1 list2)
    (if (null? list1)
        list2
        (cons (car list1)
              (append (cdr list1) list2))))
  
  (cond ((null? lst) lst)                               ;; if list is empty, return empty list
        ((pair? (car lst)) (append (nestless (car lst)) ;; checks if nested list is a pair, if true append
                                 (nestless (cdr lst))))
        (else (cons (car lst) (nestless (cdr lst))))))  ;; else combines the list

"Test"
"(nestless '((a b) c (d e (f g))))"
(nestless '((a b) c (d e (f g)))) ;; output (a b c d e f g)
(newline)

"Problem 5"
;; (merge l1 l2) function
(define (merge l1 l2)
  (cond ((null? l1) (if (null? l2)
                        l2
                        (cons (car l2) (merge '() (cdr l2)))))                                   ;; if first list is empty, return empty
        ((null? l2) (cons (car l1) (merge (cdr l1) '())))                                   ;; if second list is empty, return empty
        (else (if (> (car l1) (car l2))                   ;; if the first element in first list is greater than the first in the second
                  (cons (car l2) (merge l1 (cdr l2)))     ;; create a list with the second in the front, recall function
                  (cons (car l1) (merge (cdr l1) l2)))))) ;; combine the list with the first in the front, recall function

"Test"
"(merge '(1 2 5 6 6 6 7 7 7 67) '(2 4 5))"
(merge '(1 2 5 6 6 6 7 7 7 67) '(2 4 5)) ;; output (1 2 2 4 5 5 6 6 6 7 7 7 67)
  
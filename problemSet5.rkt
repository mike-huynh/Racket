;; Mike Huynh  Problem Set 5  10/15/17

"Problem 1a"
;; Converting to polar coordinate function
(define (c->p p)
  ;; Square function
  (define (square x) (* x x))
  
  (cons (sqrt (+ (square (car p))
                 (square (cdr p))))
        (atan (/ (cdr p)
                 (car p)))))

"Test"
"(c->p (cons 3 4))"
(c->p (cons 3 4))  ;; output (5 . 0.927)
(newline)

"Problem 1b"
(define (p->c p)
  (cons (* (car p)
           (cos (cdr p)))
        (* (car p)
           (sin (cdr p)))))

"Test"
"(p->c (cons 5 0.9272952180016122))"
(p->c (cons 5 0.9272952180016122))  ;; output (3 . 4)
(newline)

"Problem 2"
(define (max-f p)
  (lambda (x)
    (if (> ((car p) x) ((cdr p) x))
        ((car p) x)
        ((cdr p) x))))
(newline)

"Problem 3"
;; Function that combines two lists into a list of pairs
(define (zip x y)
  (if (null? x)
      y
      (if (null? y)
          x
          (cons (cons (car x) (car y)) (zip (cdr x) (cdr y))))))

"Test"
"(zip '(1 2 3 4) '(1 2 3 4))"
(zip '(1 2 3 4) '(1 2 3 4))  ;; Output ((1 . 1) (2 . 2) (3 . 3) (4 . 4))
(newline)

"Problem 4"
;; Function that decompresses a list of pairs into two lists
(define (unzip x)
  ;; Helper function for list 1
  (define (car-helper a)
    (if (null? a)
        a
        (cons (car (car a)) (car-helper (cdr a)))))
  ;; Helper function for list 2
  (define (cdr-helper a)
    (if (null? a)
        a
        (cons (cdr (car a)) (cdr-helper (cdr a)))))

  (cons (car-helper x) (cdr-helper x)))

"Test"
"(unzip '((1 . 2) (2 . 4) (3 . 6) (4 . 8)))"
(unzip '((1 . 2) (2 . 4) (3 . 6) (4 . 8))) ;; Output ((1 2 3 4) 2 4 6 8)
(newline)

"Problem 5a"
;; Function that takes a pair and evaluates it using an expression
(define (encode p)
  (+ (* (/ 1 2) (- (+ (car p) (cdr p)) 2) (- (+ (car p) (cdr p)) 1)) (car p)))

"Test"
"(encode (cons 1 2))  ;; Output 2"
(encode (cons 1 2))  ;; Output 2
(newline)

"Problem 5b"
;; Function that returns the pair from encoding
(define (decode x)
  ;; Square function
  (define (square x) (* x x))
  
  (let* ((w (floor (- (sqrt (* 2 x)) (/ 1 2))))
         (t (/ (+ (square w) w) 2))
         (x (- x t)))
    (cons x (+ (- w x) 2))))

"Test"
"(decode 2)"
(decode 2)  ;; Output (1 . 2)
(newline)

"Problem 6"
;; Function which returns all positives only, exlcudes 0
(define (positives l)
  (if (null? l)
      l
      (if (> (car l) 0)
          (cons (car l) (positives (cdr l)))
          (positives (cdr l)))))

"Test"
"(positives (list -2 -1 0 1 2))"
(positives (list -2 -1 0 1 2))
(newline)

"Problem 7"
;; Removes a value from the list
(define (remove-duplicates l)

  (cond ((null? l) l)
        ((null? (cdr l)) l)
        ((equal? (car l) (car (cdr l))) (remove-duplicates (cdr l)))
        (else (cons (car l) (remove-duplicates (cdr l))))))
        
    

"Test"
"(remove-duplicates '(b b a c a a a d d)"
(remove-duplicates '(b b a c a a a d d))  ;; Output (b a c a d)
  
  
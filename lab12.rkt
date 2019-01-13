;; Mike Huynh  Lab 12  11/28/17

;; Code from lab
;; stream primitives
(define-syntax cons-stream
  (syntax-rules ()
    ((cons-stream head tail)
     (cons head (delay tail)))))

(define (stream-car x)
  (car x))

(define (stream-cdr x)
  (force (cdr x)))

(define empty-stream? null?)

;; utiity for tracing function calls 
(define (tracer name . values)
  ;  usage: if at start of function defined (foo a b c)
  ;  put in (tracer 'foo a b c)
  ;  interesting to put into something that is delayed
  (define (display-spaced item)
    (display item)
    (display " "))
  (display-spaced name)
  (display-spaced "with parameter(s):")
  (for-each display-spaced values)
  (newline))
;;;;;;;;;;;;;;;;

"Problem 1a"
(define (stream-filter p str)
  (cond ((empty-stream? str) str)
        ((p (stream-car str))
         (cons-stream (stream-car str) (stream-filter p (stream-cdr str))))
        (else (stream-filter p (stream-cdr str)))))

"Problem 1b"
(define (stream-map f str)
  (cond ((empty-stream? str) str)
        (else (cons-stream (f (stream-car str)) (stream-map f (stream-cdr str))))))

"Problem 1c"
(define (stream-nth index str)
  (define (helper x acc str)
    (cond ((empty-stream? str) str)
          ((= x acc) (stream-car str))
          (else (helper x (+ acc 1) (stream-cdr str)))))
  (helper index 1 str))

"Problem 2"
(define (str-to-list str k)
  (define (helper x y acc)
    (cond ((empty-stream? x) x)
          ((> acc y) '())
          (else (cons (stream-car x)
                      (helper (stream-cdr x) y (+ acc 1))))))
  (helper str k 1))

"Problem 3a"
(define (scale-stream k str)
  (if (empty-stream? str)
      str
      (cons-stream (* k (stream-car str))
                   (scale-stream k (stream-cdr str)))))

"Problem 3b"
(define (add-streams str1 str2)
  (cond ((empty-stream? str1) str2)
        ((empty-stream? str2) str1)
        (else (cons-stream (+ (stream-car str1) (stream-car str2))
                           (add-streams (stream-cdr str1) (stream-cdr str2))))))

"Problem 3c"
(define (mult-streams str1 str2)
  (cond ((empty-stream? str1) str2)
        ((empty-stream? str2) str1)
        (else (cons-stream (* (stream-car str1) (stream-car str2))
                           (mult-streams (stream-cdr str1) (stream-cdr str2))))))

"Problem 3d"
(define (append-streams str1 str2)
  (cond ((empty-stream? str1) str2)
        ((empty-stream? str2) str1)
        (else (cons-stream (stream-car str1)
                           (append-streams (stream-cdr str1) str2)))))

"Problem 4a"
(define (enumerate-integer-stream)
  (define (enum-helper case)
    (cons-stream case (enum-helper (+ case 1))))
    
  (enum-helper 0))

"Problem 4b"
(define (odd-factors-of k)
  (define (fact-help case k)
    (if (= (modulo case k) 0)
        (cons-stream case (fact-help (+ case 1) k))
        (fact-help (+ case 1) k)))
  (stream-filter odd? (fact-help 0 k)))

"Problem 4c"
(define (partial-sums str)
  (define (part-help str acc)
    (cond ((empty-stream? (stream-cdr str)) str)
          ((empty-stream? str) str)
          (else (cons-stream acc (part-help (stream-cdr str) (+ acc (stream-car (stream-cdr str))))))))
  (part-help str (stream-car str)))

"Problem 4d"
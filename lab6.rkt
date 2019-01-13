;; Mike Huynh  Lab 6  10/10/17

"Problem 1a"
(define (make-complex a b)
  (cons a b))
(newline)

"Problem 1b"
(define (real x)
  (car x))
(newline)

"Problem 1c"
(define (imag x)
  (cdr x))
(newline)

"Problem 2a"
(define (complex-add x y)
  (make-complex (+ (real x) (real y))
                (+ (imag x) (imag y))))
(newline)

"Problem 2b"
(define (complex-sub x y)
  (make-complex (- (real x) (real y))
                (- (imag x) (imag y))))
(newline)

"Problem 2c"
(define (complex-mult x y)
  (make-complex (- (* (real x) (real y))
                   (* (imag x) (imag y)))
                (+ (* (imag x) (real y))
                   (* (real x) (imag y)))))
(newline)

"Problem 3"
(define (complex-conj x)
  (make-complex (real x) (* -1 (imag x))))
(newline)

"Problem 4a"
(define (count-positives lst)
  (if (null? lst)
      0
      (if (> (car lst) 0)
          (+ 1 (count-positives (cdr lst)))
          (count-positives (cdr lst)))))

"Test"
"(count-positives (list 1 -23 0 -11 3 1002))"
(count-positives (list 1 -23 0 -11 3 1002))  ;; output 3
"(count-positives '())"
(count-positives '())  ;; output 0
(newline)

"Problem 4b"
(define (sum-list lst)
  (if (null? lst)
      0
      (+ (car lst) (sum-list (cdr lst)))))

"Test"
"(sum-list '(1 2 3 4 5))"
(sum-list '(1 2 3 4 5)) ;; output 15
(newline)

"Problem 4c"
(define (consecutive-ints a b)
  (if (> a b)
      '()
      (cons a (consecutive-ints (+ a 1) b))))

"Test"
"(consecutive-ints -4 6)"
(consecutive-ints -4 6)  ;; (-4 -3 -2 -1 0 1 2 3 4 5 6)
(newline)

(define (square x)
  (* x x))

(define (consecutive-squares a b)
  (if (> a b)
      '()
      (cons (square a) (consecutive-squares (+ a 1) b))))

"Test"
"(consecutive-squares 1 10)"
(consecutive-squares 1 10)  ;; (1 4 9 16 25 36 49 64 81 100)

"(consecutive-square 4 -6)"
(consecutive-squares 4 -6)
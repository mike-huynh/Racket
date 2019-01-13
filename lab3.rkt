; Mike Huynh  Lab 3  9/19/17

"Problem 1a."
(define (num-in-gen n)
  (cond ((<= n 0) 0)
        ((= n 1) 2)
        (else (* 2 (num-in-gen (- n 1))))))
"test"
"(num-in-gen 4)"
(num-in-gen 4) ; output 16
(newline)

"Problem 1b."
(define (num-ancestors n)
  (cond ((<= n 0) 0)
        ((= n 1) 2)
        (else (+ (num-in-gen n) (num-ancestors (- n 1))))))
"test"
"(num-ancestors 3)"
(num-ancestors 3) ; output 2+4+8= 14
(newline)

"Problem 2a."
(define (pell-num n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (* 2 (pell-num (- n 1))) (pell-num (- n 2))))))
"test"
"(pell-num 5)"
(pell-num 5) ; output 29
(newline)

"Problem 2b."
(define (comp-pell-num n)
  (cond ((= n 0) 2)
        ((= n 1) 2)
        (else (+ (* 2 (comp-pell-num (- n 1))) (comp-pell-num (- n 2))))))
"test"
"(comp-pell-num 4)"
(comp-pell-num 4) ; output 34
(newline)

"Problem 2c."
(define (sqrt-2-approx n)
  (/ (/ (comp-pell-num n) 2) (pell-num n)))
"test"
"(sqrt-2-approx 6)"
(sqrt-2-approx 6) ; output 99/70

"Problem 3a."
(define (square x)
  (* x x))
(define (fastexp base exp)
  (cond ((= exp 0) 1)
        ((even? exp) (square (fastexp base (/ exp 2))))
        ((odd? exp) (* base (square (fastexp base (/ (- exp 1) 2)))))))
"test"
"(fastexp 5 4)"
(fastexp 5 4)

"Problem 3b."
"Let's say we are trying to find out 2e10. In the power function, it would recurse 10 times. In the fastexp function, it would recurse about 4 times, which is much faster than the power function."
(newline)

"Problem 4a."
(define (cont-frac k x)
  (if (= k 0)
      0
      (/ (- x 1) (+ 2 (cont-frac (- k 1) x)))))
"test"
"(cont-frac 4 4)"
(cont-frac 100 64) ; output 60/61
(newline)

"Problem 4b."
(define (new-sqrt x n)
  (+ 1 (cont-frac n x)))
"test"
"(new-sqrt 9 100)"
(new-sqrt 9 100)
"As you can see, the new-sqrt 9 100 is very close to 3. As you increase n, the closer it gets to the sqrt of x and in this case it is very close to the square root of 9, which is 3."
  
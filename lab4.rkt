;Mike Huynh  Lab 4  9/26/17

"Problem 1"
(define (harmonic-iter n)
  (define (harmonic-i n a)
    (if (= n 0)
        a
        (harmonic-i (- n 1) (+ a (/ 1 n)))))
  (harmonic-i n 0))

"test"
"(harmonic-iter 3)"
(harmonic-iter 3) ; output 1 5/6
(newline)

"Problem 2"
(define (harm-term k)
  (/ 1 k))

(define (harm-sum n)
  (define (sum f n)
    (if (= n 1)
        (f 1)
        (+ (f n) (sum f (- n 1)))))  
  (sum harm-term n))

"Test"
"(harm-sum 3)" 
(harm-sum 3) ; output 1 5/6
"(harm-sum 4)"
(harm-sum 4) ; output 2 1/12
(newline)

"Problem 3a."
(define (g-sum f a b)
  (cond ((> a b) 0)
        ((= a b) (f b))
        (else (+ (f b) (g-sum f a (- b 1))))))

"Problem 3b."
"Test"
"(g-sum harm-term 1 5)"
(g-sum harm-term 1 5)
(newline)

"Problem 3c."
(define (geometric-term k)
  (cond ((= k 0) 1)
        (else (/ 1 (expt 2 k)))))
(define (geom-series-np2 n)
  (g-sum geometric-term 0 n))

"Test"
"(geom-series-np2 4)"
(geom-series-np2 4)
(newline)

"Problem 3d."
(define (convergent-series a b)
  (if (> a b)
      0
      (g-sum (lambda (k) (/ 1 (expt k 2))) a b)))

"Test"
"(convergent-series 1 5)"
(convergent-series 1 5)
(newline)

"Problem 4a."
(define (find sequence test n)
  (define (help s k nt)
    (if (= nt n)
        (s (- k 1))
        (if (test (s k))
            (help s (+ k 1) (+ nt 1))
            (help s (+ k 1) nt))))
  (help sequence 1 0))

"Problem 4b."
"Test"
"(find abs even? 15)"
(find abs even? 15) ; 30
"(find abs even? 15)"
(find abs odd? 15) ;;29
(newline)

"Problem 4c."
(define (nth-fib-prime n)
  
  ;fibonacci function
  (define (fibonacci n)
    (cond ((= n 1) 1)
          ((= n 2) 1)
          (else (+ (fibonacci (- n 1)) (fibonacci (- n 2))))))

  ; prime function
  (define (prime? n)
    (define (divisor? k) (= 0 ( modulo n k )))
    (define ( divisors-upto k )
      (and (> k 1)
      (or (divisor? k) (divisors-upto (- k 1)))))
    (if (= n 1)
        #f
        (not ( divisors-upto (- n 1)))))

  (find fibonacci prime? n))

"Test"
"(nth-fib-prime 6)"
(nth-fib-prime 6) ; output 233
(newline)

"Problem 5a."
(define (comp f g)
  (lambda (x)
    (f (g x))))

(define (double x) (* 2 x ))
(define (add-one x) (+ 1 x))
(define com (comp add-one double))

"Test"
"(com 3)"
(com 3) ; 7
"((comp double add-one) 3)"
((comp double add-one) 3) ; 8
(newline)

"Problem 5b."
(define (pos-cos x)
  (if (>= (cosh x) 0)
      (cosh x)
      (* -1 (cosh x))))
(newline)

"Problem 5c."
(define (square x) (* x x))
"Test"
"((comp square sqrt) 5)"
((comp square sqrt) 5)
"((comp sqrt sqaure) 5)"
((comp sqrt square) 5)
"It returns the parameter that you have given. Order of the function does not matter."
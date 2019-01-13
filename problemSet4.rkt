; Mike Huynh  Problem Set 4  9/29/17

"Problem 1a"
;; Harmonic function
(define (harmonic n)
  ;; Sum function
  (define (sum term a next b)
    (if (> a b)
        0
        (+ (term a)
           (sum term (next a) next b))))
  (sum (lambda (x) (/ 1 x))
       1
       (lambda (y) (+ y 1))
       n))

"Test"
"(harmonic 5)"
(harmonic 5) ;; Output 2 17/60
(newline)

"Problem 1b"
;; sum function iterative ver.
(define (sum-i term a next b)
  ;; helper function
  (define (iter a b c)
    (if (> a b)
        c
        (iter (next a) b (+ (term a) c))))
  (iter a b 0))
;; Harmonic function iterative version
(define (harmonic-i n)
  (sum-i (lambda (x) (/ 1 x))
         1
         (lambda (y) (+ y 1))
         n))

"Test"
"(harmonic-i 5)"
(harmonic-i 5) ;; output 2 17/60
(newline)

"Problem 1c"
"(harmonic 1) and (harmonic-i 1)"
(harmonic 1) ;; output 1
(harmonic-i 1)
(newline)
"(harmonic 50) and (harmonic-i 50)"
(harmonic 50) ; output 4 1547/3099
(harmonic-i 50)
(newline)
"(harmonic 100) and (harmonic-i 100)" ;; output 5 5256/2789
(harmonic 100)
(harmonic-i 100)
(newline)

"Problem 2a"
;; product function recursion
(define (product term a next b)
  (if (> a b)
      1
      (* (term a)
         (product term (next a) next b))))

"Test"
"(product (lambda (x) x) 1 (lambda (y) (+ 1 y)) 5)"
(product (lambda (x) x) 1 (lambda (y) (+ 1 y)) 5) ;; output 120
(newline)

"Problem 2b"
;; product function iterative
(define (product-i term a next b)
  ;;helper function
  (define (iter a b c)
    (if (> a b)
        c
        (iter (next a) b (* (term a) c))))
  (iter a b 1))

"Test"
"(product-i (lambda (x) (* x 1)) 1 (lambda (y) (+ y 1)) 5)"
(product-i (lambda (x) (* x 1)) 1 (lambda (y) (+ y 1)) 5) ;; output 120
(newline)

"Problem 2c"
;; pi approximation using product function
(define (pi-approx n)
  (define (f n)
    (if (= (modulo n 2) 0)
        (+ n 2)
        (+ n 1)))
  (define (g n)
    (if (= (modulo n 2) 0)
        (+ n 1)
        (+ n 2)))
  (* 4.0 (/ (product-i f 1 (lambda (y) (+ y 1)) n)
     (product-i g 1 (lambda (y) (+ y 1)) n))))
"Test"
"(pi-approx 5)"
(pi-approx 5) ;; output 2.9257
(newline)

"Problem 2d"
"(pi-approx 1)"
(pi-approx 1) ;; output 2/3 or 2.6666
"(pi-approx 100)"
(pi-approx 100) ;; output 3.1570
"(pi-approx 1000)"
(pi-approx 1000) ;; output 3.1431
(newline)

"Problem 3a"
;;cont-frac function
(define (cont-frac n d k)
  ;; helper function to iterate through
  (define (help-i a b)
    (if (= a 0)
        b
        (help-i (- a 1)
                (/ (n a) (+ (d a) b)))))
  (help-i k 0))

"Test"
"(cont-frac (lambda (i) 1.0) (lambda (i) 1.0) 5)"
(cont-frac (lambda (i) 1.0) (lambda (i) 1.0) 5) ; output 0.625
(newline)

"Problem 3b"
;; approximate e using cont-frac function
(define (e-approx k)
  (+ (cont-frac (lambda (i) 1)
             (lambda (i)
               (if (= (remainder i 3) 2)
                   (/ (+ i 1) 1.5)
                   1))
             k)
     2))

"Test"
"(e-approx 10)"
(e-approx 10) ;; output 2.7183
(newline)

"Problem 3c"
;; define square function
(define (square x) (* x x))
;; define pi
(define pi 3.13159)

;; tan-cf function
(define (tan-cf x k)
  ;; defines function n to use in (cont-frac n d k)
  (define (n k)
    (if (= k 1)
        x
        (- (square x))))
  ;; defines function d to use in (cont-frac n d k)
  (define (d k)
    (- (* 2 k) 1))
  ;; calls cont-frac function
  (cont-frac n d k))

"Test"
"(tan-cf (/ pi 4) 5)"
(tan-cf (/ pi 4) 5) ;; output .9950 which is close to 1
(newline)

"Problem 4a"
;; derivative function
(define (der f h)
  ;; helper function
  (define (g x)
    (/ (- (f (+ x h)) (f x)) h))
  g)

"Problem 4b"
;; create function to display sin
(define my-sin (der sin 0.5))

"(my-sin 0)"
"(cos 0)"
(my-sin 0)
(cos 0)  ;; output ~1
(newline)

"(my-sin (/ pi 2))"
"(cos (/ pi 2)"
(my-sin (/ pi 2))
(cos (/ pi 2)) ;; output ~0
(newline)

"(my-sin pi)"
"(cos pi)"
(my-sin pi)
(cos pi) ;; output ~-1
(newline)

"(my-sin (/ (* 3 pi) 4))"
"(cos (/ (* 3 pi) 4))"
(my-sin (/ (* 3 pi) 4))
(cos (/ (* 3 pi) 4)) ;; output ~2sqrt2
(newline)

"Problem 4c"
;; fun function
(define (fun x)
  (+ (- (* 3 (square x)) (* 2 x)) 7))
;; derivative of fun function
(define func (der fun .001))
;; fun1 function is 6x-2
(define (fun1 x)
  (- (* 6 x) 2))

"Test"
"(func 5)"
"fun1 5)"
(func 5)
(fun1 5) ;; output 28
(newline)

"(func 10)"
"(fun1 10)"
(func 10)
(fun1 10) ;; output 58
(newline)

"Problem 5"
;; compose function from lab 4
(define (comp f g)
  (lambda (x)
    (f (g x))))
;; repeated function
(define (repeated f n)
  (if (= n 1)
      f
      (comp f (repeated f (- n 1)))))

"Test"
"((repeated square 2) 5)"
((repeated square 2) 5) ;; output 625

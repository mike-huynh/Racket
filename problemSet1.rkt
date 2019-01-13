; Mike Huynh   9/7/17   Problem Set #1
"Problem 1a."
"(* (+ 22 42) (* 54 99))"
(* (+ 22 42) (* 54 99)) ; output 342144
(newline)

"Problem 1b."
"(* (* (+ 22 42) 54) 99)"
(* (* (+ 22 42) 54) 99) ; output 342144
(newline)

"Problem 1c."
"(+ (* 64 102) (* 16 (/ 44 22)))"
(+ (* 64 102) (* 16 (/ 44 22))) ; output 6560
(newline)

"Problem 1d."
(define (square x)
  (* x x))
"(= (+ (/ (+ 12 144 20 (* 3 (sqrt 4))) 7) (* 5 11)) (+ (square 9) 0))"
(= (+ (/ (+ 12 144 20 (* 3 (sqrt 4))) 7) (* 5 11)) (+ (square 9) 0)) ; output #t
(newline)

"Problem 2a."
"For the first two expressions in the first problem, both are equal but the way they are ordered is different. As you can see in the 2nd expression, the operands went first before the parameters while the first one is 'messy'."
(newline)

"Problem 2b."
"They are necessary. Like the order of operations, Scheme needs to know which parameters are getting added, subtracted, etc. If you were to write an expression for 3 + 4 * 5 in Scheme, parentheses matter as it can give two completely different answers."
(newline)

"Problem 3a."
(define (cube x)
  (* x x x))
"tests"
"(cube 2)"
(cube 2) ; output 8
"(cube 3)"
(cube 3) ; output 27
(newline)

"Problem 3b."
(define (q x)
  (+ (- (+ (* (square x) (cube x)) (* 11 (square (square x))) (* 24 (cube x))) x) 21))
(define (p x)
  (cube (q x)))
"tests"
"(q 3)" ; output 1800
(q 3)
"(p 3)" ; output 5832000000
(p 3)
(newline)

"Problem 3c."
(define (tenth x)
  (* (cube (cube x)) x))
"tests"
"(tenth 10)" ; output 10,000,000,000
(tenth 10)
"(tenth 2)"
(tenth 2) ; output 1024
(newline)

"Problem 3d."
(define (hundredth x)
  (tenth (tenth x)))
"tests"
"(hundredth 2)" ; outputs 2^100
(hundredth 2)
(newline)

"Problem 3e."
"There are alot of ways to check if a parameter to the hundredth power is the correct number. One way is to multiply that parameter by itself 100 times. But since the function tenth is base 10, and the hundredth function is just the parameter base 10 to the 10th power, you can assume it is to the 100th power."
(newline)

"Problem 3f."
"If you were to multiply the parameter by itself 100 times, it would make the code look extremely messy and prone for error."
(newline)

"Problem 4a."
(define (y-value x b m)
  (+ (* m x) b))
"tests"
"(y-value 1 5 5)"
(y-value 1 5 5) ; output 10
(newline)

"Problem 4b."
(define (points-slope x1 y1 x2 y2)
  (/ (- y2 y1) (- x2 x1)))
"tests"
"(points-slope 2 10 3 15)"
(points-slope 2 10 3 15) ;output 5
(newline)

"Problem 4c."
(define (points-intercept x1 y1 x2 y2)
  (- y1 (* x1 (points-slope x1 y1 x2 y2))))
"tests"
"(points-intercept 3 10 4 15)"
(points-intercept 3 10 4 15) ; output -5
"(points-intercept 2 20 4 40)"
(points-intercept 2 30 4 50) ; output 10
(newline)

"Problem 4d."
(define (on-parallels? x1 y1 x2 y2 x3 y3 x4 y4)
  (cond ((= (- x2 x1) (- x4 x3)) #t)
        ((= (- x2 x1) 0) #f)
        ((= (- x4 x3) 0) #f)
        ((= (points-slope x1 y1 x2 y2) (points-slope x3 y3 x4 y4)) #t)))
"tests"
"(on-parallels? 1 -3.5 100 -3.5 10 0 1000 0)"
(on-parallels? 1 -3.5 100 -3.5 10 0 1000 0) ; output #t
(newline)

"Problem 5a."
(define (root1 a b c)
  (/ (+ (* -1 b) (sqrt (- (square b) (* 4 a c)))) (* 2 a)))
"tests"
"(root1 2 -5 -3)"
(root1 2 -5 -3) ; output 3
(newline)

"Problem 5b."
(define (root2 a b c)
  (/ (- (* -1 b) (sqrt (- (square b) (* 4 a c)))) (* 2 a)))
"tests"
"(root2 2 -5 -3)"
(root2 2 -5 -3) ; output -1/2
(newline)

"Problem 5c."
(define (number-of-roots a b c)
  (cond ((= (- (square b) (* 4 a c)) 0) 1)
        ((> (- (square b) (* 4 a c)) 0) 2)
        ((< (- (square b) (* 4 a c)) 0) 2)))
"tests"
"(number-of-roots 1 -5 3)"
(number-of-roots 1 -5 3) ; outputs 2
(newline)

"Problem 5d."
(define (real-roots? a b c)
  (>= (- (square b) (* 4 a c)) 0))
(real-roots? 1 -5 3)
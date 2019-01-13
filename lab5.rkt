;; Mike Huynh  Lab 5  10/3/17

"Problem 1a"
(define (USD-to-Bitcoin x)
  (/ x 4407.25))

(USD-to-Bitcoin 4470)
(newline)

"Problem 1b"
(define (energy-from-mass m)
  (let ((c 299792458))
    (* m (expt c 2))))

(energy-from-mass 1)
(newline)

"Problem 1c"
(define (area-triangle b h)
  (/ (* b h) 2))

(area-triangle 3 4)
(newline)

"Problem 2a"
(define (repeat n)
  (cond ((<= n 0) 0)
        ((= n 1) 0.1)
        (else (+ (/ 1 (expt 10 n)) (repeat (- n 1))))))

(repeat 5)
(newline)

"Problem 2b"
(define (compute-1-22 n)
  (cond ((<= n 1) 0)
        ((even? n) (+ 0.0 (* 4 (/ 1 (expt 10 n))) (compute-1-22 (- n 1))))
        (else (+ 0.0 (* 5 (/ 1 (expt 10 n))) (compute-1-22 (- n 1))))))

(compute-1-22 5)
(newline)

"Problem 2c"
(define (dominant f g)
  (if (> (f 1) (g 1))
      1
      2))

((dominant (lambda (x) (* 3 x)) (lambda (y) (+ 4 x))) 1) 